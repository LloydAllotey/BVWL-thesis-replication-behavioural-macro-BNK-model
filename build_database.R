#######################################################################
# Thesis replication: build_database.R
#
# This script builds the database data/thesis.duckdb from the raw downloads in
# data/raw/. Every raw table is stored unchanged and every cleaning step is saved
# as its own table, so the reader can trace how the estimation panel was built.
# The finished database ships with the folder. This script only needs to run
# again if a raw file is replaced.
#
# What this script does:
#   Part I    sets paths, the estimation window and the table names
#   Part II   reads the headline HICP block of every raw SPF file      -> raw_spf_hicp
#   Part III  keeps the rolling one-year-ahead point forecasts          -> spf_panel
#   Part IV   keeps the five-year-ahead (longest horizon) forecasts     -> spf_long_term
#   Part V    reads the 5y5y inflation-linked swap rate                 -> raw_ils_5y5y, ils_monthly
#   Part VI   reads realised HICP inflation                             -> raw_hicp_yoy, hicp_monthly
#   Part VII  reads the ECB deposit facility rate                       -> raw_ecb_dfr, dfr_changes
#   Part VIII builds one row per survey round with the regime labels    -> rounds
#   Part IX   builds the perceived anchor of the thesis (Equation 3.2)  -> anchor_series
#   Part X    joins everything to one row per forecast                  -> estimation_panel
#   Part XI   writes all tables to the database and prints a summary
#
# Input:  data/raw/spf/<round>.csv   ECB Survey of Professional Forecasters, one file per round
#         data/raw/ils_5y5y/*.xlsx   Bloomberg FWISEU55 Index, monthly, last price
#         data/raw/hicp/*.xlsx       Bloomberg ECCPEMUY Index (euro area HICP, year on year), monthly
#         data/raw/ecb_dfr/*.csv     ECB, key ECB interest rates, one row per change (see SOURCE.txt)
# Output: data/thesis.duckdb
# Running time: under a minute.
#
# Lloyd Allotey, St. Gallen, November 2026
#######################################################################


# Part I: Set up ----------------------------------------------------------

# Load packages
library(dplyr)    # filter, mutate, group_by, summarise, joins
library(readxl)   # read_excel for the Bloomberg exports
library(DBI)      # dbConnect, dbWriteTable
library(duckdb)   # the database engine

# Fixed paths. Run this script from the folder it sits in (open the .Rproj first).
SPF_FOLDER <- "data/raw/spf"
ILS_FILE   <- list.files("data/raw/ils_5y5y", pattern = "\\.xlsx$", full.names = TRUE)[1]
HICP_FILE  <- list.files("data/raw/hicp",     pattern = "\\.xlsx$", full.names = TRUE)[1]
DFR_FILE   <- list.files("data/raw/ecb_dfr",  pattern = "\\.csv$",  full.names = TRUE)[1]
DB_FILE    <- "data/thesis.duckdb"

# Columns of the key interest rates file: the date a change took effect and the deposit facility rate
DFR_DATE_COLUMN  <- "date"
DFR_VALUE_COLUMN <- "deposit_facility"

# The estimation window of the thesis (Table 3.4) and the baseline of the perceived anchor
# (Section 3.2). main.R can change the anchor's window again; this is the thesis version.
FIRST_ROUND <- "2004Q3"   # first round with a 5y5y ILS quote
LAST_ROUND  <- "2026Q2"   # last round before the cut off date, 30 June 2026
TARGET      <- 2          # the ECB's announced inflation target, in %
BENCH_FROM  <- "2004Q3"   # normalisation window of the swap rate (Equation 3.2), 40 rounds
BENCH_TO    <- "2014Q2"

# Small helpers used below
round_year    <- function(round) as.integer(substr(round, 1, 4))
round_quarter <- function(round) as.integer(substr(round, 6, 6))
year_of       <- function(date)  as.integer(format(date, "%Y"))
month_of      <- function(date)  as.integer(format(date, "%m"))

if (is.na(ILS_FILE) | is.na(HICP_FILE) | is.na(DFR_FILE)) {
  stop("A raw file is missing. Check data/raw/ils_5y5y, data/raw/hicp and data/raw/ecb_dfr.")
}


# Part II: The raw SPF files ----------------------------------------------

# Each raw file stacks several surveys in one sheet: headline HICP first, then core
# inflation, GDP growth, unemployment and the assumptions. The blocks reuse the same
# target labels, so only the lines of the first block (up to the core inflation title)
# are read. Everything is kept as text here; nothing is cleaned yet.
read_hicp_block <- function(file) {
  lines <- readLines(file, warn = FALSE)
  end   <- grep("^CORE INFLATION|^GROWTH EXPECTATIONS", lines)[1] - 1
  block <- read.csv(text = paste(lines[2:end], collapse = "\n"),
                    colClasses = "character", check.names = FALSE)
  data.frame(round         = sub("\\.csv$", "", basename(file)),
             target_period = block$TARGET_PERIOD,
             forecaster    = block$FCT_SOURCE,
             point         = block$POINT,
             stringsAsFactors = FALSE)
}

spf_files    <- list.files(SPF_FOLDER, pattern = "^[0-9]{4}Q[1-4]\\.csv$", full.names = TRUE)
raw_spf_hicp <- bind_rows(lapply(spf_files, read_hicp_block)) |>
  filter(target_period != "")          # lines of commas only, between the blocks

#CHECK: 111 files, 1999Q1 to 2026Q3
length(spf_files); range(raw_spf_hicp$round)


# Part III: The rolling one-year-ahead panel ------------------------------

# The rolling horizon of a round is the month eleven months after the survey month
# (Table 3.1): a Q1 round asks about December of the same year, Q2 about March,
# Q3 about June and Q4 about September of the next year.
one_year_target <- function(round) {
  year <- round_year(round)
  case_when(round_quarter(round) == 1 ~ paste0(year,     "Dec"),
            round_quarter(round) == 2 ~ paste0(year + 1, "Mar"),
            round_quarter(round) == 3 ~ paste0(year + 1, "Jun"),
            round_quarter(round) == 4 ~ paste0(year + 1, "Sep"))
}

# "2021Mar" -> 2021-03-01, and "2021Q2" -> 2021-04-01, the first day of the survey month
label_to_date  <- function(label) as.Date(paste(substr(label, 1, 4), match(substr(label, 5, 7), month.abb), "01", sep = "-"))
survey_date_of <- function(round) as.Date(paste(round_year(round), 3 * round_quarter(round) - 2, "01", sep = "-"))

spf_panel <- raw_spf_hicp |>
  filter(target_period == one_year_target(round),    # the rolling horizon only
         point != "") |>                              # density-only answers have no point forecast
  transmute(round,
            survey_date = survey_date_of(round),
            forecaster  = as.integer(forecaster),
            target_period,
            target_date = label_to_date(target_period),
            forecast    = as.numeric(point)) |>
  arrange(survey_date, forecaster)

#CHECK: 5'355 point forecasts, 112 forecasters, 110 rounds up to 2026Q2 (Section 3.1.2)
spf_panel |> filter(round <= LAST_ROUND) |>
  summarise(forecasts = n(), forecasters = n_distinct(forecaster), rounds = n_distinct(round))


# Part IV: The five-year-ahead forecasts ----------------------------------

# The long-term question is the latest calendar year in the HICP block, four years ahead in
# Q1 and Q2 rounds and five years ahead in Q3 and Q4 rounds. Six early rounds asked for at
# most two years ahead; they have no long-term question and are left out.
spf_long_term <- raw_spf_hicp |>
  filter(grepl("^[0-9]{4}$", target_period)) |>      # calendar-year labels only
  mutate(target_year = as.integer(target_period)) |>
  group_by(round) |>
  filter(target_year == max(target_year)) |>          # the longest horizon of the round
  ungroup() |>
  filter(point != "") |>
  transmute(round,
            survey_date   = survey_date_of(round),
            forecaster    = as.integer(forecaster),
            target_year,
            horizon_years = target_year - round_year(round),
            forecast      = as.numeric(point)) |>
  filter(horizon_years >= 4) |>
  arrange(survey_date, forecaster)

#CHECK: 4'639 forecasts, 112 forecasters, 104 rounds up to 2026Q2 (Appendix A)
spf_long_term |> filter(round <= LAST_ROUND) |>
  summarise(forecasts = n(), forecasters = n_distinct(forecaster), rounds = n_distinct(round))


# Part V: The 5y5y inflation-linked swap rate -----------------------------

# Bloomberg export: five lines of metadata, then Date, PX_LAST and PX_BID. One quote per
# month, stamped on the last trading day. A round takes the quote of its survey month.
raw_ils_5y5y <- read_excel(ILS_FILE, skip = 5) |>
  transmute(date = as.Date(Date), px_last = PX_LAST, px_bid = PX_BID)

ils_monthly <- raw_ils_5y5y |>
  filter(!is.na(px_last)) |>
  mutate(year = year_of(date), month = month_of(date)) |>
  group_by(year, month) |>
  summarise(ils = last(px_last), .groups = "drop")   # the last quote if a month holds several

#CHECK: June 2004 to June 2026, 265 months
range(raw_ils_5y5y$date); nrow(ils_monthly)


# Part VI: Realised HICP inflation ----------------------------------------

# Bloomberg export of the euro area HICP, year-on-year change in %, monthly. A forecast is
# scored against the value of its target month, never against the print of the survey month.
raw_hicp_yoy <- read_excel(HICP_FILE, skip = 5) |>
  transmute(date = as.Date(Date), px_last = PX_LAST)

hicp_monthly <- raw_hicp_yoy |>
  filter(!is.na(px_last)) |>
  mutate(year = year_of(date), month = month_of(date)) |>
  group_by(year, month) |>
  summarise(hicp = last(px_last), .groups = "drop")

#CHECK: December 1996 to June 2026
range(raw_hicp_yoy$date)


# Part VII: The ECB deposit facility rate ---------------------------------

# The ECB's table of key interest rates (deposit facility, main refinancing operations,
# marginal lending facility), one row per change with the date it took effect. Only the date
# and the deposit facility rate are used (column names in Part I); the raw table keeps every
# column. Rows where only another rate changed are harmless: the level in force on a day is
# still the one of the last row on or before that day.
raw_ecb_dfr <- read.csv(DFR_FILE, stringsAsFactors = FALSE, check.names = FALSE)
if (!all(c(DFR_DATE_COLUMN, DFR_VALUE_COLUMN) %in% names(raw_ecb_dfr))) {
  stop("The deposit facility rate file has no columns ", DFR_DATE_COLUMN, " and ", DFR_VALUE_COLUMN,
       ". Its columns are: ", paste(names(raw_ecb_dfr), collapse = ", "))
}

dfr_changes <- data.frame(date = as.Date(raw_ecb_dfr[[DFR_DATE_COLUMN]]),
                          rate = as.numeric(raw_ecb_dfr[[DFR_VALUE_COLUMN]])) |>
  filter(!is.na(date), !is.na(rate)) |>
  arrange(date)

# The rate in force on a given day is the level of the last change on or before that day
dfr_on <- function(days) {
  sapply(days, function(day) tail(dfr_changes$rate[dfr_changes$date <= day], 1))
}

#CHECK: 69 rows, 1 January 1999 to 16 September 2026; negative from 11 June 2014 to 26 July 2022
nrow(dfr_changes); dfr_changes |> filter(rate < 0)


# Part VIII: One row per survey round -------------------------------------

# The estimation window: survey mean and dispersion, the swap rate of the survey month, the
# realised inflation of the target month and the deposit facility rate on the first day of
# the survey month (Appendix A, Table A.3).
rounds <- spf_panel |>
  filter(round >= FIRST_ROUND, round <= LAST_ROUND) |>
  group_by(round, survey_date, target_date) |>
  summarise(n_forecasts = n(),
            spf_mean    = mean(forecast),
            spf_sd      = sd(forecast), .groups = "drop") |>
  mutate(survey_year = year_of(survey_date), survey_month = month_of(survey_date),
         target_year = year_of(target_date), target_month = month_of(target_date)) |>
  left_join(ils_monthly,  by = c("survey_year" = "year", "survey_month" = "month")) |>
  left_join(hicp_monthly, by = c("target_year" = "year", "target_month" = "month")) |>
  mutate(dfr = dfr_on(survey_date)) |>
  select(-survey_year, -survey_month, -target_year, -target_month)

# Regimes (Table 3.4): a round belongs to the ELB regime if the deposit facility rate was
# negative on every day of its quarter. Rounds before the first ELB round are "pre ELB",
# rounds after the last one "post ELB".
negative_all_quarter <- function(survey_date) {
  days <- seq(survey_date, by = "day", length.out = 92)
  days <- days[format(days, "%m") %in% format(seq(survey_date, by = "month", length.out = 3), "%m")]
  all(dfr_on(days) < 0)
}
rounds <- rounds |>
  mutate(elb    = sapply(survey_date, negative_all_quarter),
         regime = case_when(elb                               ~ "ELB",
                            survey_date < min(survey_date[elb]) ~ "pre ELB",
                            TRUE                                 ~ "post ELB")) |>
  select(-elb)

# The survey anchor of Model S: mean five-year-ahead forecast of the round (Section 6.1)
rounds <- rounds |>
  left_join(spf_long_term |>
              group_by(round) |>
              summarise(lt_mean = mean(forecast), lt_target_year = first(target_year), .groups = "drop"),
            by = "round")

#CHECK: 88 rounds, 40 pre ELB / 32 ELB / 16 post ELB, 4'173 forecasts, 3 rounds without realised HICP
nrow(rounds); table(rounds$regime); sum(rounds$n_forecasts); sum(is.na(rounds$hicp))


# Part IX: The perceived anchor (Equation 3.2) ----------------------------

# q_t = ILS_t - (mean ILS over the benchmark window - target). The constant difference is read
# as the premium of the swap rate. The gap q_t - target is the credibility gap (Equation 3.3).
premium <- mean(rounds$ils[rounds$round >= BENCH_FROM & rounds$round <= BENCH_TO]) - TARGET

anchor_series <- rounds |>
  transmute(round, survey_date, ils,
            premium = premium,
            anchor  = ils - premium,
            gap     = ils - premium - TARGET)

#CHECK: premium 0.3452 (mean swap rate 2.3452), anchor 2.00 / 1.24 / 1.90 by regime
round(premium, 4)
anchor_series |> left_join(rounds |> select(round, regime), by = "round") |>
  group_by(regime) |> summarise(anchor = round(mean(anchor), 2), rounds = n())


# Part X: One row per forecast --------------------------------------------

estimation_panel <- spf_panel |>
  filter(round >= FIRST_ROUND, round <= LAST_ROUND) |>
  left_join(rounds |> select(round, regime, ils, hicp, dfr, spf_mean, lt_mean), by = "round") |>
  left_join(anchor_series |> select(round, anchor, gap), by = "round")

#CHECK: 4'173 forecasts, 102 forecasters, 88 rounds (Table 3.4)
estimation_panel |> summarise(forecasts = n(), forecasters = n_distinct(forecaster), rounds = n_distinct(round))


# Part XI: Write the database ---------------------------------------------

table_descriptions <- data.frame(
  table = c("raw_spf_hicp", "spf_panel", "spf_long_term", "raw_ils_5y5y", "ils_monthly",
            "raw_hicp_yoy", "hicp_monthly", "raw_ecb_dfr", "dfr_changes", "rounds",
            "anchor_series", "estimation_panel"),
  description = c(
    "Headline HICP block of every raw SPF file, as text (round, target period, forecaster, point)",
    "Rolling one-year-ahead point forecasts, all rounds (Section 3.1)",
    "Five-year-ahead point forecasts, the longest horizon of each round (Section 6.1)",
    "Bloomberg FWISEU55 Index as exported, monthly last price",
    "5y5y ILS rate per calendar month",
    "Bloomberg ECCPEMUY Index as exported, euro area HICP year on year, monthly",
    "Realised HICP inflation per calendar month",
    "Key ECB interest rates as published on the ECB website, one row per change (data/raw/ecb_dfr/SOURCE.txt)",
    "Deposit facility rate, date of change and level in force from that date",
    "One row per round of the estimation window: SPF mean and SD, ILS, realised HICP, DFR, regime, survey anchor",
    "Perceived anchor of the thesis (Equation 3.2) with the premium and the credibility gap",
    "One row per forecast of the estimation window, joined with the round-level series"),
  stringsAsFactors = FALSE)

# The tables are written to a new file first. The database that ships with the folder is
# replaced only after every table has been written and read back with the expected number
# of rows, so a build that stops halfway leaves the old database untouched.
tables <- list(table_descriptions = table_descriptions,
               raw_spf_hicp       = raw_spf_hicp,
               spf_panel          = as.data.frame(spf_panel),
               spf_long_term      = as.data.frame(spf_long_term),
               raw_ils_5y5y       = as.data.frame(raw_ils_5y5y),
               ils_monthly        = as.data.frame(ils_monthly),
               raw_hicp_yoy       = as.data.frame(raw_hicp_yoy),
               hicp_monthly       = as.data.frame(hicp_monthly),
               raw_ecb_dfr        = raw_ecb_dfr,
               dfr_changes        = as.data.frame(dfr_changes),
               rounds             = as.data.frame(rounds),
               anchor_series      = as.data.frame(anchor_series),
               estimation_panel   = as.data.frame(estimation_panel))

NEW_FILE <- paste0(DB_FILE, ".new")
if (file.exists(NEW_FILE)) file.remove(NEW_FILE)
con <- dbConnect(duckdb(shared_home = FALSE), dbdir = NEW_FILE)   # no ~/.duckdb folder, no question
for (name in names(tables)) dbWriteTable(con, name, tables[[name]])

# Read back: every table with its number of rows, which must equal the rows written
cat("\nTables written:\n")
for (name in names(tables)) {
  n <- dbGetQuery(con, paste("SELECT count(*) AS n FROM", name))$n
  cat(sprintf("%-20s %6d rows\n", name, n))
  if (n != nrow(tables[[name]])) stop("Table ", name, " was not written completely.")
}
dbDisconnect(con, shutdown = TRUE)

# Replace the shipped database (fails if another R session still has it open)
if (file.exists(DB_FILE) && !file.remove(DB_FILE)) {
  stop("Cannot replace ", DB_FILE, ": close the other R session that has it open. ",
       "The new build is in ", NEW_FILE, ".")
}
if (!file.rename(NEW_FILE, DB_FILE)) {
  stop("The new database could not be moved into place. It is still in ", NEW_FILE, ".")
}
cat("\nDatabase written to", DB_FILE, "\n")
