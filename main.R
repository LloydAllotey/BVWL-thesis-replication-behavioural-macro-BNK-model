#######################################################################
# Thesis replication: main.R
#
# Imperfect Credibility at the Effective Lower Bound:
# A Behavioural Macroeconomic Analysis of Eurozone Inflation Expectations
#
# This is the only script the reader needs. It reads the database built by
# build_database.R, classifies the SPF forecasts around two centres, estimates the
# two behavioural parameters of the expectations block by maximum likelihood and
# evaluates both models against the SPF mean. With the default parameters it
# reproduces the main results of Chapters 4 to 6. Change a parameter in Part I,
# run the script again, and every table and figure follows.
#
# What this script does:
#   Set up     loads the packages
#   Part I     parameters, each with its thesis value in a comment
#   Part II    input checks (hard stops and warnings)
#   Part III   loads the data, builds the centres and calibrates the constants
#   Part IV    model functions (Equations 4.1 and 5.1 to 5.14)
#   Part V     classification of every forecast and the shares by regime (Table 4.2)
#   Part VI    estimation of gamma and rho for both models (Table 5.3)
#   Part VII   evaluation against the SPF mean (Tables 5.4, 5.6 and 5.7)
#   Part VIII  side analyses, each behind a switch (Tables 5.5 and 6.1 to 6.4), and Table J.1
#   Part IX    comparison with the thesis baseline
#   Part X     Figures 4.1 to 4.3, 5.1 to 5.4, J.1 and J.2
#
# Input:  data/thesis.duckdb (tables estimation_panel and rounds)
# Output: tables in the console, figures in the Plots pane. Every table and figure
#         also stays in the workspace as an object named after its thesis number,
#         for example table_5_6_gain or fig_5_2_profiles.
# Running time: about two and a half minutes with the defaults.
#
# Lloyd Allotey, St. Gallen, November 2026
#######################################################################


# Set up ------------------------------------------------------------------

library(DBI)       # dbConnect, dbReadTable
library(duckdb)    # the database engine
library(dplyr)     # filter, mutate, group_by, summarise, joins
library(ggplot2)   # figures

options(width = 160)   # print wide tables on one line
options(warn = 1)      # print a warning where it occurs, not at the end of the run


# Part I: Parameters ------------------------------------------------------

# Data inputs: the database and its two tables (built by build_database.R). No other
# data value is hard coded in this script.
DB_FILE      <- "data/thesis.duckdb"
TABLE_PANEL  <- "estimation_panel"   # one row per forecast
TABLE_ROUNDS <- "rounds"             # one row per survey round

# Centre of the second model. "ILS": perceived anchor from the 5y5y inflation-linked
# swap (Model B, thesis). "LTSPF": mean SPF five-year-ahead forecast (Model S).
anchor <- "ILS"

# Performance score. "saturating": Equation 5.2 with tau and w (thesis). "msfe": the
# squared error, De Grauwe's -MSFE (Section 6.3.1). tau, w and E_star are then unused.
score <- "saturating"

# Statistic of the biases (Equation 5.5). "median": thesis. "mean": Section 6.3.2.
bias_statistic <- "median"

# Rounds the biases are calibrated on. "complete": the whole sample (thesis). "pre ELB":
# the rounds before the ELB regime, held fixed afterwards (Section 6.3.2).
bias_window <- "complete"

# Centres, sample and regimes
target      <- 2         # announced inflation target of Model A, in % (thesis: 2)
first_round <- "2004Q3"  # estimation window (thesis: 2004Q3 to 2026Q2, 88 rounds)
last_round  <- "2026Q2"
bench_from  <- "2004Q3"  # normalisation window of the perceived anchor, Equation 3.2
bench_to    <- "2014Q2"  # (thesis: 2004Q3 to 2014Q2, 40 rounds, premium 0.3452)
elb_from    <- "2014Q3"  # ELB regime, Table 3.4 (thesis: 2014Q3 to 2022Q2, 32 rounds)
elb_to      <- "2022Q2"
horizon     <- 4         # rounds between a forecast and its evaluation (thesis: 4)

# Calibrated constants (Table 5.2). NA takes the value the thesis calibrates from the
# data. To see what one constant does, all else equal, replace its NA by a number.
# The other constants keep their values.
delta  <- NA   # thesis 0.2824: half width of the targeter band (Equation 4.3)
E_star <- NA   # thesis 3.11: threshold between ordinary and extreme errors (Equation 5.7)
tau    <- NA   # thesis 0.399: scale of the saturating score (Equation 5.6)
w      <- NA   # thesis 0.101: weight of the squared error (Equation 5.6)

# Heuristic biases (Table 5.1), one entry per model. NA calibrates the bias from the
# classified forecasts (Equation 5.5), so it follows delta, the centre and the two bias
# switches above. A number fixes it.
bias_up   <- c(A = NA, B = NA, S = NA)   # thesis 0.700 (A), 0.487 (B), 0.594 (S)
bias_down <- c(A = NA, B = NA, S = NA)   # thesis 0.600 (A), 0.544 (B), 0.531 (S)

U_0 <- 0   # performance of every heuristic before the first evaluation (thesis: 0)

# Starting values of the optimiser (Section 5.3.1)
quick_mode        <- TRUE           # FALSE: 25 x 12 = 300 starts (thesis). TRUE: 6 x 5 = 30.
gamma_start_range <- c(0.01, 100)   # log-spaced gamma (thesis: 0.01 to 100)
rho_start_range   <- c(0, 0.98)     # evenly spaced rho (thesis: 0 to 0.98)

# Side analyses. Each re-estimates gamma and rho with 30 starting values, as in Chapter 6.
run_feasibility <- TRUE   # Table 5.5: can the model reach the SPF mean at all?
run_anchors     <- TRUE   # Table 6.1: Models A, B and S side by side
run_delta       <- TRUE   # Table 6.2: delta at -50%, -25%, +25%, +50%
run_score       <- TRUE   # Table 6.3: -MSFE instead of the saturating score, tau and w varied
run_biases      <- TRUE   # Table 6.4: statistic and window of the biases, real-time biases

# The thesis value of every parameter, and the value of this run. Part IX compares them.
baseline <- list(anchor = "ILS", score = "saturating",
                 bias_statistic = "median", bias_window = "complete", target = 2,
                 first_round = "2004Q3", last_round = "2026Q2",
                 bench_from = "2004Q3", bench_to = "2014Q2",
                 elb_from = "2014Q3", elb_to = "2022Q2", horizon = 4,
                 delta = NA, E_star = NA, tau = NA, w = NA,
                 bias_up = c(A = NA, B = NA, S = NA), bias_down = c(A = NA, B = NA, S = NA),
                 U_0 = 0, quick_mode = FALSE,
                 gamma_start_range = c(0.01, 100), rho_start_range = c(0, 0.98))
this_run <- mget(names(baseline))

# Table 5.6 of the thesis: RMSFE of the centre alone and of the complete model, in pp
thesis_table_5_6_gain <- data.frame(
  regime      = rep(c("Full sample", "Pre ELB", "ELB", "Post ELB"), each = 2),
  model       = rep(c("A", "B"), times = 4),
  centre_only = c(0.667, 0.555, 0.402, 0.451, 0.730, 0.323, 0.951, 0.975),
  complete    = c(0.492, 0.473, 0.433, 0.403, 0.441, 0.297, 0.679, 0.796),
  gain_pct    = c(26, 15, -8, 11, 40, 8, 29, 18))


# Part II: Input checks ---------------------------------------------------

# What a valid value looks like: one number, one number or NA, a round such as "2014Q3"
is_number       <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)
is_number_or_na <- function(x) length(x) == 1 && (is.na(x) || is.numeric(x))
is_round        <- function(x) is.character(x) && length(x) == 1 && grepl("^[0-9]{4}Q[1-4]$", x)

# Hard stops: values that are impossible or would crash the analysis
if (!is_number(target))  stop("Parameter target must be one number.")
if (!is_number(horizon)) stop("Parameter horizon must be one number.")
if (!is_number(U_0))     stop("Parameter U_0 must be one number.")

if (!is_number_or_na(delta)) {
  stop("Parameter delta must be one number or NA (calibrated from the data).")
}
if (!is_number_or_na(E_star)) {
  stop("Parameter E_star must be one number or NA (calibrated from the data).")
}
if (!is_number_or_na(tau)) {
  stop("Parameter tau must be one number or NA (calibrated from the data).")
}
if (!is_number_or_na(w)) {
  stop("Parameter w must be one number or NA (calibrated from the data).")
}
if (any(c(delta, tau, E_star) <= 0, na.rm = TRUE)) stop("delta, tau and E_star must be positive.")
if (!is.na(w) && w < 0)                            stop("w cannot be negative.")

if (!identical(names(bias_up), c("A", "B", "S")) ||
    !identical(names(bias_down), c("A", "B", "S"))) {
  stop("bias_up and bias_down need one entry per model: c(A = NA, B = NA, S = NA).")
}
if (horizon < 1 || horizon != round(horizon)) {
  stop("horizon must be a whole number of rounds, at least 1.")
}
if (!anchor %in% c("ILS", "LTSPF"))      stop('anchor must be "ILS" or "LTSPF".')
if (!score %in% c("saturating", "msfe")) stop('score must be "saturating" or "msfe".')
if (!bias_statistic %in% c("median", "mean")) {
  stop('bias_statistic must be "median" or "mean".')
}
if (!bias_window %in% c("complete", "pre ELB")) {
  stop('bias_window must be "complete" or "pre ELB".')
}

if (!is_round(first_round)) stop('Parameter first_round must look like "2014Q3".')
if (!is_round(last_round))  stop('Parameter last_round must look like "2014Q3".')
if (!is_round(bench_from))  stop('Parameter bench_from must look like "2014Q3".')
if (!is_round(bench_to))    stop('Parameter bench_to must look like "2014Q3".')
if (!is_round(elb_from))    stop('Parameter elb_from must look like "2014Q3".')
if (!is_round(elb_to))      stop('Parameter elb_to must look like "2014Q3".')
if (first_round >= last_round) stop("first_round must come before last_round.")
if (bench_from > bench_to)     stop("bench_from must not come after bench_to.")
if (elb_from > elb_to)         stop("elb_from must not come after elb_to.")

if (any(gamma_start_range <= 0) || gamma_start_range[1] >= gamma_start_range[2]) {
  stop("gamma_start_range must be two positive numbers in increasing order.")
}
if (any(rho_start_range < 0) || any(rho_start_range >= 1) ||
    rho_start_range[1] >= rho_start_range[2]) {
  stop("rho_start_range must lie inside [0, 1) in increasing order.")
}
if (!file.exists(DB_FILE)) stop("Database not found: ", DB_FILE, ". Run build_database.R first.")

# Warnings: values outside the range tested in the thesis. The script runs, but the
# results may be meaningless.
if (!is.na(delta) && (delta < 0.07 || delta > 3.0)) {
  warning("delta outside the tested range 0.07 to 3.0.")
}
if (!is.na(tau) && (tau < 0.1 || tau > 1.0)) {
  warning("tau outside the tested range 0.1 to 1.0.")
}
if (!is.na(w) && w > 0.5) {
  warning("w above the tested range (0 to 0.5).")
}
if (!is.na(E_star) && (E_star < 1 || E_star > 10)) {
  warning("E_star outside the tested range 1 to 10.")
}


# Part III: Load data -----------------------------------------------------

# shared_home = FALSE: duckdb keeps nothing in the home folder and asks no question about it
con    <- dbConnect(duckdb(shared_home = FALSE), dbdir = DB_FILE, read_only = TRUE)
panel  <- dbReadTable(con, TABLE_PANEL)   # one row per forecast
rounds <- dbReadTable(con, TABLE_ROUNDS)  # one row per round
dbDisconnect(con, shutdown = TRUE)

# Hard stops that need the data: the sample window must lie inside the data
if (first_round < min(rounds$round) || last_round > max(rounds$round)) {
  stop("The sample window lies outside the data (", min(rounds$round), " to ",
       max(rounds$round), ").")
}
if (bench_from < first_round || bench_to > last_round) {
  stop("The normalisation window lies outside the sample.")
}
if (elb_from < first_round || elb_to > last_round) {
  stop("The ELB regime lies outside the sample.")
}

# Keep the sample window. The regime stored in the database is dated by the deposit
# facility rate (Table 3.4). It is kept as regime_dfr and checked against Part I below.
rounds <- rounds |>
  filter(round >= first_round, round <= last_round) |>
  arrange(survey_date) |>
  rename(regime_dfr = regime)
panel <- panel |>
  filter(round >= first_round, round <= last_round) |>
  arrange(survey_date, forecaster) |>
  select(round, survey_date, forecaster, forecast, hicp)

# The perceived anchor (Equation 3.2): the swap rate less its mean premium over the
# normalisation window, under the assumption that expectations were anchored at the
# target during that window
in_bench <- rounds$round >= bench_from & rounds$round <= bench_to
if (sum(in_bench) < 8) {
  warning("The normalisation window has fewer than 8 rounds; the premium rests on little data.")
}
premium <- mean(rounds$ils[in_bench]) - target
#CHECK: premium 0.3452 (Section 3.2)
cat("Premium of the swap rate:", round(premium, 4), "\n")

# The centre of every model in every round (Table 5.1)
rounds <- rounds |>
  mutate(centre_A = target,          # Model A: the announced target
         centre_B = ils - premium,   # Model B: the perceived anchor
         centre_S = lt_mean)         # Model S: the mean SPF five-year-ahead forecast

# The two models of this run: Model A and the model chosen with `anchor`
second <- if (anchor == "ILS") "B" else "S"
models <- c("A", second)

# Regimes of this run, from the parameters of Part I
regime_of <- function(round) {
  case_when(round < elb_from ~ "Pre ELB", round <= elb_to ~ "ELB", TRUE ~ "Post ELB")
}
rounds <- rounds |> mutate(regime = regime_of(round))
#CHECK: the parameter dates agree with the regimes dated by the deposit facility rate (Table 3.4)
if (any(toupper(rounds$regime) != toupper(rounds$regime_dfr))) {
  warning("elb_from / elb_to differ from the regimes dated by the deposit facility rate ",
          "in the database.")
}
cat("Rounds by regime:",
    paste(names(table(rounds$regime)), table(rounds$regime), collapse = ", "), "\n")

# The calibrated constants (Table 5.2). First the value the data imply for each constant,
# at full precision. Realised inflation is known for the forecasts made up to `horizon`
# rounds before the last round.
last_known <- rounds$round[nrow(rounds) - horizon]
errors     <- panel |>
  filter(round <= last_known, !is.na(hicp)) |>
  mutate(error = hicp - forecast)
robust_sd  <- median(abs(errors$error - median(errors$error))) / qnorm(0.75)

from_data <- c(delta  = median(rounds$spf_sd),   # Equation 4.3
               E_star = 3 * robust_sd)           # Equation 5.7
from_data["tau"] <- sqrt(2) * from_data["delta"]                        # Equation 5.6
from_data["w"]   <- 1 / (from_data["E_star"]^2 + from_data["tau"]^2)    # Equation 5.6

# Each NA of Part I takes the value from the data. A typed number stays, and changes
# nothing but that one constant.
if (is.na(delta))  delta  <- from_data[["delta"]]
if (is.na(E_star)) E_star <- from_data[["E_star"]]
if (is.na(tau))    tau    <- from_data[["tau"]]
if (is.na(w))      w      <- from_data[["w"]]

#CHECK: delta 0.2824, E_star 3.1135, tau 0.3994, w 0.1015 from 4'001 errors
cat("\nCalibrated constants: value used and value implied by the data (",
    nrow(errors), " errors):\n", sep = "")
print(data.frame(used      = signif(c(delta, E_star, tau, w), 6),
                 from_data = signif(from_data[c("delta", "E_star", "tau", "w")], 6),
                 note      = ifelse(is.na(this_run[c("delta", "E_star", "tau", "w")]),
                                    "", "typed in Part I"),
                 row.names = c("delta", "E_star", "tau", "w")))
if (score == "msfe") {
  cat("Score = -MSFE: the squared error alone, so tau, w and E_star are not used in this run.\n")
}
if (any(c(bias_up, bias_down) <= delta, na.rm = TRUE)) {
  warning("A fixed bias is not larger than delta, so that heuristic lies inside the ",
          "targeter band.")
}


# Part IV: Model functions ------------------------------------------------

# Three names recur as arguments. `model` is a model letter, "A", "B" or "S". `data` is
# the list that prepare() returns for one model. `score_set` holds the settings of the
# performance score, made by score_settings().

REGIMES <- c("Full sample", "Pre ELB", "ELB", "Post ELB")

# Classification of a forecast around a centre with tolerance delta (Equation 4.1). The
# small tolerance keeps a forecast that sits exactly on the band edge inside the band.
classify <- function(forecast, centre, delta) {
  case_when(forecast > centre + delta + 1e-9 ~ "hawk",
            forecast < centre - delta - 1e-9 ~ "dove",
            TRUE                             ~ "targeter")
}

# The whole panel classified around the centre of one model. This is the only place
# where forecasts are classified: Part V and prepare() both call it.
classify_panel <- function(model, delta) {
  centres <- data.frame(round  = rounds$round,
                        regime = rounds$regime,
                        centre = rounds[[paste0("centre_", model)]])
  panel |>
    left_join(centres, by = "round") |>
    mutate(model = model,
           type  = classify(forecast, centre, delta))
}

# The fit a warning is about: the model and the settings that can differ between fits,
# written as in the rows of Tables 6.2 and 6.3, for example
# "Model B, delta 0.4000, tau 0.3994, w 0.0000". `calibration` names the biases of a fit of
# Table 6.4 whose biases are not those of the run, for example "mean biases, real time".
fit_label <- function(model, delta, score_set = NULL, calibration = NULL) {
  four  <- function(v) format(round(v, 4), nsmall = 4)
  label <- paste0("Model ", model, ", delta ", four(delta))
  if (!is.null(calibration)) label <- paste0(label, ", ", calibration)
  if (is.null(score_set)) return(label)
  if (score_set$msfe) return(paste0(label, ", score -MSFE"))
  paste0(label, ", tau ", four(score_set$tau), ", w ", four(score_set$w))
}

# The two biases of one model in every round (Equation 5.5): the median or the mean of the
# distances of the hawkish and of the dovish forecasts to the centre. `window` chooses the
# forecasts. "complete": all rounds (thesis). "pre ELB": the rounds before the ELB regime.
# Both give one constant bias. "real time": in every round the forecasts of all earlier
# rounds, so the bias changes from round to round (Section 6.3.2). It needs `horizon`
# earlier rounds. `b_up` and `b_down` are the biases of Part I: a number is kept as it is.
# A bias stays NA where it cannot be calibrated, because no forecast of the rounds used
# lies on its side of the targeter band.
calibrate_biases <- function(forecasts, statistic, window, b_up, b_down) {
  average <- if (statistic == "median") median else mean
  hawks   <- forecasts |> filter(type == "hawk") |> mutate(distance = forecast - centre)
  doves   <- forecasts |> filter(type == "dove") |> mutate(distance = centre - forecast)
  if (window == "pre ELB") {
    hawks <- hawks |> filter(regime == "Pre ELB")
    doves <- doves |> filter(regime == "Pre ELB")
  }
  up   <- rep(as.numeric(b_up),   nrow(rounds))
  down <- rep(as.numeric(b_down), nrow(rounds))

  if (window == "real time") {
    for (i in seq_len(nrow(rounds))) {
      earlier_hawks <- hawks$distance[hawks$round < rounds$round[i]]
      earlier_doves <- doves$distance[doves$round < rounds$round[i]]
      if (is.na(b_up) && i > horizon && length(earlier_hawks) > 0) {
        up[i] <- average(earlier_hawks)
      }
      if (is.na(b_down) && i > horizon && length(earlier_doves) > 0) {
        down[i] <- average(earlier_doves)
      }
    }
  } else {
    if (is.na(b_up)   && nrow(hawks) > 0) b_up   <- average(hawks$distance)
    if (is.na(b_down) && nrow(doves) > 0) b_down <- average(doves$distance)
    up[]   <- b_up
    down[] <- b_down
  }

  # b_up and b_down: the constant biases, NA in real time unless typed in Part I
  list(by_round = data.frame(round  = rounds$round,
                             date   = rounds$survey_date,
                             regime = rounds$regime,
                             b_up   = up,
                             b_down = down),
       b_up     = as.numeric(b_up),
       b_down   = as.numeric(b_down),
       n_hawks  = nrow(hawks),
       n_doves  = nrow(doves))
}

# Data of one model: the two biases (Equation 5.5) and, for every scored round, the
# counts, the centre of the evaluation round (where the heuristics were formed), the
# realised inflation they are scored against, the current centre and the SPF mean.
# By default the biases are the ones set in Part I for this model, calibrated with the
# statistic and on the rounds of the two bias switches. Table 6.4 passes other choices;
# `same_rounds` scores constant biases on the rounds that real-time biases can use.
prepare <- function(model, delta, b_up = bias_up[[model]], b_down = bias_down[[model]],
                    statistic = bias_statistic, window = bias_window, same_rounds = FALSE) {
  forecasts <- classify_panel(model, delta)
  biases    <- calibrate_biases(forecasts, statistic, window, b_up, b_down)
  in_rounds <- if (window == "pre ELB") " in the pre ELB rounds" else ""
  if (window != "real time" && is.na(biases$b_up)) {
    stop("No forecast lies above the targeter band around this centre", in_rounds,
         ", so the hawk bias cannot be calibrated. Move the centre or set bias_up.")
  }
  if (window != "real time" && is.na(biases$b_down)) {
    stop("No forecast lies below the targeter band around this centre", in_rounds,
         ", so the dove bias cannot be calibrated. Move the centre or set bias_down.")
  }

  per_round <- forecasts |>
    group_by(round) |>
    summarise(centre   = first(centre),
              hawk     = sum(type == "hawk"),
              targeter = sum(type == "targeter"),
              dove     = sum(type == "dove"),
              .groups  = "drop") |>
    left_join(rounds |> select(round, survey_date, regime, hicp, spf_mean), by = "round") |>
    left_join(biases$by_round |> select(round, b_up, b_down), by = "round") |>
    arrange(survey_date)

  # A forecast made in round t - horizon is scored in round t, when its outcome is known.
  # A real-time bias exists from round horizon + 1 on, so the first `horizon` rounds form
  # no heuristics then.
  scored     <- (horizon + 1):nrow(per_round)
  evaluation <- scored - horizon
  known      <- !is.na(per_round$hicp[evaluation])
  if (window == "real time" || same_rounds) known <- known & evaluation > horizon
  scored     <- scored[known]
  evaluation <- evaluation[known]
  counts     <- as.matrix(per_round[scored, c("hawk", "targeter", "dove")])

  # The biases of a fit of Table 6.4 that are not those of the run, named in the warnings
  calibration <- NULL
  if (statistic != bias_statistic || window != bias_window || same_rounds) {
    calibration <- paste0(statistic, " biases, ", window,
                          if (window != "real time") " sample", if (same_rounds) ", same rounds")
  }
  used <- c(evaluation, scored)
  if (any(is.na(per_round$b_up[used]), is.na(per_round$b_down[used]))) {
    stop(fit_label(model, delta, calibration = calibration), ": a real-time bias is ",
         "missing, because no earlier forecast lies on that side of the targeter band.")
  }

  # A centre far from the forecasts leaves one heuristic with nearly every forecaster; the
  # shares then hardly move over time and gamma and rho are poorly identified
  pooled <- colSums(counts)
  if (max(pooled) / sum(pooled) > 0.9) {
    warning(sprintf(paste("%s: one heuristic takes %.0f%% of the scored forecasts",
                          "(hawks %d, targeters %d, doves %d). The shares hardly vary,",
                          "so gamma and rho may not be identified."),
                    fit_label(model, delta, calibration = calibration),
                    100 * max(pooled) / sum(pooled), pooled[1], pooled[2], pooled[3]),
            call. = FALSE)
  }

  # b_up and b_down are the constant biases (NA in real time). The heuristics scored in a
  # round were formed with the biases of the evaluation round (old); the expectation of a
  # round uses the biases of that round (now).
  list(model       = model,
       delta       = delta,
       calibration = calibration,
       round       = per_round$round[scored],
       date        = per_round$survey_date[scored],
       regime      = per_round$regime[scored],
       counts      = counts,
       centre_old  = per_round$centre[evaluation],
       centre_now  = per_round$centre[scored],
       hicp        = per_round$hicp[evaluation],
       spf_mean    = per_round$spf_mean[scored],
       b_up        = biases$b_up,
       b_down      = biases$b_down,
       b_up_old    = per_round$b_up[evaluation],
       b_down_old  = per_round$b_down[evaluation],
       b_up_now    = per_round$b_up[scored],
       b_down_now  = per_round$b_down[scored],
       n_hawks     = biases$n_hawks,
       n_doves     = biases$n_doves)
}

# Settings of the performance score: the saturating score of Equation 5.2 or the -MSFE
score_settings <- function(tau, w, msfe = FALSE) list(tau = tau, w = w, msfe = msfe)

# Shares of the hawk, targeter and dove heuristic in every scored round
probabilities <- function(gamma, rho, data, score_set) {
  heuristic <- cbind(data$centre_old + data$b_up_old,    # Table 5.1
                     data$centre_old,
                     data$centre_old - data$b_down_old)
  error <- data$hicp - heuristic                     # Equation 5.1
  if (score_set$msfe) {
    psi <- error^2                                   # -MSFE (Section 6.3.1)
  } else {
    psi <- error^2 / (error^2 + score_set$tau^2) + score_set$w * error^2   # Equation 5.2
  }
  U        <- matrix(0, nrow(psi), 3)
  U_before <- c(U_0, U_0, U_0)
  for (t in 1:nrow(psi)) {                           # Equation 5.3
    U[t, ]   <- rho * U_before - (1 - rho) * psi[t, ]
    U_before <- U[t, ]
  }
  weights <- exp(gamma * (U - apply(U, 1, max)))     # Equation 5.4
  weights / rowSums(weights)
}

# Minus the multinomial log-likelihood (Equation 5.8); a huge value outside the admissible
# region, gamma in [0, 1000] and rho in [0, 0.999)
minus_loglik <- function(par, data, score_set) {
  if (par[1] < 0 || par[1] > 1000 || par[2] < 0 || par[2] >= 0.999) return(1e12)
  -sum(data$counts * log(pmax(probabilities(par[1], par[2], data, score_set), 1e-12)))
}

# Starting values: log-spaced gamma, evenly spaced rho (Section 5.3.1)
starting_values <- function(n_gamma, n_rho) {
  expand.grid(gamma = exp(seq(log(gamma_start_range[1]), log(gamma_start_range[2]),
                              length.out = n_gamma)),
              rho   = seq(rho_start_range[1], rho_start_range[2], length.out = n_rho))
}

# Nelder-Mead from every starting value, each search restarted once from its own result.
# Returns the best pair and the end point of every search (to check that they agree).
estimate <- function(data, score_set, starts) {
  searches <- data.frame(gamma = numeric(0), rho = numeric(0), loglik = numeric(0))
  for (i in 1:nrow(starts)) {
    search <- optim(c(starts$gamma[i], starts$rho[i]), minus_loglik,
                    data = data, score_set = score_set, method = "Nelder-Mead",
                    control = list(maxit = 4000, reltol = 1e-12))
    search <- optim(search$par, minus_loglik,
                    data = data, score_set = score_set, method = "Nelder-Mead",
                    control = list(maxit = 4000, reltol = 1e-12))
    searches[i, ] <- c(search$par, -search$value)
  }
  best <- searches[which.max(searches$loglik), ]
  list(gamma = best$gamma, rho = best$rho, loglik = best$loglik, searches = searches)
}

# Dispersion factor (Equation 5.10): deviance over (rounds x 2 free shares - 2 parameters)
dispersion <- function(data, prob) {
  shares   <- data$counts / rowSums(data$counts)
  deviance <- 2 * sum(data$counts * log(pmax(shares, 1e-12) / pmax(prob, 1e-12)))
  deviance / (nrow(data$counts) * 2 - 2)
}

# Standard errors (Equations 5.11 and 5.12): the Hessian of minus the log-likelihood from
# central differences with steps of 0.01, inverted and multiplied by the dispersion factor.
# At the bound of the admissible region the Wald approximation does not hold, and a flat
# likelihood cannot be inverted. In both cases the standard errors are NA, with a warning
# that names the fit.
standard_errors <- function(gamma, rho, data, score_set, phi) {
  no_se <- list(se = c(NA_real_, NA_real_), correlation = NA_real_)
  if (gamma < 0.01 || gamma > 1000 - 0.01 || rho < 0.01 || rho > 0.999 - 0.01) {
    warning(sprintf(paste("%s: the estimate (gamma %.4g, rho %.4g) lies at the bound of the",
                          "admissible region, so gamma and rho are not identified in this fit",
                          "and no standard errors are reported."),
                    fit_label(data$model, data$delta, score_set, data$calibration), gamma, rho),
            call. = FALSE)
    return(no_se)
  }
  step <- 0.01
  f    <- function(a, b) minus_loglik(c(gamma + a * step, rho + b * step), data, score_set)
  H_gg <- (f(1, 0) - 2 * f(0, 0) + f(-1, 0)) / step^2
  H_rr <- (f(0, 1) - 2 * f(0, 0) + f(0, -1)) / step^2
  H_gr <- (f(1, 1) - f(1, -1) - f(-1, 1) + f(-1, -1)) / (4 * step^2)
  V    <- tryCatch(phi * solve(matrix(c(H_gg, H_gr, H_gr, H_rr), 2, 2)),
                   error = function(condition) NULL)
  if (is.null(V) || any(!is.finite(V)) || any(diag(V) <= 0)) {
    warning(sprintf(paste("%s: the Hessian at the estimate (gamma %.4g, rho %.4g) cannot be",
                          "inverted. The likelihood is flat in at least one direction, so no",
                          "standard errors are reported."),
                    fit_label(data$model, data$delta, score_set, data$calibration), gamma, rho),
            call. = FALSE)
    return(no_se)
  }
  list(se = sqrt(diag(V)), correlation = V[1, 2] / sqrt(V[1, 1] * V[2, 2]))
}

# One complete fit of a model: estimation, shares, aggregate expectation (Equation 5.13)
# and the errors against the SPF mean (Equation 5.14). `results` holds one row per scored
# round, for the complete model and for the centre alone. `statistic`, `window` and
# `same_rounds` choose the biases as in prepare().
fit_model <- function(model, score_set, starts, delta_used = delta,
                      statistic = bias_statistic, window = bias_window, same_rounds = FALSE) {
  data      <- prepare(model, delta_used, statistic = statistic, window = window,
                       same_rounds = same_rounds)
  estimated <- estimate(data, score_set, starts)
  prob      <- probabilities(estimated$gamma, estimated$rho, data, score_set)
  phi       <- dispersion(data, prob)

  heuristics  <- cbind(data$centre_now + data$b_up_now,
                       data$centre_now,
                       data$centre_now - data$b_down_now)
  expectation <- rowSums(prob * heuristics)                         # Equation 5.13
  results     <- data.frame(model        = model,
                            round        = data$round,
                            date         = data$date,
                            regime       = data$regime,
                            centre       = data$centre_now,
                            lower        = data$centre_now - data$b_down_now,   # attainable
                            upper        = data$centre_now + data$b_up_now,     # range
                            expectation  = expectation,
                            spf_mean     = data$spf_mean,
                            error        = expectation - data$spf_mean,       # complete model
                            centre_error = data$centre_now - data$spf_mean)   # centre alone

  c(data, estimated, list(prob = prob, phi = phi, results = results),
    standard_errors(estimated$gamma, estimated$rho, data, score_set, phi))
}

# Loss measures (Equation 5.14)
rmsfe <- function(error) sqrt(mean(error^2))
mafe  <- function(error) mean(abs(error))
msfe  <- function(error) mean(error^2)

# Every row once under its own regime and once under "Full sample", so that a group_by on
# the regime also gives the full-sample line of the thesis tables
with_full_sample <- function(rows) {
  bind_rows(rows |> mutate(regime = "Full sample"), rows)
}

# Number formats of the thesis tables
three  <- function(v) format(round(v, 3), nsmall = 3)
two    <- function(v) format(round(v, 2), nsmall = 2)
pct    <- function(v) sprintf("%+d%%", round(v))
signed <- function(v) ifelse(v == 0, "0.000", sprintf("%+.3f", v))   # a gain with its sign


# Part V: Classification --------------------------------------------------

# One row per forecast and model
classified <- bind_rows(classify_panel("A", delta), classify_panel(second, delta))

# Shares per round and model (Equation 4.2)
shares <- classified |>
  group_by(model, round) |>
  summarise(date     = first(survey_date),
            regime   = first(regime),
            n        = n(),
            hawk     = mean(type == "hawk"),
            targeter = mean(type == "targeter"),
            dove     = mean(type == "dove"),
            .groups  = "drop")

# Table 4.2: mean targeter share by regime around both centres
table_4_2_shares <- with_full_sample(shares) |>
  group_by(regime) |>
  summarise(rounds                = n_distinct(round),
            targeter_share_second = 100 * mean(targeter[model == second]),
            targeter_share_A      = 100 * mean(targeter[model == "A"]),
            .groups               = "drop") |>
  mutate(difference_pp = targeter_share_second - targeter_share_A) |>
  arrange(match(regime, REGIMES)) |>
  as.data.frame()
#CHECK: 47.03 / 31.71, 51.50 / 45.12, 44.87 / 9.56, 40.16 / 42.50 (Table 4.2, Model B)
cat("\nTable 4.2  Targeter shares by regime, in % (second model =", second, ")\n")
print(table_4_2_shares |> mutate(across(where(is.double), function(v) round(v, 2))),
      row.names = FALSE)


# Part VI: Estimation of gamma and rho ------------------------------------

starts_main <- if (quick_mode) starting_values(6, 5) else starting_values(25, 12)
starts_side <- starting_values(6, 5)
base_score  <- score_settings(tau, w, msfe = (score == "msfe"))

cat("\nEstimating Model A and Model", second, "from", nrow(starts_main), "starting values,",
    if (score == "msfe") "-MSFE score" else "saturating score", "...\n")
# fit holds the fit of each model, for example fit$A. results holds one row per scored
# round and model and is the source of the tables and figures below.
fit     <- list()
results <- NULL
for (model in models) {
  fit[[model]] <- fit_model(model, base_score, starts_main)
  results      <- bind_rows(results, fit[[model]]$results)
}

# The same element of both fits side by side, for example from_fits("gamma")
from_fits <- function(element) sapply(fit, function(model_fit) model_fit[[element]])

# Convergence: how many searches end within 0.0001 of the best log-likelihood
#CHECK: all 300 searches end at the same point in both models (Section 5.3.1)
for (model in models) {
  at_maximum <- sum(abs(fit[[model]]$searches$loglik - fit[[model]]$loglik) < 1e-4)
  cat(sprintf("Model %s: log-likelihood %.4f, %d of %d searches at the maximum\n",
              model, fit[[model]]$loglik, at_maximum, nrow(fit[[model]]$searches)))
}

# Table 5.2 (part): the calibrated biases and the forecasts they rest on
#CHECK: A 0.700 / 0.600 on 527 / 2'288 forecasts, B 0.487 / 0.544 on 1'092 / 1'100
cat("\nBiases (Equation 5.5)\n")
print(data.frame(model     = models,
                 hawk_bias = three(from_fits("b_up")),
                 hawks     = from_fits("n_hawks"),
                 dove_bias = three(from_fits("b_down")),
                 doves     = from_fits("n_doves")),
      row.names = FALSE)

# Table 5.3: estimates, dispersion, adjusted standard errors and 95% Wald intervals
#CHECK: gamma 3.056 / 2.271, rho 0.611 / 0.625, phi 15.35 / 15.36, se 0.347, 0.069 / 0.396, 0.098
wald <- function(value, se) {
  paste0("[", three(value - 1.96 * se), ", ", three(value + 1.96 * se), "]")
}
# One column of the table: the formatted results of one model
column_5_3 <- function(model_fit) {
  c(three(model_fit$gamma),
    three(model_fit$rho),
    two(model_fit$correlation),
    two(model_fit$phi),
    paste(three(model_fit$se[1]), three(model_fit$se[2]), sep = ", "),
    wald(model_fit$gamma, model_fit$se[1]),
    wald(model_fit$rho, model_fit$se[2]),
    two(model_fit$loglik))
}
table_5_3_estimates <- data.frame(
  row.names = c("Intensity of choice, gamma", "Memory, rho", "Correlation",
                "Dispersion factor, phi", "Se(gamma), Se(rho)", "Wald interval, gamma",
                "Wald interval, rho", "Log-likelihood"))
for (model in models) {
  table_5_3_estimates[[paste("Model", model)]] <- column_5_3(fit[[model]])
}
cat("\nTable 5.3  Overdispersion, standard errors and confidence intervals\n")
print(table_5_3_estimates)


# Part VII: Evaluation and Table 5.6 --------------------------------------

# Table 5.4: the aggregate expectation of each model against the SPF mean, by regime, and
# the difference between the two models
#CHECK: full sample RMSFE 0.492 / 0.473, ELB 0.441 / 0.297, post ELB 0.679 / 0.796
losses <- with_full_sample(results) |>
  group_by(regime, model) |>
  summarise(rounds  = n(),
            MSFE    = msfe(error),
            RMSFE   = rmsfe(error),
            MAFE    = mafe(error),
            .groups = "drop")
difference <- losses |>
  group_by(regime) |>
  summarise(rounds  = first(rounds),
            MSFE    = MSFE[model == "A"]  - MSFE[model == second],
            RMSFE   = RMSFE[model == "A"] - RMSFE[model == second],
            MAFE    = MAFE[model == "A"]  - MAFE[model == second],
            .groups = "drop") |>
  mutate(model = paste(models, collapse = "-"))
table_5_4_errors <- bind_rows(losses, difference) |>
  arrange(match(regime, REGIMES), match(model, c(models, paste(models, collapse = "-")))) |>
  select(regime, rounds, model, MSFE, RMSFE, MAFE) |>
  as.data.frame()
cat("\nTable 5.4  Model expectations against the mean SPF forecast\n")
print(table_5_4_errors |> mutate(across(c(MSFE, RMSFE, MAFE), three)), row.names = FALSE)

# Table 5.6: the centre alone against the complete model. The switching gain is the fall
# in the RMSFE that the switching component adds to the centre (Equation 5.15).
#CHECK: ELB A 0.730 -> 0.441 (+40%), B 0.323 -> 0.297 (+8%)
table_5_6_gain <- with_full_sample(results) |>
  group_by(regime, model) |>
  summarise(centre_only = rmsfe(centre_error),
            complete    = rmsfe(error),
            .groups     = "drop") |>
  mutate(gain     = centre_only - complete,
         gain_pct = 100 * gain / centre_only) |>
  arrange(match(regime, REGIMES), match(model, models)) |>
  as.data.frame()
cat("\nTable 5.6  RMSFE against the SPF mean (pp): the centre alone and the complete model\n")
print(table_5_6_gain |>
        mutate(centre_only = three(centre_only),
               complete    = three(complete),
               gain        = sprintf("%+.3f", gain),
               gain_pct    = pct(gain_pct)),
      row.names = FALSE)

# Table 5.7: the location gain next to the switching gain. Both models start from the
# announced target alone, which is the centre of Model A. The location gain is the fall in
# the RMSFE from the target to the model's own centre (zero for Model A), the switching gain
# the further fall from that centre to the complete model, as in Table 5.6. Together they
# give the total gain, also shown in % of the RMSFE of the target alone.
#CHECK: ELB A 0.730, 0.730, 0.441: 0.000, +0.289, +0.289 (+40%)
#CHECK: ELB B 0.730, 0.323, 0.297: +0.407, +0.026, +0.433 (+59%)
table_5_7_location <- table_5_6_gain |>
  group_by(regime) |>
  mutate(target_alone = centre_only[model == "A"]) |>
  ungroup() |>
  transmute(regime, model, target_alone, centre_only, complete,
            location_gain  = target_alone - centre_only,
            switching_gain = centre_only - complete,
            total_gain     = target_alone - complete,
            total_gain_pct = 100 * total_gain / target_alone) |>
  as.data.frame()
cat("\nTable 5.7  RMSFE against the SPF mean (pp): location gain and switching gain\n")
print(table_5_7_location |>
        mutate(across(c(target_alone, centre_only, complete), three),
               across(c(location_gain, switching_gain, total_gain), signed),
               total_gain_pct = sprintf("%+.0f%%", total_gain_pct)),
      row.names = FALSE)


# Part VIII: Side analyses (switchable) -----------------------------------

# One line per model and variation, in the layout of the Chapter 6 tables
summarise_fit <- function(model_fit, variation) {
  all_rounds <- model_fit$results
  elb        <- all_rounds |> filter(regime == "ELB")
  data.frame(variation        = variation,
             model            = model_fit$model,
             gamma            = model_fit$gamma,
             rho              = model_fit$rho,
             b_up             = model_fit$b_up,
             b_down           = model_fit$b_down,
             rmsfe_full       = rmsfe(all_rounds$error),
             rmsfe_elb        = rmsfe(elb$error),
             mafe_full        = mafe(all_rounds$error),
             mafe_elb         = mafe(elb$error),
             centre_rmsfe_elb = rmsfe(elb$centre_error),
             gain_elb         = 100 * (1 - rmsfe(elb$error) / rmsfe(elb$centre_error)))
}

# Both models under one setting, re-estimated from the starting values of the side analyses
both_models <- function(variation, score_set, delta_used = delta) {
  bind_rows(summarise_fit(fit_model("A",    score_set, starts_side, delta_used), variation),
            summarise_fit(fit_model(second, score_set, starts_side, delta_used), variation))
}

# Thesis layout: gamma and rho per model, each loss as "A / second"
layout_variations <- function(rows) {
  pair <- function(value, model) {
    paste(three(value[model == "A"]), "/", three(value[model == second]))
  }
  rows |>
    group_by(variation) |>
    summarise(gamma_A      = three(gamma[model == "A"]),
              rho_A        = three(rho[model == "A"]),
              gamma_2      = three(gamma[model == second]),
              rho_2        = three(rho[model == second]),
              `RMSFE full` = pair(rmsfe_full, model),
              `RMSFE ELB`  = pair(rmsfe_elb, model),
              `MAFE full`  = pair(mafe_full, model),
              `MAFE ELB`   = pair(mafe_elb, model),
              `Gain ELB`   = paste0(round(gain_elb[model == "A"]), "% / ",
                                    round(gain_elb[model == second]), "%"),
              .groups      = "drop") |>
    arrange(match(variation, unique(rows$variation))) |>
    as.data.frame()
}
show_variations <- function(rows) print(layout_variations(rows), row.names = FALSE)

baseline_label <- if (score == "msfe") "Baseline (-MSFE)" else "Baseline"
baseline_rows  <- bind_rows(summarise_fit(fit$A, baseline_label),
                            summarise_fit(fit[[second]], baseline_label))

# Table 5.5: can the model reach the SPF mean at all? The attainable range is
# [centre - dove bias, centre + hawk bias]. A SPF mean outside it cannot be reproduced by
# any shares.
if (run_feasibility) {
  outside <- with_full_sample(results) |>
    mutate(is_below = spf_mean < lower,   # below the dove heuristic
           is_above = spf_mean > upper,   # above the hawk heuristic
           gap      = case_when(is_below ~ lower - spf_mean,
                                is_above ~ spf_mean - upper,
                                TRUE     ~ 0)) |>
    group_by(regime, model) |>
    summarise(rounds   = n(),
              outside  = sum(is_below | is_above),
              below    = sum(is_below),
              above    = sum(is_above),
              distance = mean(gap[is_below | is_above]),
              .groups  = "drop")

  # Model A and the second model side by side, one line per regime
  table_5_5_feasibility <- left_join(outside |> filter(model == "A")    |> select(-model),
                                     outside |> filter(model == second) |> select(-model),
                                     by = c("regime", "rounds"),
                                     suffix = paste0("_", models)) |>
    arrange(match(regime, REGIMES)) |>
    as.data.frame()
  #CHECK: full sample 31 (25 below, 6 above) / 15 (8, 7); ELB 20 / 2; mean distance 0.309 / 0.520
  cat("\nTable 5.5  Feasibility: rounds in which the SPF mean lies outside the attainable range\n")
  print(table_5_5_feasibility |> mutate(across(starts_with("distance"), three)),
        row.names = FALSE)
}

# Table 6.1: the three anchors side by side. The two models of the main run are reused;
# the third model is fitted here.
if (run_anchors) {
  cat("\nTable 6.1  Models A, B and S (the third model from 30 starting values) ...\n")
  third     <- setdiff(c("B", "S"), second)
  fit_third <- fit_model(third, base_score, starts_side)
  table_6_1_anchors <- bind_rows(baseline_rows, summarise_fit(fit_third, baseline_label)) |>
    arrange(match(model, c("A", "B", "S"))) |>
    transmute(model,
              gamma            = three(gamma),
              rho              = three(rho),
              biases           = paste(three(b_up), "/", three(b_down)),
              rmsfe_full       = three(rmsfe_full),
              rmsfe_elb        = three(rmsfe_elb),
              mafe_full        = three(mafe_full),
              mafe_elb         = three(mafe_elb),
              centre_rmsfe_elb = three(centre_rmsfe_elb),
              gain_elb         = pct(gain_elb))
  #CHECK: Model S gamma 2.805, rho 0.588, biases 0.594 / 0.531, RMSFE 0.431 / 0.348, gain +35%
  print(table_6_1_anchors, row.names = FALSE)

  # The three centres by regime
  cat("\nMean centre by regime\n")
  print(rounds |>
          group_by(regime) |>
          summarise(rounds    = n(),
                    announced = target,
                    perceived = round(mean(centre_B), 2),
                    survey    = round(mean(centre_S), 2),
                    .groups   = "drop") |>
          as.data.frame(),
        row.names = FALSE)
}

# Table 6.2: the tolerance parameter. Biases are recalibrated for every delta; tau, w and
# E_star stay at their values above.
if (run_delta) {
  cat("\nTable 6.2  Sensitivity to the tolerance parameter (30 starting values per fit) ...\n")
  table_6_2_delta <- baseline_rows
  for (multiplier in c(0.5, 0.75, 1.25, 1.5)) {
    variation <- sprintf("delta = %s (%+d%%)",
                         format(round(multiplier * delta, 4), nsmall = 4),
                         round(100 * (multiplier - 1)))
    table_6_2_delta <- bind_rows(table_6_2_delta,
                                 both_models(variation, base_score,
                                             delta_used = multiplier * delta))
  }
  #CHECK: delta -50%: gamma 3.043 / 2.511, RMSFE ELB 0.485 / 0.297
  #CHECK: delta +50%: gamma 2.992 / 2.081, RMSFE ELB 0.373 / 0.298
  show_variations(table_6_2_delta)
}

# Table 6.3: the performance score. Under the saturating score: -MSFE as the alternative,
# tau varied with w following Equation 5.6, w varied with tau fixed. Under -MSFE, tau and w
# play no role, so the only alternative is the saturating score with the constants above.
if (run_score) {
  if (score == "saturating") {
    cat("\nTable 6.3  Sensitivity of the performance score (30 starting values per fit) ...\n")
    table_6_3_score <- bind_rows(baseline_rows,
                                 both_models("-MSFE", score_settings(tau, w, msfe = TRUE)))
    for (multiplier in c(0.5, 0.75, 1.25, 1.5, 2)) {
      tau_varied <- multiplier * tau
      w_follows  <- 1 / (E_star^2 + tau_varied^2)
      variation  <- sprintf("tau = %s (%+d%%)", format(round(tau_varied, 4), nsmall = 4),
                            round(100 * (multiplier - 1)))
      table_6_3_score <- bind_rows(table_6_3_score,
                                   both_models(variation,
                                               score_settings(tau_varied, w_follows)))
    }
    for (multiplier in c(0, 0.5, 0.75, 1.25, 1.5, 2)) {
      variation <- sprintf("w = %s (%+d%%)", format(round(multiplier * w, 4), nsmall = 4),
                           round(100 * (multiplier - 1)))
      table_6_3_score <- bind_rows(table_6_3_score,
                                   both_models(variation, score_settings(tau, multiplier * w)))
    }
    #CHECK: -MSFE gamma 0.676 / 0.381, RMSFE ELB 0.440 / 0.285; w = 0: gamma 3.900 / 3.353
  } else {
    cat("\nTable 6.3  The saturating score as the alternative to -MSFE",
        "(30 starting values per fit) ...\n")
    variation <- sprintf("Saturating score, tau = %s, w = %s",
                         format(tau, nsmall = 4), format(w, nsmall = 4))
    table_6_3_score <- bind_rows(baseline_rows,
                                 both_models(variation, score_settings(tau, w)))
  }
  show_variations(table_6_3_score)
}

# Table 6.4: the calibration of the biases. The rows are those of the thesis in every run:
# the statistic (median or mean), the rounds the constant biases are calibrated on (complete
# sample or pre ELB) and real-time biases, where the bias of a round uses the forecasts of
# all earlier rounds. Real-time biases need `horizon` earlier rounds, so they are compared
# with the baseline on the rounds that remain. A row that has the biases of the run reuses
# its estimates. A bias typed in Part I keeps its value in every row.

# The biases of both models under one calibration: one row per round and model. A bias is
# NA where it cannot be calibrated.
biases_of <- function(statistic, window) {
  paths <- NULL
  for (model in models) {
    biases <- calibrate_biases(classify_panel(model, delta), statistic, window,
                               bias_up[[model]], bias_down[[model]])
    paths  <- bind_rows(paths, data.frame(model = model, statistic = statistic,
                                          biases$by_round))
  }
  paths
}

# The first bias that cannot be calibrated, for the warnings below
describe_gap <- function(gaps) {
  if (is.na(gaps$b_up[1])) {
    sprintf("Model %s has no hawk bias, because no forecast lies above the targeter band",
            gaps$model[1])
  } else {
    sprintf("Model %s has no dove bias, because no forecast lies below the targeter band",
            gaps$model[1])
  }
}

# The biases typed in Part I, for a line above Tables 6.4 and J.1
typed <- c(sprintf("bias_up of Model %s (%.3f)", models, bias_up[models]),
           sprintf("bias_down of Model %s (%.3f)", models, bias_down[models]))
typed <- typed[!is.na(c(bias_up[models], bias_down[models]))]
typed_note <- paste0("Typed in Part I and kept in every row: ",
                     paste(typed, collapse = ", "), ".\n")

if (run_biases) {
  cat("\nTable 6.4  Sensitivity to the calibration of the biases",
      "(30 starting values per fit) ...\n")

  # Both models with one calibration of the biases, and the number of scored rounds
  with_biases <- function(variation, statistic, window, same_rounds = FALSE) {
    rows <- NULL
    for (model in models) {
      if (statistic == bias_statistic && window == bias_window && !same_rounds) {
        model_fit <- fit[[model]]   # the biases of the run
      } else {
        model_fit <- fit_model(model, base_score, starts_side, statistic = statistic,
                               window = window, same_rounds = same_rounds)
      }
      rows <- bind_rows(rows, summarise_fit(model_fit, variation) |>
                                mutate(rounds = nrow(model_fit$results)))
    }
    rows
  }

  # Constant biases: the statistic and the rounds they are calibrated on
  table_6_4_biases <- bind_rows(
    with_biases("Baseline, medians, complete sample", "median", "complete"),
    with_biases("Means, complete sample",             "mean",   "complete"))
  gaps <- biases_of("median", "pre ELB") |> filter(is.na(b_up) | is.na(b_down))
  if (nrow(gaps) == 0) {
    table_6_4_biases <- bind_rows(
      table_6_4_biases,
      with_biases("Medians, pre ELB sample", "median", "pre ELB"))
  } else {
    warning(describe_gap(gaps), " in the pre ELB rounds. The row \"Medians, pre ELB sample\" ",
            "of Table 6.4 is left out.", call. = FALSE)
  }

  # Real-time biases of both models, with the median and with the mean. The first `horizon`
  # rounds have none. If a later round has none either, the real-time rows are left out.
  real_time_biases <- bind_rows(biases_of("median", "real time"),
                                biases_of("mean", "real time")) |>
    filter(round > rounds$round[horizon])
  gaps         <- real_time_biases |> filter(is.na(b_up) | is.na(b_down))
  real_time_ok <- nrow(gaps) == 0
  if (real_time_ok) {
    table_6_4_biases <- bind_rows(
      table_6_4_biases,
      with_biases("Baseline, same rounds", "median", "complete", same_rounds = TRUE),
      with_biases("Medians, real time",    "median", "real time"),
      with_biases("Means, real time",      "mean",   "real time"))
  } else {
    warning(describe_gap(gaps), " before ", gaps$round[1], ", so that round has no real-time ",
            "bias. The real-time rows of Table 6.4 are left out.", call. = FALSE)
  }

  #CHECK: means: gamma 3.054 / 1.667, RMSFE ELB 0.370 / 0.286, gain 49% / 11%
  #CHECK: pre ELB medians: gamma 3.836 / 2.360, RMSFE ELB 0.486 / 0.308, gain 33% / 5%
  #CHECK: same rounds: gamma 2.998 / 2.257, RMSFE ELB 0.445 / 0.297, gain 39% / 8%
  #CHECK: real-time medians: gamma 4.085 / 2.467, RMSFE ELB 0.425 / 0.304, gain 42% / 6%
  cat("Biases of this run: ", bias_statistic, "s, ", bias_window, " sample",
      if (bias_statistic == "mean" && bias_window == "pre ELB") " (not a row of this table)",
      ".\n", sep = "")
  if (length(typed) > 0) cat(typed_note)
  scored_rounds <- table_6_4_biases |> filter(model == "A") |> select(rounds, variation)
  print(left_join(scored_rounds, layout_variations(table_6_4_biases), by = "variation"),
        row.names = FALSE)
}

# Table J.1: the biases under each calibration (Appendix J), in every run. The real-time
# biases change from round to round. They are averaged by regime over the rounds that have
# one, and they are part of the table only if Table 6.4 has run.
constant_biases <- function(variation, statistic, window) {
  biases_of(statistic, window) |>
    group_by(model) |>
    summarise(variation = variation, regime = "constant",
              hawk = first(b_up), dove = first(b_down), .groups = "drop")
}
biases_long <- bind_rows(
  constant_biases("Baseline, medians, complete sample", "median", "complete"),
  constant_biases("Means, complete sample",             "mean",   "complete"),
  constant_biases("Medians, pre ELB sample",            "median", "pre ELB"))
if (run_biases && real_time_ok) {
  real_time_average <- function(variation, statistic_used) {
    real_time_biases |>
      filter(statistic == statistic_used) |>
      group_by(model, regime) |>
      summarise(variation = variation, hawk = mean(b_up), dove = mean(b_down),
                .groups = "drop")
  }
  biases_long <- bind_rows(biases_long,
                           real_time_average("Medians, real time", "median"),
                           real_time_average("Means, real time",   "mean"))
}

# Model A and the second model side by side
table_j_1_biases <- left_join(biases_long |> filter(model == "A")    |> select(-model),
                              biases_long |> filter(model == second) |> select(-model),
                              by = c("variation", "regime"),
                              suffix = paste0("_", models)) |>
  arrange(match(variation, unique(biases_long$variation)),
          match(regime, c("constant", REGIMES))) |>
  as.data.frame()
#CHECK: means A 1.031 / 0.689, B 0.754 / 0.653; pre ELB medians A 0.400 / 0.500, B 0.407 / 0.556
#CHECK: real-time medians at the ELB A 0.400 / 0.591, B 0.410 / 0.552
#CHECK: real-time means at the ELB A 0.470 / 0.681, B 0.463 / 0.661
cat("\nTable J.1  The biases under each calibration",
    "(real time: mean over the rounds of the regime)\n")
if (length(typed) > 0) cat(typed_note)
print(table_j_1_biases |> mutate(across(where(is.double), three)), row.names = FALSE)


# Part IX: Comparison with the baseline -----------------------------------

# 1. Parameters that differ from the thesis baseline
changed <- names(baseline)[!mapply(identical, this_run, baseline)]
cat("\nComparison with the thesis baseline\n")
if (length(changed) == 0) {
  cat("No parameter differs from the thesis baseline.\n")
} else {
  print(data.frame(parameter = changed,
                   thesis    = sapply(baseline[changed], toString),
                   this_run  = sapply(this_run[changed], toString)),
        row.names = FALSE)
}

# 2. Table 5.6 of this run next to the thesis values (the thesis compares Models A and B)
comparison <- table_5_6_gain |>
  mutate(model = ifelse(model == "S", "B", model)) |>
  select(regime, model, centre_only, complete, gain_pct) |>
  left_join(thesis_table_5_6_gain, by = c("regime", "model"), suffix = c("", "_thesis")) |>
  mutate(match = round(centre_only, 3) == centre_only_thesis &
                 round(complete, 3) == complete_thesis &
                 round(gain_pct) == gain_pct_thesis) |>
  transmute(regime, model,
            centre_only     = three(centre_only),
            thesis_centre   = three(centre_only_thesis),
            complete        = three(complete),
            thesis_complete = three(complete_thesis),
            gain_pct        = pct(gain_pct),
            thesis_gain     = pct(gain_pct_thesis),
            match)
if (second == "S") {
  cat("(The second model is Model S here; the thesis column shows Model B.)\n")
}
if (score == "msfe") {
  cat("(The thesis values rest on the saturating score; differences are expected under -MSFE.)\n")
}
if (bias_statistic != "median" || bias_window != "complete") {
  cat("(The thesis values rest on medians over the complete sample; differences are expected.)\n")
}
print(comparison, row.names = FALSE)
if (all(comparison$match)) {
  cat("Table 5.6 matches the thesis exactly.\n")
} else {
  cat("Table 5.6 differs from the thesis.\n")
}


# Part X: Figures ---------------------------------------------------------

# Style of the thesis figures: serif font (Times New Roman in Word), white panels, light
# grid, legend below. Model A blue, Model B orange, Model S navy; hawks green, targeters
# navy, doves red; the ELB regime shaded grey.
FONT         <- "serif"
MODEL_COLOUR <- c(A = "#2B7BD6", B = "#E8673A", S = "#1F3A5F")
TYPE_COLOUR  <- c(Hawks = "#6B8E4E", Targeters = "#1F3A5F", Doves = "#C0392B")
TYPE_FILL    <- c(Hawks = "#BCCCAF", Targeters = "#9AA6B7", Doves = "#E3A6A0")   # lighter tints
RED          <- "#C0392B"

thesis_theme <- theme_bw(base_size = 10, base_family = FONT) +
  theme(text             = element_text(family = FONT, colour = "black"),
        axis.text        = element_text(colour = "black", size = 9),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey88", linewidth = 0.25),
        panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.4),
        legend.position  = "bottom",
        legend.title     = element_blank(),
        legend.text      = element_text(size = 9),
        strip.background = element_blank(),
        strip.text       = element_text(size = 10, hjust = 0))

# The ELB regime as dates, for the shading
round_to_date <- function(round) {
  as.Date(paste(substr(round, 1, 4), 3 * as.integer(substr(round, 6, 6)) - 2, "01", sep = "-"))
}
elb_start <- round_to_date(elb_from)
elb_end   <- seq(round_to_date(elb_to), by = "3 months", length.out = 2)[2] - 1
shade_elb <- annotate("rect", xmin = elb_start, xmax = elb_end, ymin = -Inf, ymax = Inf,
                      fill = "grey75", alpha = 0.4)
two_year_axis <- scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                              expand = expansion(c(0.01, 0.01)))

# Labels of the models, of their centres and of the three types
model_label  <- c(A = "Model A, announced target",
                  B = "Model B, perceived anchor",
                  S = "Model S, survey anchor")
centre_label <- c(A = paste0("Announced target, ", format(target), "%"),
                  B = "Perceived anchor",
                  S = "Survey anchor")
type_label   <- c(hawk = "Hawks", targeter = "Targeters", dove = "Doves")

# Figure 4.1: every forecast and the tolerance band around the two centres. The points are
# circles with a ring in the colour of their type and a lighter fill, so that dense clouds
# of points stay readable.
points_4_1 <- classified |>
  mutate(type  = factor(type_label[type], levels = type_label),
         facet = factor(centre_label[model], levels = centre_label[models]))
band_4_1 <- points_4_1 |> distinct(facet, survey_date, centre)

fig_4_1_forecasts <- ggplot() +
  geom_point(data = points_4_1, aes(survey_date, forecast, colour = type, fill = type),
             shape = 21, size = 1.2, stroke = 0.3) +
  geom_line(data = band_4_1, aes(survey_date, centre + delta), colour = "black", linewidth = 0.35) +
  geom_line(data = band_4_1, aes(survey_date, centre - delta), colour = "black", linewidth = 0.35) +
  geom_line(data = band_4_1, aes(survey_date, centre), colour = RED, linewidth = 0.5) +
  facet_wrap(~ facet, ncol = 1) +
  scale_colour_manual(values = TYPE_COLOUR) +
  scale_fill_manual(values = TYPE_FILL) +
  guides(colour = guide_legend(override.aes = list(size = 2.2))) +
  two_year_axis +
  labs(x = NULL, y = "Expected inflation (%)",
       title = "Figure 4.1  Individual point forecasts and the tolerance band around the centres",
       caption = "Black lines: centre +/- delta. Red line: centre.") +
  thesis_theme

# Figure 4.2: the share composition under both centres. geom_area stacks the first level
# on top, so hawks lie above and doves below.
shares_long <- bind_rows(
  shares |> transmute(model, date, type = "Hawks",     share = hawk),
  shares |> transmute(model, date, type = "Targeters", share = targeter),
  shares |> transmute(model, date, type = "Doves",     share = dove)) |>
  mutate(type  = factor(type, levels = names(TYPE_COLOUR)),
         facet = factor(centre_label[model], levels = centre_label[models]))

fig_4_2_shares <- ggplot(shares_long, aes(date, share, fill = type)) +
  geom_area(alpha = 0.9, colour = NA) +
  geom_vline(xintercept = c(elb_start, elb_end), colour = "black", linetype = "dotted",
             linewidth = 0.4) +
  facet_wrap(~ facet, ncol = 1) +
  scale_fill_manual(values = TYPE_COLOUR, breaks = names(TYPE_COLOUR)) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100 * x), expand = expansion(c(0, 0))) +
  two_year_axis +
  labs(x = NULL, y = "Share of respondents",
       title = "Figure 4.2  Evolution of the share composition for both centres",
       caption = "Dotted lines: the ELB regime.") +
  thesis_theme

# Figure 4.3: the targeter share around both centres
fig_4_3_targeters <- ggplot(mapping = aes(date, targeter)) +
  shade_elb +
  geom_line(data = shares |> filter(model == "A"),
            aes(colour = centre_label[["A"]]), linewidth = 0.5, linetype = "22") +
  geom_line(data = shares |> filter(model == second),
            aes(colour = centre_label[[second]]), linewidth = 0.65) +
  scale_colour_manual(values = setNames(c("grey40", "#1F3A5F"), centre_label[models])) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100 * x), limits = c(0, 1)) +
  two_year_axis +
  labs(x = NULL, y = "Targeter share", title = "Figure 4.3  Degree of anchoring for both centres") +
  thesis_theme

# Figure 5.1: the three heuristics of each model
heuristics <- bind_rows(
  results |> transmute(model, date, heuristic = "Hawk heuristic",     value = upper),
  results |> transmute(model, date, heuristic = "Targeter heuristic", value = centre),
  results |> transmute(model, date, heuristic = "Dove heuristic",     value = lower)) |>
  mutate(heuristic = factor(heuristic, levels = unique(heuristic)),
         model     = factor(model_label[model], levels = model_label[models]))

fig_5_1_heuristics <- ggplot(heuristics,
                             aes(date, value, colour = heuristic, linetype = heuristic)) +
  shade_elb +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ model, ncol = 1) +
  scale_colour_manual(values = c("#008300", "#4a3aa7", "#e34948")) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted")) +
  two_year_axis +
  labs(x = NULL, y = "% per year", title = "Figure 5.1  Heuristic forecasts for both centres") +
  thesis_theme

# Figure 5.2: profile log-likelihood of each parameter, relative to the maximum and divided
# by the dispersion factor. Values above -1.92 lie inside the 95% interval.
cat("\nDrawing Figure 5.2 (profile log-likelihoods) ...\n")
gamma_grid <- seq(0.2 * min(from_fits("gamma")), 2 * max(from_fits("gamma")),
                  length.out = 120)                       # follows the estimates
rho_grid   <- seq(0, 0.99, by = 0.01)
PROFILE_LABEL <- c("Intensity of choice, gamma", "Memory, rho")

# The two profiles of one model: the highest log-likelihood when one parameter is fixed
# on a grid and the other one is re-estimated
profile_of <- function(model_fit) {
  loglik <- function(gamma, rho) -minus_loglik(c(gamma, rho), model_fit, base_score)
  profile_gamma <- sapply(gamma_grid, function(gamma) {
    optimize(function(rho) loglik(gamma, rho), c(0, 0.998), maximum = TRUE)$objective
  })
  profile_rho <- sapply(rho_grid, function(rho) {
    optimize(function(gamma) loglik(gamma, rho), c(0, 50), maximum = TRUE)$objective
  })
  bind_rows(data.frame(model     = model_label[[model_fit$model]],
                       parameter = PROFILE_LABEL[1],
                       x         = gamma_grid,
                       y         = (profile_gamma - model_fit$loglik) / model_fit$phi),
            data.frame(model     = model_label[[model_fit$model]],
                       parameter = PROFILE_LABEL[2],
                       x         = rho_grid,
                       y         = (profile_rho - model_fit$loglik) / model_fit$phi))
}
profiles <- NULL
for (model in models) profiles <- bind_rows(profiles, profile_of(fit[[model]]))
profiles <- profiles |> filter(y >= -8)

# The estimates sit at the maximum of each profile. Their values are printed above the dots,
# the left-hand estimate to the left of its dot and the right-hand one to the right, so the
# two labels never overlap and never cover a curve.
estimate_points <- bind_rows(
  data.frame(model = model_label[models], parameter = PROFILE_LABEL[1], x = from_fits("gamma")),
  data.frame(model = model_label[models], parameter = PROFILE_LABEL[2], x = from_fits("rho"))) |>
  mutate(y = 0) |>
  group_by(parameter) |>
  mutate(label = sprintf("%.3f", x),
         hjust = if_else(rank(x, ties.method = "first") == 1, 1.1, -0.1)) |>
  ungroup()

fig_5_2_profiles <- ggplot(profiles, aes(x, y, colour = model)) +
  geom_hline(yintercept = -1.92, colour = RED, linetype = "dashed", linewidth = 0.5) +
  geom_line(linewidth = 0.6) +
  geom_point(data = estimate_points, aes(x, y, fill = model), shape = 21, colour = "white",
             size = 2.4) +
  geom_text(data = estimate_points, aes(x, y, label = label, hjust = hjust), vjust = -0.7,
            size = 3, family = FONT, show.legend = FALSE) +
  facet_wrap(~ parameter, scales = "free_x") +
  scale_colour_manual(values = unname(MODEL_COLOUR[models]), breaks = unname(model_label[models])) +
  scale_fill_manual(values = unname(MODEL_COLOUR[models]), breaks = unname(model_label[models]),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = 0.15)) +
  scale_y_continuous(limits = c(-8, 0.9), breaks = seq(-8, 0, 2)) +
  labs(x = NULL, y = "Log-likelihood relative to maximum / phi",
       title = "Figure 5.2  Estimated behavioural parameters: profile log-likelihoods",
       caption = "Dashed red line: 95% threshold, -1.92. Dots: the estimates, with their values.") +
  thesis_theme

# Figure 5.3: the aggregate expectation of each model and the SPF mean. The dotted line is
# the lowest expectation Model A can produce (its dove heuristic).
expectations <- bind_rows(
  results |>
    filter(model == "A") |>
    transmute(date, series = "SPF mean forecast", value = spf_mean),
  results |>
    transmute(date, series = model_label[model], value = expectation)) |>
  mutate(series = factor(series, levels = c("SPF mean forecast", model_label[models])))

fig_5_3_expectations <- ggplot(expectations, aes(date, value, colour = series)) +
  shade_elb +
  geom_hline(yintercept = target - fit$A$b_down, colour = RED, linetype = "dotted",
             linewidth = 0.5) +
  geom_line(linewidth = 0.6) +
  scale_colour_manual(values = c("black", unname(MODEL_COLOUR[models]))) +
  two_year_axis +
  labs(x = NULL, y = "% per year",
       title = "Figure 5.3  Aggregate model expectations and the SPF mean",
       caption = sprintf("Dotted line: lowest attainable expectation of Model A, %.1f%%.",
                         target - fit$A$b_down)) +
  thesis_theme

# Figure 5.4: the SPF mean relative to the attainable range of each model
ranges <- results |>
  mutate(model = factor(model_label[model], levels = model_label[models]))

fig_5_4_range <- ggplot(ranges, aes(date)) +
  shade_elb +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.18) +
  geom_line(aes(y = centre, colour = model), linetype = "dashed", linewidth = 0.4) +
  geom_line(aes(y = expectation, colour = model), linewidth = 0.6) +
  geom_point(aes(y = spf_mean), size = 0.9) +
  facet_wrap(~ model, ncol = 1) +
  scale_colour_manual(values = unname(MODEL_COLOUR[models]), breaks = unname(model_label[models])) +
  scale_fill_manual(values = unname(MODEL_COLOUR[models]), breaks = unname(model_label[models])) +
  two_year_axis +
  labs(x = NULL, y = "% per year",
       title = "Figure 5.4  The SPF mean relative to the attainable ranges",
       caption = paste("Band: attainable range [centre - dove bias, centre + hawk bias].",
                       "Dashed: centre. Solid: model expectation. Dots: SPF mean.")) +
  thesis_theme +
  theme(legend.position = "none")

# Figures J.1 and J.2: the real-time hawk bias and dove bias of both models (Appendix J),
# drawn if Table 6.4 has run. A bias changes only when a new round is observed, so the
# lines are steps.
if (run_biases && real_time_ok) {
  bias_paths <- real_time_biases |>
    mutate(model     = factor(model_label[model], levels = model_label[models]),
           statistic = factor(if_else(statistic == "median", "Median", "Mean"),
                              levels = c("Median", "Mean")))
  bias_layers <- list(
    shade_elb,
    geom_step(linewidth = 0.6),
    facet_wrap(~ model, ncol = 1),
    scale_colour_manual(values = unname(MODEL_COLOUR[models]), guide = "none"),
    scale_linetype_manual(values = c(Median = "solid", Mean = "22")),
    two_year_axis,
    thesis_theme)

  fig_j_1_hawk_bias <- ggplot(bias_paths,
                              aes(date, b_up, colour = model, linetype = statistic)) +
    bias_layers +
    labs(x = NULL, y = "Hawk bias (pp)", title = "Figure J.1  Real-time hawk bias",
         caption = "The bias of a round uses the forecasts of all earlier rounds.")

  fig_j_2_dove_bias <- ggplot(bias_paths,
                              aes(date, b_down, colour = model, linetype = statistic)) +
    bias_layers +
    labs(x = NULL, y = "Dove bias (pp)", title = "Figure J.2  Real-time dove bias",
         caption = "The bias of a round uses the forecasts of all earlier rounds.")
}

# Draw every figure (use the arrows of the Plots pane to move between them)
print(fig_4_1_forecasts)
print(fig_4_2_shares)
print(fig_4_3_targeters)
print(fig_5_1_heuristics)
print(fig_5_2_profiles)
print(fig_5_3_expectations)
print(fig_5_4_range)
if (run_biases && real_time_ok) {
  print(fig_j_1_hawk_bias)
  print(fig_j_2_dove_bias)
}

cat("\nDone.\n")
