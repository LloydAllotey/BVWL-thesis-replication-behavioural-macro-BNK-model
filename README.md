# Thesis_Replication — Imperfect Credibility at the Effective Lower Bound

Written by: Lloyd Allotey, November 2026  

This repository holds the code that reproduces the main results of the empirical chapters of my
bachelor thesis *Imperfect Credibility at the Effective Lower Bound: A Behavioural Macroeconomic
Analysis of Eurozone Inflation Expectations*.

> To what extent does a time varying perceived inflation anchor improve the ability of the
> expectations block of a Behavioural New Keynesian model to explain euro area inflation
> expectations, particularly at the effective lower bound, and does any potential gain stem from
> the anchor itself or from agents switching between forecasting rules?

Two of the four raw sources are Bloomberg exports. Bloomberg data may not be passed on, so these
two files and the database built from them are absent here. Everything else is in the repository,
and with the two exports in place the analysis runs as described below (see [Data](#data)).

## Run the analysis

1. Clone the repository, or download it as a ZIP and extract it.
2. Save the two Bloomberg exports in `data/raw/ils_5y5y/` and `data/raw/hicp/` (see [Data](#data)).
3. Open `Thesis_Replication.Rproj` in RStudio.
4. In the console, once:

   ```r
   renv::restore(prompt = FALSE)
   ```

   This installs the packages, about a minute. No Rtools or Xcode is needed.
5. Build the database, once:

   ```r
   source("build_database.R")
   ```

   Under a minute.
6. Then:

   ```r
   source("main.R")
   ```

   About two and a half minutes. Tables appear in the console, figures in the Plots pane.

From a terminal, in the folder that holds `main.R`: `Rscript -e "renv::restore(prompt = FALSE)"`,
then `Rscript build_database.R` and `Rscript main.R` (the figures then go to `Rplots.pdf`).

Without renv: `install.packages(c("DBI", "duckdb", "dplyr", "ggplot2", "readxl"))`, delete
`.Rprofile`, set the working directory to this folder and run the two scripts.

## What main.R does

- Reads `data/thesis.duckdb`, built from the raw files by `build_database.R`.
- Classifies every one-year-ahead SPF forecast, 2004Q3 to 2026Q2, as hawk, targeter or dove
  around two centres. Model A uses the announced target of 2%, Model B the perceived anchor from
  the 5y5y inflation-linked swap rate.
- Estimates the intensity of choice gamma and the memory rho by maximum likelihood.
- Evaluates both models against the SPF mean, over the full sample and by regime.
- Runs the side analyses of Chapters 5 and 6, each behind a switch.

Every table and figure stays in the workspace as an object named after its thesis number, for
example `table_5_6_gain` or `fig_5_2_profiles`.

## Changing parameters

All parameters sit in Part I of `main.R`, each with the thesis value in a comment. Change one,
run the script again, and every table and figure follows.

- `anchor`: `"ILS"` (Model B) or `"LTSPF"` (Model S as the second model).
- `score`: `"saturating"` (thesis) or `"msfe"` (De Grauwe's squared error).
- `bias_statistic`: `"median"` (thesis) or `"mean"` of the biases.
- `bias_window`: biases calibrated on the `"complete"` sample (thesis) or on the `"pre ELB"` rounds.
- `target`: the announced target.
- `first_round`, `last_round`, `bench_from`, `bench_to`, `elb_from`, `elb_to`: the three windows.
- `horizon`: rounds between a forecast and its evaluation.
- `delta`, `E_star`, `tau`, `w`: `NA` calibrates the constant from the data (thesis). A number
  overrides that one constant, all else equal.
- `bias_up`, `bias_down`: `NA` calibrates the bias of a model, a number fixes it.
- `quick_mode`: `TRUE` (set here) uses 30 starting values, `FALSE` the 300 of the thesis. The
  estimates are the same.
- `run_feasibility`, `run_anchors`, `run_delta`, `run_score`, `run_biases`: the side analyses.

At the end the script lists the parameters that differ from the thesis and checks Table 5.6
against the thesis values.

## The database

```r
source("build_database.R")
```

The script reads the raw files in `data/raw/` and writes `data/thesis.duckdb`. Every raw table is
stored unchanged and every cleaning step is saved as its own table. Run it again whenever a raw
file is replaced. Open the database from one R session at a time.

## Folder

```
├── main.R                         # the analysis, read top to bottom
├── build_database.R               # raw files to data/thesis.duckdb
├── Thesis_Replication.Rproj
├── renv.lock, renv/, .Rprofile    # package versions, restored by renv
├── .renvignore                    # keeps the guide's rendering packages out of the lockfile
├── data/
│   ├── raw/
│   │   ├── spf/                   # ECB SPF, one csv per round, 1999Q1 to 2026Q3
│   │   ├── ils_5y5y/              # Bloomberg FWISEU55 Index, monthly (file to be added)
│   │   ├── hicp/                  # Bloomberg ECCPEMUY Index, monthly (file to be added)
│   │   └── ecb_dfr/               # ECB key interest rates, csv plus SOURCE.txt
│   └── thesis.duckdb              # 13 tables, written by build_database.R
└── notebooks/
    └── user_guide.qmd / .html     # the user guide
```

## Data

| Source | What | Use | In the repository |
|--------|------|-----|-------------------|
| ECB [Survey of Professional Forecasters](https://www.ecb.europa.eu/stats/ecb_surveys/survey_of_professional_forecasters/html/index.en.html) | Individual point forecasts of HICP inflation, one csv per round | Forecasts classified as hawks, targeters, doves; five-year-ahead mean as the anchor of Model S | Yes |
| Bloomberg `FWISEU55 Index` | 5y5y forward inflation-linked swap rate, monthly | Perceived anchor of Model B | No |
| Bloomberg `ECCPEMUY Index` | Euro area HICP inflation, year on year, monthly | Realised inflation each heuristic is scored against | No |
| ECB [Key ECB interest rates](https://www.ecb.europa.eu/stats/policy_and_exchange_rates/key_ecb_interest_rates/html/index.en.html) | Deposit facility rate, one row per change | Dates the ELB regime | Yes |

Source of the SPF files and of the key interest rates: European Central Bank.
`data/raw/ecb_dfr/SOURCE.txt` states how the interest rate table was prepared.

The two Bloomberg files come from a Bloomberg terminal, each as a monthly history exported to
Excel. `build_database.R` expects the layout of that export, five lines of metadata followed by
the columns `Date` and `PX_LAST`, for the swap rate also `PX_BID`.

- `FWISEU55 Index`, June 2004 to June 2026, saved as an .xlsx file in `data/raw/ils_5y5y/`.
- `ECCPEMUY Index`, December 1996 to June 2026, saved as an .xlsx file in `data/raw/hicp/`.

The file names are free, the script takes the .xlsx file it finds in each folder.

The estimation window has 88 rounds (40 pre ELB, 32 ELB, 16 post ELB) and 4'173 forecasts from 102
forecasters. The stored panel also holds the 41 forecasts of 2026Q3, after the thesis cut-off.

## User guide

`notebooks/user_guide.html` lists every parameter and function and shows each table and figure
with an example. To render it again, run `renv::install(c("knitr", "rmarkdown"))` once and then
`quarto render user_guide.qmd` in the `notebooks/` folder.

The guide was written for the course version of this folder, which came as an archive with the
database inside. Where it says that the database ships ready, build it first as described above.

## Use of AI tools

The R scripts implement my preliminary analysis in Microsoft Excel and were developed with the
assistance of Claude Code (Anthropic). I read and verified the scripts and their results. The
thesis lists all tools in its Declaration of Aids.
