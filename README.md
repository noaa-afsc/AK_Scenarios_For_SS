# AKScenariosForSS

Alaska Tier 3 harvest projection scenarios using Stock Synthesis 3 and `r4ss`.

## What's included

- `Do_AK_TIER_3_Scenarios()` runs 15 projection scenarios and returns annual and five-year summary tables.
- Scenarios 2–8 and 9–11 are parallelized where workers are available. Scenarios 9–11 use the catch source selected by `Fyr_ts`.
- Scenarios 9–15 can skip Hessian calculations with `no_hessian_late = TRUE`.
- Spawning-biomass and catch figures include scenarios 1–7 only. Catch figures include horizontal catch-at-F40% and catch-at-F35% reference lines.
- When `Scenario2 == 3` **and** `Fyr_ts == 2`, the scenario 12–15 forecast catches come only from the matching `Scenario2_catch.csv` rows for their horizon; missing fleets/years are not filled.

## Install

This folder is an **R package**. On a computer with R installed:

```r
install.packages(c("remotes", "r4ss", "data.table", "ggplot2", "testthat", "roxygen2"))
remotes::install_local("/path/to/AKScenariosForSS", dependencies = TRUE)
library(AKScenariosForSS)
```

Or, after uploading to GitHub:

```r
remotes::install_github("noaa-afsc/AK_Scenarios_For_SS")
```

SS3 must be available as an executable in the model input folder (or through `exe`). It is **not** installed with this R package.

## Model input folder

Create a dedicated `PROJ` directory containing a **converged** assessment run with its model inputs,`.ctl`,`.dat`, `starter.ss`,`forecast.ss`, and your platform's SS3 executable. The function writes 15 `scenario_*` subdirectories there, overwriting scenario inputs; use a copy of your assessment run.

For Scenario 2 option 3, provide `Scenario2_catch.csv` with `Year,Seas,Fleet,Catch_or_F`. It is searched in your working directory, unless `catch_csv` provides a path.

## Example

```r
library(AKScenariosForSS)
result <- Do_AK_TIER_3_Scenarios(
  DIR = "Model_26_1_2/PROJ",
  CYR = 2026,
  SYR = 1977,
  SEXES = 1,
  FLEETS = 1:2,
  Scenario2 = 3,
  Fyr_ts = 2,
  catch_csv = "Scenario2_catch.csv",
  cores = 8,
  do_fig = TRUE,
  no_hessian_late = TRUE
)

result$Five_year
result$Tables$Catch
result$FIGS[[2]]$ALL  # Catch plot for scenarios 1-7
```


## Integration testing and known limitations

The tests under `tests/testthat` are **offline unit tests**, not model runs. The `tests/manual/regression_comparison.R` script provides result comparisons for a **separate SS3-enabled machine**. Never use the same `PROJ` directory for legacy and refactored tests.

## License

No license has been selected. The `LICENSE` file is a placeholder and must be replaced with the project owner's approved terms before publishing or permitting redistribution. The package metadata should be updated at the same time.
