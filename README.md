# AKScenariosForSS

Alaska Tier 3 harvest projection scenarios using Stock Synthesis 3 and `r4ss`.

> **Validation status:** This package is assembled from `AK_Tier3_Scenarios_refactored_v12.R` without changes to the underlying scenario calculations. The code has **not** been executed against your SS3 model or numerically regression-tested in this environment. Review five-year ABC/OFL values against SS3 outputs before assessment use.

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
remotes::install_github("YOUR_GITHUB_USER/YOUR_REPOSITORY")
```

SS3 must be available as an executable in the model input folder (or through `exe`). It is **not** installed with this R package.

## Model input folder

Create a dedicated `PROJ` directory containing a **converged** assessment run with its model inputs, `ss.par`, `starter.ss` configured to read `ss.par`, `forecast.ss`, and your platform's SS3 executable. The function writes 15 `scenario_*` subdirectories there, overwriting scenario inputs; use a copy of your assessment run.

For Scenario 2 option 3, provide `Scenario2_catch.csv` with `Year,Seas,Fleet,Catch_or_F`. It is searched in your working directory or the parent of `DIR`, unless `catch_csv` provides a path.

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

## Package maintenance

```r
roxygen2::roxygenise("/path/to/AKScenariosForSS")
testthat::test_local("/path/to/AKScenariosForSS")
# Or from the package directory:
# devtools::check()
```

The package includes a manual `man/` help file and `NAMESPACE`, so documentation is available without running roxygen on installation. The roxygen header remains the source of truth.

## Integration testing and known limitations

The tests under `tests/testthat` are **offline unit tests**, not model runs. The `tests/manual/regression_comparison.R` script provides result comparisons for a **separate SS3-enabled machine**. Never use the same `PROJ` directory for legacy and refactored tests.

Scientific checks still required: single-sex SSB division by two, input-basis/F convention for Scenario 2 option 2, annual F and catch labels for multi-fleet output, and five-year ABC/OFL scenario mapping. `init_dir` is retained for backwards-compatible calls but unused.

## License

No license has been selected. The `LICENSE` file is a placeholder and must be replaced with the project owner's approved terms before publishing or permitting redistribution. The package metadata should be updated at the same time.
