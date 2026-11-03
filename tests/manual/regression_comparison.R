# Regression and validation tests for the refactored AK Tier 3 scenario script.
# Requires: refactored script sourced first; r4ss and data.table installed.
# Run with two clean independent copies of the same PROJ input directory,
# one for legacy and one for refactored runs. Do not run both on the same folder.

AK_compare_outputs <- function(legacy, revised, tolerance = 1e-6,
                               relative = FALSE, stop_on_failure = TRUE) {
  # legacy / revised are saved objects from both versions of the function.
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table required")
  names_to_check <- c("SSB", "CATCH", "Five_year")
  results <- list()
  for (table_name in names_to_check) {
    a <- data.table::as.data.table(legacy[[table_name]])
    b <- data.table::as.data.table(revised[[table_name]])
    if (is.null(a) || is.null(b)) stop("Missing output table ", table_name)
    keys <- if (table_name == "Five_year") "Yr" else c("model", "Yr")
    value_cols <- intersect(names(a), names(b))
    value_cols <- setdiff(value_cols, keys)
    if (nrow(a) != nrow(b) || !setequal(names(a), names(b))) {
      stop(table_name, ": different row count or column names")
    }
    data.table::setorderv(a, keys)
    data.table::setorderv(b, keys)
    if (!identical(as.data.frame(a[, ..keys]), as.data.frame(b[, ..keys])))
      stop(table_name, ": keys do not align")
    for (col in value_cols) {
      aa <- a[[col]]; bb <- b[[col]]
      if (is.numeric(aa) && is.numeric(bb)) {
        delta <- abs(aa - bb)
        scale <- if (relative) pmax(1, abs(aa), abs(bb)) else 1
        bad <- (is.na(aa) != is.na(bb)) | (!is.na(delta) & delta > tolerance * scale)
        maxdiff <- if (all(is.na(delta))) NA_real_ else max(delta, na.rm = TRUE)
      } else {
        bad <- (is.na(aa) != is.na(bb)) | (!is.na(aa) & !is.na(bb) & aa != bb)
        maxdiff <- NA_real_
      }
      results[[length(results) + 1L]] <- data.frame(table = table_name,
        column = col, discrepancies = sum(bad), max_abs_diff = maxdiff)
    }
  }
  out <- do.call(rbind, results)
  if (stop_on_failure && any(out$discrepancies)) {
    print(out[out$discrepancies > 0, ])
    stop("Regression comparison detected nonmatching fields")
  }
  out
}

AK_test_scenario_keys <- function(root, CYR, fleets = 1, n_forecast = 5L) {
  # Verify forecast inputs that were actually written, not just R tables.
  if (!requireNamespace("r4ss", quietly = TRUE)) stop("r4ss required")
  data <- list()
  for (n in 1:15) {
    loc <- file.path(root, paste0("scenario_", n))
    if (!file.exists(file.path(loc, "forecast.ss"))) stop("Missing forecast: ", loc)
    f <- r4ss::SS_readforecast(file.path(loc, "forecast.ss"))
    fc <- f$ForeCatch
    if (!is.null(fc) && nrow(as.data.frame(fc))) {
      d <- .ak_check_forecatch(fc, fleets = fleets,
                               label = paste0("scenario_", n))
      if (any(d$Year < CYR)) warning("Scenario ", n, " has specified catch before CYR")
    }
    data[[n]] <- data.frame(scenario = n, reference_B = f$Btarget,
                            reference_SPR = f$SPRtarget,
                            n_fixed_entries = if (is.null(fc)) 0 else nrow(as.data.frame(fc)))
  }
  do.call(rbind, data)
}

# Example workflow (run in an R environment with models and SS installed):
# library(AKScenariosForSS)
# original <- readRDS("legacy_results.rds")
# revised <- readRDS("refactored_results.rds")
# comparison <- AK_compare_outputs(original, revised, tolerance = 1e-6)
# write.csv(comparison, "AK_projection_regression_comparison.csv", row.names = FALSE)
# forecast_checks <- AK_test_scenario_keys("Model_26.1_750/PROJ", CYR = 2026, fleets = 1:2)
