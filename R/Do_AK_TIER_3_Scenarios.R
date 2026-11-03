#' Run Alaska Tier 3 Stock Synthesis projection scenarios
#'
#' @description
#' Generate 15 Alaska Tier 3 forecast scenarios from a converged Stock
#' Synthesis 3 (SS3) assessment and assemble spawning biomass, fishing
#' mortality, catch, and five-year ABC/OFL projection summaries.
#'
#' @details
#' Requires a prepared SS3 model directory containing input files,
#' `forecast.ss`, an executable (unless `skip_run = TRUE`), and a starter
#' configuration that loads converged parameters from `ss.par`.
#'
#' Scenario 1 uses F40%; scenario 2 is the selected alternative;
#' scenario 3 changes the averaging period for F; scenario 4 scales
#' the limit fraction; scenario 5 uses zero catch; scenario 6 uses F35%;
#' scenarios 7 and 8 use F35% with forecast catches derived from scenario 1.
#' Scenarios 9–11 use F35% and independently take fixed catches from
#' the scenario chosen by `Fyr_ts`. Scenarios 12–15 build the remaining
#' five-year ABC/spawning-biomass projections.
#'
#' Scenarios 2–8 and 9–11 can run in parallel. With `no_hessian_late = TRUE`,
#' scenarios 9–15 skip Hessian calculations. Figures cover only scenarios
#' 1–7. For `Scenario2 = 3` and `Fyr_ts = 2`, the specified catch table
#' for scenarios 12–15 includes **only** matching records present in the CSV;
#' missing fleets and years are left unspecified.
#'
#' Scenario runs create and overwrite `scenario_1` through `scenario_15`
#' subdirectories of `DIR`. Use a copy of the converged model directory.
#'
#' @param DIR Character path to the SS3 model projection input directory.
#' @param CYR Integer, current assessment year.
#' @param SYR Integer, first assessment model year.
#' @param SEXES Integer, number of sexes in the model (1 or 2). For
#'   `SEXES = 1`, the function retains the original division of SSB by 2;
#'   users must verify that this convention applies to their model.
#' @param FLEETS Integer vector of fishery fleet IDs.
#' @param Scenario2 Integer from 1 to 3. 1 = default FmaxABC; 2 =
#'   specified F multiplier; 3 = catches supplied by `catch_csv`.
#' @param S2_F Numeric multiplier for the scenario-2 F option.
#' @param s4_F Numeric fraction for scenario 4 (default 0.75).
#' @param Fyr_ts Integer from 1 to 7. Scenario used for the five-year
#'   catch time series and scenarios 9–11 catch source.
#' @param do_fig Logical; generate SSB/catch figures for scenarios 1–7.
#' @param init_dir Retained for backwards-compatible calls; currently unused.
#' @param exe Optional SS3 executable filename or path. If `NULL`, search
#'   for a platform-appropriate executable in `DIR`.
#' @param cores Maximum number of parallel workers (up to 7 initially).
#' @param catch_csv Path to `Scenario2_catch.csv`. Searched relative to
#'   the current working directory or the parent of `DIR`.
#' @param skip_run Logical; reuse existing scenario outputs instead of
#'   executing SS3. Scenario forecast files may still be overwritten.
#' @param no_hessian_late Logical; when TRUE and `skip_run = FALSE`, pass
#'   `-nohess` for scenarios 9–15 and report their SDs as missing.
#'
#' @return A list with `SSB` (yearly biomass and F summaries), `CATCH`
#'   (forecast catch and SDs), `Five_year` (five-year ABC/OFL, SSB and F
#'   table), `Tables` (scenario comparisons), and, when `do_fig = TRUE`,
#'   `FIGS` (SSB/catch ggplot lists). An attribute `scenario_map` records
#'   which scenario supplies each five-year quantity.
#'
#' @section Dependencies:
#' Requires `r4ss`, `data.table`, and `ggplot2` and a compatible SS3
#' executable for forecast runs. The parallel workers use base `parallel`.
#'
#' @section Validation:
#' The 2026 v12 implementation has not been validated for numerical
#' equivalence to legacy results. In particular, confirm spawning biomass
#' conventions, multi-fleet catch treatment, and five-year ABC/OFL values
#' before assessment use.
#'
#' @examples
#' \dontrun{
#' result <- Do_AK_TIER_3_Scenarios(
#'   DIR = "Model_26_1_2/PROJ", CYR = 2026, SYR = 1977,
#'   SEXES = 1, FLEETS = 1:2, Scenario2 = 3,
#'   Fyr_ts = 2, catch_csv = "Scenario2_catch.csv",
#'   do_fig = TRUE, no_hessian_late = TRUE
#' )
#' result$Five_year
#' }
#'
#' @author Steve Barbeaux, NOAA Alaska Fisheries Science Center
#' @import data.table
#' @export

Do_AK_TIER_3_Scenarios <- function(
    DIR = "Model_24.1.25/PROJ", CYR = 2025, SYR = 1977,
    SEXES = 1, FLEETS = 1, Scenario2 = 1, S2_F = 0.4, s4_F = 0.75,
    Fyr_ts = 2, do_fig = TRUE, init_dir = NULL,
    exe = NULL, cores = 8L, catch_csv = "Scenario2_catch.csv",
    skip_run = FALSE, no_hessian_late = TRUE) {
  .ak_require()
  DIR <- normalizePath(DIR, winslash = "/", mustWork = TRUE)
  if (!all(c(Scenario2 %in% 1:3, Fyr_ts %in% 1:7, SEXES %in% 1:2,
             CYR >= SYR, length(FLEETS) > 0L, all(FLEETS > 0),
             length(unique(FLEETS)) == length(FLEETS)))) stop("Invalid argument combination")
  if (Scenario2 == 2L && length(FLEETS) != 1L)
    stop("Scenario2=2 F input is currently single-fleet only; revise before using multiple fleets")
  if (Scenario2 == 2L && S2_F < 0) stop("S2_F must be nonnegative")
  if (s4_F <= 0 || s4_F > 1) stop("s4_F must be in (0,1]")
  base <- r4ss::SS_readforecast(file = file.path(DIR, "forecast.ss"))
  FCASTY <- .ak_one(base$Nforecastyrs, "Nforecastyrs")
  if (FCASTY < 5L) stop("At least five forecast years are required")
  cfg <- list(CYR = CYR, SYR = SYR, EYR = CYR + FCASTY, FCASTY = FCASTY,
              FLEETS = FLEETS, Scenario2 = Scenario2, S2_F = S2_F,
              s4_F = s4_F, Fyr_ts = Fyr_ts,
              sex_factor = if (SEXES == 1L) 2 else 1,
              no_hessian_late = isTRUE(no_hessian_late) && !skip_run)
  if (SEXES == 1L) warning("Single-sex SpawnBio divided by 2 as in original; verify this model convention")
  executable <- if (skip_run) NULL else .ak_executable(DIR, exe)
  catch_file <- NULL
  if (Scenario2 == 3L) {
    # Path explicit to prevent changing-working-directory bugs.
    csv_path <- if (file.exists(catch_csv)) catch_csv else file.path(dirname(DIR), catch_csv)
    if (!file.exists(csv_path)) stop("Scenario2 catch CSV not found: ", csv_path)
    catch_file <- .ak_check_forecatch(utils::read.csv(csv_path), FLEETS,
                                      label = "Scenario2_catch.csv")
  }
  base1 <- .ak_reference(base, 0.4)
  .ak_run(DIR, 1L, base1, executable, skip_run)
  baseline <- r4ss::SS_output(dir = file.path(DIR, "scenario_1"), verbose = FALSE)
  # Independent scenarios 2-8 run in parallel; use mclapply only on
  # Unix platforms. Windows uses PSOCK workers with self-contained R closures.
  workers <- min(max(1L, as.integer(cores)), 7L)
  cl <- NULL
  # Close PSOCK workers even if an error interrupts a scenario. Clear the
  # handle after normal shutdown to avoid trying to close connections twice.
  on.exit({
    if (!is.null(cl)) parallel::stopCluster(cl)
  }, add = TRUE)
  task <- function(number) {
    f <- .ak_prepare_independent(number, base1, DIR, cfg, baseline, catch_file)
    .ak_run(DIR, number, f, executable, skip_run)
    number
  }
  if (workers > 1L && .Platform$OS.type != "windows") {
    results <- parallel::mclapply(2:8, task, mc.cores = workers)
    errors <- vapply(results, inherits, logical(1), "try-error")
    if (any(errors)) stop("Parallel scenario failures: ",
                          paste((2:8)[errors], collapse = ", "))
  } else if (workers > 1L) {
    # PSOCK is supported on Windows. Export both the task closure (holding its
    # configuration) and the helpers referenced by that closure.
    cl <- parallel::makeCluster(workers)
    helpers <- c(".ak_prepare_independent", ".ak_check_forecatch", ".ak_one",
                 ".ak_reference", ".ak_run", ".ak_copy_inputs")
    parallel::clusterExport(cl, helpers, envir = environment(.ak_require))
    results <- parallel::parLapply(cl, 2:8, task)
  } else {
    lapply(2:8, task)
  }
  # OFL scenarios 9-11 are independent: each draws specified catches
  # from the selected Fyr_ts scenario (1-7), for different horizon lengths
  # (CYR through CYR + 2/3/4). Do not chain scenarios 9-11.
  ofl_source_name <- paste0("scenario_", Fyr_ts)
  ofl_source <- if (Fyr_ts == 1L) baseline else
    r4ss::SS_output(dir = file.path(DIR, ofl_source_name), verbose = FALSE)
  ofl_task <- function(number) {
    f <- .ak_reference(base1, 0.35)
    f$ForeCatch <- .ak_check_forecatch(
      r4ss::SS_ForeCatch(ofl_source, yrs = seq.int(CYR, CYR + number - 7L), zeros = TRUE),
      label = paste0("scenario_", number, " from ", ofl_source_name))
    .ak_run(DIR, number, f, executable, skip_run, no_hessian = cfg$no_hessian_late)
    number
  }
  ofl_numbers <- 9:11
  ofl_workers <- min(workers, length(ofl_numbers))
  if (ofl_workers > 1L && .Platform$OS.type != "windows") {
    ofl_results <- parallel::mclapply(ofl_numbers, ofl_task, mc.cores = ofl_workers)
    errors <- vapply(ofl_results, inherits, logical(1), "try-error")
    if (any(errors)) stop("Parallel OFL scenario failures: ",
                          paste(ofl_numbers[errors], collapse = ", "))
  } else if (ofl_workers > 1L) {
    # Reuse the existing PSOCK cluster from scenarios 2-8.
    ofl_results <- parallel::parLapply(cl, ofl_numbers, ofl_task)
  } else {
    lapply(ofl_numbers, ofl_task)
  }
  # Later scenarios are sequential. Release the PSOCK workers before loading
  # and processing their outputs, reducing open socket connections.
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
    cl <- NULL
  }
  # ABC chain: prior catch from 1, 12, 13, 14 -> scenarios 12-15.
  previous <- baseline
  for (number in 12:15) {
    d1 <- number - 11L
    f <- base1
    if (Scenario2 == 3L && Fyr_ts == 2L) {
      # Sparse CSV specification: ONLY explicitly provided catch records.
      # Missing years and fleets remain unspecified; never fill with zeros
      # or with entries from prior scenarios.
      sub <- catch_file[
        catch_file$Year >= CYR + 1L & catch_file$Year <= CYR + d1,
        , drop = FALSE
      ]
      if (nrow(sub)) {
        sub <- .ak_check_forecatch(sub, FLEETS,
          label = paste0("Scenario2 CSV for scenario_", number))
      }
      # Retain a zero-row, four-column table when nothing was specified
      # in the current horizon. No new catch records are fabricated.
      f$ForeCatch <- sub
    } else {
      # Original chaining behavior for other scenario configurations.
      f$ForeCatch <- .ak_check_forecatch(
        r4ss::SS_ForeCatch(previous, yrs = seq.int(CYR, CYR + d1), zeros = TRUE),
        label = paste0("scenario_", number))
    }
    .ak_run(DIR, number, f, executable, skip_run, no_hessian = cfg$no_hessian_late)
    previous <- r4ss::SS_output(dir = file.path(DIR, paste0("scenario_", number)), verbose = FALSE)
  }
  names_all <- paste0("scenario_", 1:15)
  mods <- setNames(lapply(names_all, function(n)
    r4ss::SS_output(dir = file.path(DIR, n), verbose = FALSE)), names_all)
  summary_list <- setNames(lapply(names_all, function(n) .ak_summary(mods[[n]], n, cfg)), names_all)
  catch_list <- setNames(lapply(names_all, function(n) .ak_catch(mods[[n]], n, cfg)), names_all)
  five <- .ak_five_year(summary_list, catch_list, mods, cfg)
  summary <- data.table::rbindlist(summary_list)
  catches <- data.table::rbindlist(catch_list)
  summary <- summary[model != "scenario_8"] # original output behavior
  catches <- catches[model != "scenario_8"]
  pscenarios <- paste0("scenario_", 1:7)
  output <- list(
    SSB = summary, CATCH = catches, Five_year = five,
    Tables = list(
      Catch = data.table::dcast(catches[Yr >= CYR & model %in% pscenarios], Yr ~ model, value.var = "Catch"),
      F = data.table::dcast(summary[Yr >= CYR & model %in% pscenarios], Yr ~ model, value.var = "F"),
      SSB = data.table::dcast(summary[Yr >= CYR & model %in% pscenarios], Yr ~ model, value.var = "SSB")
    )
  )
  if (do_fig) {
    # Only scenarios 1-7 appear in figures; scenarios 8-15 are used
    # for projection tables but are excluded from the figure lists.
    output$FIGS <- .ak_figures(summary, catches, cfg,
                               sb40 = five$SB40[[1]], sb35 = five$SB35[[1]],
                               sb100 = five$SB100[[1]])
  }
  attr(output, "scenario_map") <- data.frame(
    year = seq.int(CYR + 1L, CYR + 5L),
    sb_scenario = c(Fyr_ts, 12:15), abc_scenario = c(1L, 12:15),
    ofl_scenario = c(6L, 8:11))
  output
}

# Example: uncomment and edit paths / file parameters.
# result <- Do_AK_TIER_3_Scenarios(
#   DIR = "Model_26.1_750/PROJ", CYR = 2026, SYR = 1977,
#   SEXES = 1, FLEETS = 1:2, Scenario2 = 3, Fyr_ts = 2,
#   catch_csv = "Scenario2_catch.csv", do_fig = TRUE, cores = 8,
#   no_hessian_late = TRUE)
