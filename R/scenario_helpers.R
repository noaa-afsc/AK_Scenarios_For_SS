# Internal SS3 scenario helpers (derived without computational edits from v12).
.ak_require <- function() {
  needed <- c("r4ss", "data.table", "ggplot2")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
}

# Resolve the optional Scenario 2 catch CSV before any SS3 runs start.
.ak_scenario2_csv <- function(catch_csv, model_dir) {
  if (!is.character(catch_csv) || length(catch_csv) != 1L ||
      is.na(catch_csv) || !nzchar(trimws(catch_csv))) {
    stop("For Scenario2 = 3, catch_csv must be a nonempty filename or path.",
         call. = FALSE)
  }
  candidates <- unique(c(catch_csv, file.path(dirname(model_dir), catch_csv)))
  existing <- candidates[file.exists(candidates) & !dir.exists(candidates)]
  if (!length(existing)) {
    msg <- paste0(
      "Scenario2 = 3 requires a catch CSV, but the file was not found. ",
      "Searched: ", paste(candidates, collapse = "; "),
      ". Supply catch_csv or put Scenario2_catch.csv in a searched location."
    )
    warning(msg, call. = FALSE)
    stop("Stopping: required Scenario 2 catch CSV is missing.", call. = FALSE)
  }
  normalizePath(existing[[1L]], winslash = "/", mustWork = TRUE)
}

.ak_one <- function(x, label) {
  if (length(x) != 1L || is.na(x)) stop("Expected exactly one value for ", label, "; found ", length(x))
  as.numeric(x)
}

.ak_row <- function(dt, year, field, label) {
  z <- dt[dt$Yr == year, field, with = FALSE][[1]]
  .ak_one(z, paste(label, year))
}

.ak_check_forecatch <- function(x, fleets = NULL, required_years = NULL,
                                require_all = FALSE, label = "ForeCatch") {
  # SS_ForeCatch() can produce '#Year', 'Seas', 'Fleet', 'dead(B)' (or
  # 'dead(N)'/'retain(B)'/'retain(N)'), and an optional 'comment'.
  # Read-forecast/CSV tables often use Year, Seas, Fleet, Catch_or_F.
  # Normalize both representations to four numeric columns, as required
  # by the forecast input section. Keep all catch values unchanged.
  if (is.null(x)) stop(label, ": no catch records returned")
  x <- as.data.frame(x, check.names = FALSE)
  if (!nrow(x)) stop(label, ": no catch records returned")
  n <- names(x)
  year_col <- intersect(c("Year", "#Year", "X.Year", "Yr"), n)
  seas_col <- intersect(c("Seas", "Season"), n)
  fleet_col <- intersect(c("Fleet", "Flt"), n)
  value_col <- intersect(c("Catch_or_F", "Catch", "dead(B)", "dead(N)",
                           "retain(B)", "retain(N)", "retain(B):_", "retain(N):_"), n)
  if (length(year_col) != 1L || length(seas_col) != 1L ||
      length(fleet_col) != 1L || length(value_col) != 1L) {
    stop(label, ": expected year, season, fleet and one catch column; received: ",
         paste(n, collapse = ", "))
  }
  cols <- c(year_col, seas_col, fleet_col, value_col)
  x <- x[, cols, drop = FALSE]
  names(x) <- c("Year", "Seas", "Fleet", "Catch_or_F")
  for (col in names(x)) {
    raw <- x[[col]]
    num <- suppressWarnings(as.numeric(as.character(raw)))
    if (any(!is.finite(num))) stop(label, ": missing or nonnumeric values in ", col)
    x[[col]] <- num
  }
  if (anyDuplicated(x[, c("Year", "Seas", "Fleet")]))
    stop(label, ": duplicate Year-Seas-Fleet combinations")
  if (!all(x$Year == as.integer(x$Year)) ||
      !all(x$Seas == as.integer(x$Seas)) ||
      !all(x$Fleet == as.integer(x$Fleet))) stop(label, ": non-integer keys")
  if (any(x$Catch_or_F < 0)) stop(label, ": negative catch/F entries")
  if (!is.null(fleets) && any(!x$Fleet %in% fleets))
    stop(label, ": unexpected fleet(s): ", paste(setdiff(unique(x$Fleet), fleets), collapse = ", "))
  if (require_all && length(required_years)) {
    if (is.null(fleets)) stop("fleets required for full coverage test")
    wanted <- expand.grid(Year = required_years, Seas = 1L, Fleet = fleets)
    keys <- function(z) paste(z$Year, z$Seas, z$Fleet, sep = ":")
    missing <- setdiff(keys(wanted), keys(x))
    if (length(missing)) stop(label, ": missing Year:Seas:Fleet keys: ",
                             paste(utils::head(missing, 12), collapse = ", "))
  }
  x
}

.ak_executable <- function(dir, exe = NULL) {
  if (!is.null(exe)) {
    if (length(exe) != 1L || !nzchar(exe)) stop("exe must be a nonempty string")
    return(exe)
  }
  names <- switch(Sys.info()[["sysname"]],
    Windows = c("ss.exe", "ss3.exe", "ss3_win.exe"),
    Darwin = c("ss_osx", "ss3", "ss3_osx_arm64"),
    Linux = c("ss3", "ss3_linux"),
    stop("Unsupported OS; supply exe explicitly"))
  found <- names[file.exists(file.path(dir, names))]
  if (!length(found)) stop("No SS3 executable in ", dir, "; supply exe explicitly")
  found[[1]]
}

.ak_copy_inputs <- function(source, destination) {
  # Copy only files from the root model folder, not scenario subdirectories.
  if (!dir.exists(destination) && !dir.create(destination, recursive = TRUE))
    stop("Cannot create scenario directory: ", destination)
  files <- list.files(source, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  files <- files[!dir.exists(files)]
  if (!length(files)) stop("Model root contains no files: ", source)
  ok <- file.copy(files, destination, overwrite = TRUE, copy.mode = TRUE)
  if (!all(ok)) stop("Could not copy: ", paste(basename(files[!ok]), collapse = ", "))
  invisible(destination)
}

.ak_run <- function(root, number, forecast, exe, skip_run = FALSE, no_hessian = FALSE) {
  name <- paste0("scenario_", number)
  dest <- file.path(root, name)
  .ak_copy_inputs(root, dest)
  r4ss::SS_writeforecast(forecast, dir = dest, file = "forecast.ss",
                          writeAll = TRUE, overwrite = TRUE)
  if (!skip_run && no_hessian) {
    # Root model outputs may contain an old standard-error file. A -nohess
    # projection must not inherit uncertainty from a different run.
    stale <- file.path(dest, c("ss.std", "ss.cor", "ss.cov", "ss3.std", "ss3.cor", "ss3.cov"))
    unlink(stale[file.exists(stale)])
  }
  if (!skip_run) {
    result <- r4ss::run(dir = dest, exe = exe,
                        extras = if (no_hessian) "-nohess" else "",
                        skipfinished = FALSE, verbose = FALSE)
    if (identical(result, "model run failed") || identical(result, "not a directory"))
      stop(name, ": r4ss::run returned ", result)
    if (!file.exists(file.path(dest, "Report.sso")))
      stop(name, ": missing Report.sso after execution; status: ", paste(result, collapse = ", "))
  }
  invisible(dest)
}

.ak_reference <- function(f, b) {
  f$Btarget <- b
  f$SPRtarget <- b
  f$Flimitfraction <- 1.0
  f
}

.ak_prepare_independent <- function(number, base, root, cfg, baseline, catch_file) {
  f <- base
  if (number == 2L) {
    if (cfg$Scenario2 == 2L) {
      year_label <- paste0("F_", cfg$CYR + 1L)
      fq <- data.table::as.data.table(baseline$derived_quants)
      f2 <- cfg$S2_F * .ak_one(fq[Label == year_label, Value], year_label)
      # Retains original interpretation: F input via InputBasis=99;
      # verify compatibility against your forecast.ss and SS version.
      years <- seq.int(cfg$CYR + 1L, cfg$CYR + cfg$FCASTY)
      f$Forecast <- 4
      f$BforconstantF <- 0.001
      f$BfornoF <- 0.0001
      f$InputBasis <- 99
      f$ForeCatch <- data.frame(Year = years, Seas = 1L,
                                Fleet = cfg$FLEETS[[1]], Catch_or_F = rep(f2, length(years)))
    } else if (cfg$Scenario2 == 3L) {
      f$ForeCatch <- catch_file
    }
  } else if (number == 3L) {
    f$Forecast <- 4
    f$BforconstantF <- 0.001
    f$BfornoF <- 0.0001
    if (length(f$Fcast_years) < 4L) stop("forecast$Fcast_years has fewer than four entries")
    f$Fcast_years[c(3, 4)] <- c(cfg$CYR - 5L, cfg$CYR - 1L)
  } else if (number == 4L) {
    f$Flimitfraction <- cfg$s4_F
  } else if (number == 5L) {
    # Zero catch overwrites existing catch on the same full key.
    zeros <- expand.grid(Year = seq.int(cfg$CYR + 1L, cfg$CYR + cfg$FCASTY),
                         Seas = 1L, Fleet = cfg$FLEETS)
    zeros$Catch_or_F <- 0
    # SS_readforecast() can return NULL, a zero-length placeholder, or an
    # empty data.frame when no fixed catches were specified in forecast.ss.
    # Scenario 5 should initialize its zero-catch records from scratch.
    raw_previous <- f$ForeCatch
    previous <- if (is.null(raw_previous) || length(raw_previous) == 0L ||
                    (is.data.frame(raw_previous) && nrow(raw_previous) == 0L)) {
      zeros[FALSE, , drop = FALSE]
    } else {
      .ak_check_forecatch(raw_previous, label = "baseline scenario 5")
    }
    key <- function(z) paste(z$Year, z$Seas, z$Fleet, sep = ":")
    f$ForeCatch <- rbind(previous[!key(previous) %in% key(zeros), , drop = FALSE], zeros)
  } else if (number == 6L) {
    f <- .ak_reference(f, 0.35)
  } else if (number %in% c(7L, 8L)) {
    f <- .ak_reference(f, 0.35)
    prior <- r4ss::SS_output(dir = file.path(root, "scenario_1"), verbose = FALSE)
    d <- if (number == 7L) 2L else 1L
    f$ForeCatch <- r4ss::SS_ForeCatch(prior, yrs = seq.int(cfg$CYR, cfg$CYR + d), zeros = TRUE)
  } else stop("Unknown independent scenario ", number)
  if (!is.null(f$ForeCatch) && length(f$ForeCatch) > 0L &&
      !(is.data.frame(f$ForeCatch) && nrow(f$ForeCatch) == 0L))
    f$ForeCatch <- .ak_check_forecatch(f$ForeCatch, label = paste("scenario", number))
  f
}

.ak_summary <- function(mod, name, cfg) {
  yrs <- seq.int(cfg$SYR, cfg$EYR)
  ts <- data.table::as.data.table(mod$timeseries)
  ts <- ts[Yr %in% yrs]
  if (anyDuplicated(ts$Yr) || !identical(as.integer(ts$Yr), yrs)) {
    stop(name, ": timeseries must have one ordered row per year; found ",
         paste(ts$Yr, collapse = ","))
  }
  at <- data.table::as.data.table(mod$annual_time_series)
  at <- at[year %in% yrs]
  if (anyDuplicated(at$year) || !identical(as.integer(at$year), yrs))
    stop(name, ": annual_time_series F years do not align with years requested")
  d <- data.table::as.data.table(mod$derived_quants)
  if (isTRUE(cfg$no_hessian_late) && as.integer(sub("scenario_", "", name)) >= 9L) {
    sdv <- rep(NA_real_, length(yrs))
  } else {
    sf <- data.table::as.data.table(mod$stdtable)
    if (!all(c("name", "std") %in% names(sf))) {
      stop(name, ": standard errors unavailable; rerun with Hessian or enable no_hessian_late")
    }
    # Only annual SSB standard deviations belong in the SSB time series.
    # Do not match SSB_unfished, SSB_SPR or other derived reference quantities.
    ssb_sd <- sf[grepl("^SSB_[0-9]{4}$", name), .(name, std)]
    ssb_sd[, Yr := as.integer(sub("^SSB_", "", name))]
    duplicates <- ssb_sd[duplicated(Yr) | duplicated(Yr, fromLast = TRUE)]
    if (nrow(duplicates)) {
      stop(name, ": duplicate annual SSB standard deviations in stdtable: ",
           paste(duplicates$name, collapse = ", "),
           ". Inspect the stock/area definitions before combining them.")
    }
    if (!nrow(ssb_sd)) {
      warning(name, ": no annual SSB_YYYY entries in stdtable; ",
              "standard deviations will be NA. Inspect stdtable$name labels.")
      sdv <- rep(NA_real_, length(yrs))
    } else {
      sd_map <- ssb_sd[, .(Yr, std)]
      sdv <- sd_map[data.table::data.table(Yr = yrs), on = "Yr"]$std
      missing_years <- yrs[is.na(sdv)]
      if (length(missing_years)) {
        warning(name, ": missing annual SSB SDs for ",
                paste(missing_years, collapse = ", "), "; CIs will be NA for those years")
      }
    }
  }
  historical_catch <- at[year <= cfg$CYR, dead_catch_B_an]
  future_catch <- d[grepl("^ForeCatch_", Label), Value]
  catch <- c(historical_catch, future_catch)
  if (length(catch) != length(yrs)) {
    stop(name, ": Catch length ", length(catch), " differs from years ", length(yrs),
         ". Check ForeCatch labels / multi-fleet format")
  }
  unfished <- .ak_one(d[Label == "SSB_unfished", Value], paste(name, "SSB_unfished")) / cfg$sex_factor
  data.table::data.table(Yr = yrs, TOT = ts$Bio_all, SUMM = ts$Bio_smry,
                          SSB = ts$SpawnBio / cfg$sex_factor,
                          std = sdv / cfg$sex_factor, F = at$F_std,
                          Catch = catch, SSB_unfished = unfished, model = name)
}

.ak_catch <- function(mod, name, cfg) {
  yrs <- seq.int(cfg$CYR + 1L, cfg$EYR)
  d <- data.table::as.data.table(mod$derived_quants)
  c <- d[grepl("^ForeCatch_", Label), .(Label, Value)]
  has_no_hessian <- isTRUE(cfg$no_hessian_late) && as.integer(sub("scenario_", "", name)) >= 9L
  if (!has_no_hessian) {
    s <- data.table::as.data.table(mod$stdtable)
    if (!all(c("name", "std") %in% names(s))) {
      stop(name, ": catch standard errors unavailable; rerun with Hessian or enable no_hessian_late")
    }
    sd <- s[grepl("^ForeCatch_", name), .(name, std)]
  }
  # Prefer explicit year alignment; do not depend on row order when possible.
  parse_year <- function(z) suppressWarnings(as.integer(sub(".*?(\\d{4}).*", "\\1", z)))
  c[, Yr := parse_year(Label)]
  if (!has_no_hessian) sd[, Yr := parse_year(name)]
  if (anyNA(c$Yr) || anyDuplicated(c$Yr))
    stop(name, ": cannot uniquely align derived ForeCatch values by year")
  if (!has_no_hessian && (anyNA(sd$Yr) || anyDuplicated(sd$Yr)))
    stop(name, ": cannot uniquely align ForeCatch stdtable by year")
  vals <- c[data.table::data.table(Yr = yrs), on = "Yr"]$Value
  sigmas <- if (has_no_hessian) rep(NA_real_, length(yrs)) else
    sd[data.table::data.table(Yr = yrs), on = "Yr"]$std
  if (anyNA(vals)) stop(name, ": missing annual forecast catch")
  data.table::data.table(Yr = yrs, Catch = vals, Catch_std = sigmas, model = name)
}

.ak_five_year <- function(summ, catches, mods, cfg) {
  take <- function(table, model, year, field) {
    x <- table[[paste0("scenario_", model)]]
    .ak_row(x, year, field, paste0("scenario_", model, ":", field))
  }
  d1 <- data.table::as.data.table(mods[[1]]$derived_quants)
  d8 <- data.table::as.data.table(mods[[8]]$derived_quants)
  sb100 <- .ak_one(d1[Label == "SSB_unfished", Value], "SSB_unfished") / cfg$sex_factor
  sb40 <- .ak_one(d1[Label == "SSB_SPR", Value], "SSB_SPR (scenario 1)") / cfg$sex_factor
  sb35 <- .ak_one(d8[Label == "SSB_SPR", Value], "SSB_SPR (scenario 8)") / cfg$sex_factor
  years <- seq.int(cfg$CYR + 1L, cfg$CYR + 5L)
  # Retained from original five-year table: first year uses projection scenario,
  # future SSB and ABC use scenarios 12-15, and OFL uses 8-11.
  source_ssb <- c(cfg$Fyr_ts, 12:15)
  source_abc <- c(1L, 12:15)
  source_ofl <- c(6L, 8:11)
  get <- function(tbl, sources, field)
    vapply(seq_along(years), function(i) take(tbl, sources[i], years[i], field), numeric(1))
  ssb <- get(summ, source_ssb, "SSB")
  data.table::data.table(
    Yr = years, SSB = ssb, SSB_PER = ssb / sb100,
    SB100 = rep(sb100, 5L), SB40 = rep(sb40, 5L), SB35 = rep(sb35, 5L),
    FCATCH = vapply(years, function(y) take(summ, cfg$Fyr_ts, y, "F"), numeric(1)),
    FOFL = get(summ, source_ofl, "F"), FABC = get(summ, source_abc, "F"),
    C_CATCH = get(catches, rep(cfg$Fyr_ts, 5L), "Catch"),
    C_ABC = get(catches, source_abc, "Catch"),
    C_OFL = get(catches, source_ofl, "Catch")
  )
}

.ak_figures <- function(summary, catches, cfg, sb40, sb35, sb100) {
  gg <- ggplot2::ggplot
  ref <- data.table::rbindlist(list(
    data.table::data.table(Yr = seq.int(cfg$SYR, cfg$EYR), model = "SSB40%", SSB = sb40, std = 0),
    data.table::data.table(Yr = seq.int(cfg$SYR, cfg$EYR), model = "SSB35%", SSB = sb35, std = 0),
    data.table::data.table(Yr = seq.int(cfg$SYR, cfg$EYR), model = "SSB20%", SSB = 0.2 * sb100, std = 0)
  ))
  sd <- data.table::rbindlist(list(ref, summary[, .(Yr, model, SSB, std)]))
  sd[, `:=`(LCI = pmax(0, SSB - 1.96 * std), UCI = SSB + 1.96 * std)]
  cd <- data.table::copy(catches)
  cd[, `:=`(LCI = pmax(0, Catch - 1.96 * Catch_std), UCI = Catch + 1.96 * Catch_std)]
  # Restore the horizontal catch reference lines from the original script.
  # Both reference levels use the FINAL forecast year's catch: F40% from
  # scenario 1 and F35% from scenario 6. These are not extra SS3 runs.
  final_catch <- function(scenario) {
    value <- cd[model == scenario & Yr == cfg$EYR, Catch]
    if (length(value) != 1L || !is.finite(value)) {
      stop("Cannot construct catch reference line for ", scenario,
           ": expected one finite final-year catch at year ", cfg$EYR)
    }
    value
  }
  catch_refs <- data.table::rbindlist(list(
    data.table::data.table(
      Yr = seq.int(cfg$CYR + 1L, cfg$EYR), model = "Catch at F40%",
      Catch = final_catch("scenario_1"), Catch_std = 0),
    data.table::data.table(
      Yr = seq.int(cfg$CYR + 1L, cfg$EYR), model = "Catch at F35%",
      Catch = final_catch("scenario_6"), Catch_std = 0)
  ))
  catch_refs[, `:=`(LCI = Catch, UCI = Catch)]
  cd <- data.table::rbindlist(list(catch_refs, cd), use.names = TRUE)
  catch_reference <- c("Catch at F40%", "Catch at F35%")
  make_plot <- function(dt, y, title, selected, reference, years) {
    dat <- dt[model %in% c(reference, selected)]
    p <- gg(dat, ggplot2::aes(x = Yr, y = .data[[y]], color = model, linetype = model)) +
      ggplot2::geom_line(linewidth = 1.1) +
      ggplot2::coord_cartesian(xlim = years) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(x = "Year", y = title, title = "Projections")
    p
  }
  # Only scenarios 1-7 are plotted. Scenarios 8-15 remain in model outputs
  # and five-year calculations, but do not generate figure objects.
  sn <- paste0("scenario_", 1:7)
  ssb_reference <- c("SSB40%", "SSB35%", "SSB20%")
  ssb_plots <- list(ALL = make_plot(sd, "SSB", "Spawning biomass (t)", sn[1:7], ssb_reference,
                                   c(cfg$CYR - 1L, cfg$EYR)))
  catch_plots <- list(ALL = make_plot(cd, "Catch", "Catch (t)", sn[1:7], catch_reference,
                                     c(cfg$CYR + 1L, cfg$EYR)) +
    ggplot2::scale_linetype_manual(values = c("Catch at F40%" = "dashed",
                                              "Catch at F35%" = "dotted",
                                              stats::setNames(rep("solid", length(sn)), sn))))
  for (n in sn) {
    ssb_plots[[n]] <- make_plot(sd, "SSB", "Spawning biomass (t)", n, ssb_reference,
                                c(cfg$CYR - 1L, cfg$EYR)) +
      ggplot2::geom_ribbon(data = sd[model == n], ggplot2::aes(x = Yr, ymin = LCI, ymax = UCI),
                           inherit.aes = FALSE, alpha = 0.15)
    catch_plots[[n]] <- make_plot(cd, "Catch", "Catch (t)", n, catch_reference,
                                  c(cfg$CYR + 1L, cfg$EYR)) +
      ggplot2::scale_linetype_manual(values = c("Catch at F40%" = "dashed",
                                                "Catch at F35%" = "dotted",
                                                stats::setNames("solid", n))) +
      ggplot2::geom_ribbon(data = cd[model == n], ggplot2::aes(x = Yr, ymin = LCI, ymax = UCI),
                           inherit.aes = FALSE, alpha = 0.15)
  }
  list(ssb_plots, catch_plots)
}

