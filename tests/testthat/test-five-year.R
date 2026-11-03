test_that("five-year source mapping is stable", {
  years <- 2027:2031
  # Synthesized annual output tables; does not validate SS3 science.
  summ <- setNames(lapply(1:15, function(i) data.table::data.table(
    Yr = years, SSB = 100 * i + seq_along(years),
    F = i + seq_along(years) / 100)), paste0("scenario_", 1:15))
  catches <- setNames(lapply(1:15, function(i) data.table::data.table(
    Yr = years, Catch = 1000 * i + seq_along(years))), paste0("scenario_", 1:15))
  dq <- function(i) data.frame(Label = c("SSB_unfished", "SSB_SPR"), Value = c(1000, i))
  mods <- lapply(1:15, function(i) list(derived_quants = dq(i)))
  cfg <- list(CYR = 2026L, Fyr_ts = 2L, sex_factor = 1L)
  out <- AKScenariosForSS:::.ak_five_year(summ, catches, mods, cfg)
  expect_equal(out$Yr, years)
  expect_equal(out$SSB, c(201, 1202, 1303, 1404, 1505))
  expect_equal(out$C_ABC, c(1001, 12002, 13003, 14004, 15005))
  expect_equal(out$C_OFL, c(6001, 8002, 9003, 10004, 11005))
  expect_equal(out$C_CATCH, 2000 + 1:5)
})
