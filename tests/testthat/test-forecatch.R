test_that("forecast catch validator accepts sparse CSV data", {
  fc <- data.frame(Year = c(2027, 2029), Seas = c(1, 1),
                   Fleet = c(1, 2), Catch_or_F = c(150, 75))
  value <- AKScenariosForSS:::.ak_check_forecatch(fc, fleets = 1:2)
  expect_equal(nrow(value), 2L)
  expect_equal(value$Year, c(2027, 2029))
  expect_equal(value$Fleet, c(1, 2))
  expect_error(AKScenariosForSS:::.ak_check_forecatch(
    fc, fleets = 1:2, required_years = 2027:2029, require_all = TRUE),
    "missing Year:Seas:Fleet keys")
})

test_that("SS_ForeCatch output column names are normalized", {
  fc <- data.frame("#Year" = 2027, Seas = 1, Fleet = 1,
                   "dead(B)" = 42, comment = "from SS", check.names = FALSE)
  value <- AKScenariosForSS:::.ak_check_forecatch(fc)
  expect_named(value, c("Year", "Seas", "Fleet", "Catch_or_F"))
  expect_equal(value$Catch_or_F, 42)
})

test_that("duplicate catch keys are rejected", {
  fc <- data.frame(Year = c(2027, 2027), Seas = 1, Fleet = 1,
                   Catch_or_F = c(10, 15))
  expect_error(AKScenariosForSS:::.ak_check_forecatch(fc), "duplicate")
})

test_that("zero-catch scenario creates records for each selected fleet", {
  # Avoid model execution; test the forecast preparation helper directly.
  base <- list(ForeCatch = NULL)
  cfg <- list(CYR = 2026L, FCASTY = 5L, FLEETS = 1:2, Scenario2 = 1L)
  out <- AKScenariosForSS:::.ak_prepare_independent(5L, base,
                    root = tempdir(), cfg = cfg, baseline = NULL, catch_file = NULL)
  expect_equal(nrow(out$ForeCatch), 10L)
  expect_true(all(out$ForeCatch$Catch_or_F == 0))
})
