test_that("plots retain catch references and exclude scenarios 8-15", {
  years <- 2027:2031
  summ <- data.table::rbindlist(lapply(1:7, function(i)
    data.table::data.table(Yr = years, model = paste0("scenario_", i),
                           SSB = i * 100 + 1:5, std = rep(2, 5))))
  catch <- data.table::rbindlist(lapply(1:7, function(i)
    data.table::data.table(Yr = years, model = paste0("scenario_", i),
                           Catch = i * 100 + 1:5, Catch_std = rep(2, 5))))
  cfg <- list(SYR = 2027L, CYR = 2026L, EYR = 2031L)
  plots <- AKScenariosForSS:::.ak_figures(summ, catch, cfg, 300, 400, 1000)
  expect_length(plots[[1]], 8L) # ALL + scenarios 1-7
  expect_length(plots[[2]], 8L)
  catch_data <- plots[[2]]$ALL$data
  expect_true(all(c("Catch at F40%", "Catch at F35%") %in% catch_data$model))
  expect_false(any(grepl("scenario_(8|9|1[0-5])$", catch_data$model)))
})
