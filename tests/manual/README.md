Manual integration checks require SS3 model outputs, and are NOT run by R CMD check.
Use `regression_comparison.R` after loading the package and compare independent baseline and refactored model runs.
The helper `AK_test_scenario_keys()` calls the unexported `.ak_check_forecatch()`; when using it against an installed package replace that call with `AKScenariosForSS:::.ak_check_forecatch()` or source the package helper locally.
