# Extracted from test-events.R:57

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "gtfsrt2static", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
good <- snapshot_from_stop_times(make_g2g_stop_times())
