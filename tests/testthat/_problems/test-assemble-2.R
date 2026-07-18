# Extracted from test-assemble.R:2

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "gtfsrt2static", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
events <- snapshot_from_trip_updates(make_updates())
