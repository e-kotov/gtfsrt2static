# Extracted from test-events.R:49

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "gtfsrt2static", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
delay_only <- make_updates()[3, ]
delay_only$arrival_time <- ts(NA)
delay_only$departure_time <- ts(NA)
delay_only$arrival_delay <- 90
expect_error(
    snapshot_from_trip_updates(delay_only),
    "requires the baseline feed's scheduled times"
  )
