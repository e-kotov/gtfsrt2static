# Extracted from test-events.R:26

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "gtfsrt2static", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
events <- rt2s_events_from_trip_updates(make_updates())
s1 <- events[stop_ref == "S1"]
expect_identical(nrow(s1), 1L)
expect_identical(s1$provenance, "observed")
expect_identical(format(s1$arrival_time, "%H:%M:%S"), "06:31:05")
