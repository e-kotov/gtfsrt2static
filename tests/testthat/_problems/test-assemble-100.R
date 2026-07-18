# Extracted from test-assemble.R:100

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "gtfsrt2static", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
events <- snapshot_from_trip_updates(make_updates())
expect_warning(
    feed <- snapshot_assemble(events, baseline = make_baseline()),
    NA
  )
expect_identical(as.character(feed$trips$trip_id), "CS_1")
expect_identical(feed$trips$service_id, "SVC_20260714")
expect_null(feed$calendar)
