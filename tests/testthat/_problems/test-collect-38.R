# Extracted from test-collect.R:38

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "gtfsrt2static", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
archive <- tempfile("archive")
config <- data.frame(
    feed_id = "broken",
    url = paste0("file://", tempfile("does-not-exist")),
    interval_s = 0
  )
summary <- rt2s_collect(config, dir = archive, max_polls = 2)
