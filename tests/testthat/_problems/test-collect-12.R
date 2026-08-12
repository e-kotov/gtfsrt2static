# Extracted from test-collect.R:12

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "gtfsrt2static", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
src <- tempfile(fileext = ".pb")
writeBin(as.raw(1:64), src)
archive <- tempfile("archive")
config <- data.frame(
    feed_id = "test_vp",
    url = paste0("file://", src),
    interval_s = 0
  )
summary <- rt2s_collect(config, dir = archive, max_polls = 3)
