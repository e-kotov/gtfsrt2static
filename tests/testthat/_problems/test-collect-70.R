# Extracted from test-collect.R:70

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "gtfsrt2static", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if(!nzchar(Sys.which("zip")), "zip utility not available")
archive <- tempfile("archive")
feed_dir <- file.path(archive, "test_vp")
old_day <- file.path(feed_dir, "2026-07-14")
dir.create(old_day, recursive = TRUE)
writeBin(as.raw(1:16), file.path(old_day, "063000.pb"))
writeBin(as.raw(17:32), file.path(old_day, "063030.pb"))
manifest <- data.table::data.table(
    polled_at = c("2026-07-14T06:30:00Z", "2026-07-14T06:30:30Z", "2026-07-14T06:35:30Z"),
    status = c(200L, 200L, 200L),
    bytes = c(16L, 16L, 16L),
    stored = c(TRUE, TRUE, FALSE),
    file = c("063000.pb", "063030.pb", NA)
  )
data.table::fwrite(manifest, file.path(feed_dir, "manifest.csv"))
written <- rt2s_archive_rotate(archive)
expect_identical(basename(written), "20260714.zip")
expect_false(dir.exists(old_day))
expect_identical(
    sort(utils::unzip(written, list = TRUE)$Name),
    c("063000.pb", "063030.pb")
  )
cov <- rt2s_archive_coverage(archive)
