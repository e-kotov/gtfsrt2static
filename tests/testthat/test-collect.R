test_that("rt_collect stores changed responses and skips unchanged ones", {
  src <- tempfile(fileext = ".pb")
  writeBin(as.raw(1:64), src)
  archive <- tempfile("archive")

  config <- data.frame(
    feed_id = "test_vp",
    url = paste0("file://", src),
    interval_s = 0
  )

  summary <- rt_collect(config, dir = archive, max_polls = 3)

  expect_identical(summary$polls, 3L)
  expect_identical(summary$stored, 1L) # identical bytes on polls 2-3
  expect_identical(summary$skipped_unchanged, 2L)
  expect_identical(summary$errors, 0L)

  stored_files <- list.files(
    file.path(archive, "test_vp"),
    pattern = "\\.pb$",
    recursive = TRUE
  )
  expect_identical(length(stored_files), 1L)

  manifest <- data.table::fread(file.path(archive, "test_vp", "manifest.csv"))
  expect_identical(nrow(manifest), 3L)
  expect_identical(sum(manifest$stored), 1L)
})

test_that("rt_collect logs errors for unreachable feeds", {
  archive <- tempfile("archive")
  config <- data.frame(
    feed_id = "broken",
    url = paste0("file://", tempfile("does-not-exist")),
    interval_s = 0
  )
  summary <- rt_collect(config, dir = archive, max_polls = 2)
  expect_identical(summary$errors, 2L)
  expect_identical(summary$stored, 0L)
})

test_that("rt_rotate zips closed days and rt_coverage reports them", {
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

  written <- rt_rotate(archive)
  expect_identical(basename(written), "20260714.zip")
  expect_false(dir.exists(old_day))
  expect_identical(
    sort(utils::unzip(written, list = TRUE)$Name),
    c("063000.pb", "063030.pb")
  )

  cov <- rt_coverage(archive)
  expect_identical(cov$polls, 3L)
  expect_identical(cov$stored, 2L)
  expect_identical(cov$skipped_unchanged, 1L)
  expect_identical(cov$longest_gap_s, 300)
})

test_that("rt_service_template renders all four flavors", {
  for (type in c("systemd", "launchd", "cron", "docker")) {
    tpl <- capture.output(out <- rt_service_template(type))
    expect_true(nzchar(out))
    expect_match(out, "rt_collect", fixed = TRUE)
  }
})