#' Continuously Archive GTFS-Realtime Feeds
#'
#' GTFS-Realtime endpoints are ephemeral: one FeedMessage, overwritten every
#' few seconds - anything not fetched is lost forever. \code{rt2s_collect()}
#' polls a set of feeds and stores the raw responses on disk, unparsed
#' (fetch bytes, compare, write; archive fidelity never depends on parser
#' versions and malformed responses are preserved as evidence).
#'
#' Storage layout: \code{dir/<feed_id>/<YYYY-MM-DD>/<HHMMSS>.pb} for the open
#' day (file names in UTC), plus \code{dir/<feed_id>/manifest.csv} logging
#' every poll (time, HTTP status, bytes, stored/skipped). Roll finished days
#' into the daily ZIPs that \code{gtfsrealtime::read_gtfsrt_*()} ingest with
#' \code{\link{rt2s_archive_rotate}}.
#'
#' Responses identical to the previous poll of the same feed are logged but
#' not stored (\code{skipped_unchanged}); a long streak of unchanged
#' responses is also how you spot a frozen feed - see
#' \code{\link{rt2s_archive_coverage}}.
#'
#' @param config A data.frame with one row per feed: \code{feed_id},
#'   \code{url}, \code{interval_s} (poll interval in seconds), and optionally
#'   \code{auth_header} (e.g. \code{"x-api-key: ..."} - prefer reading the
#'   secret from an environment variable when building the config).
#' @param dir Archive root directory (created if needed).
#' @param max_polls Maximum number of polls per feed before returning; the
#'   default \code{Inf} runs until interrupted. Finite values are mainly for
#'   testing and supervised restarts.
#' @return Invisibly, a data.table summary of the run (per feed: polls,
#'   stored, skipped, errors).
#' @export
rt2s_collect <- function(config, dir, max_polls = Inf) {
  config <- data.table::as.data.table(config)
  validate_required_columns(config, c("feed_id", "url", "interval_s"), "config")
  if (!"auth_header" %in% names(config)) {
    config[, auth_header := NA_character_]
  }
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  state <- lapply(seq_len(nrow(config)), function(i) {
    list(
      last_bytes = NULL,
      next_due = Sys.time(),
      polls = 0L,
      stored = 0L,
      skipped = 0L,
      errors = 0L
    )
  })
  names(state) <- config$feed_id

  poll_one <- function(i) {
    feed <- config[i]
    st <- state[[feed$feed_id]]
    handle <- curl::new_handle()
    if (!is.na(feed$auth_header)) {
      parts <- strsplit(feed$auth_header, ":\\s*")[[1]]
      curl::handle_setheaders(handle, .list = stats::setNames(
        list(paste(parts[-1], collapse = ": ")),
        parts[1]
      ))
    }
    polled_at <- Sys.time()
    resp <- tryCatch(
      curl::curl_fetch_memory(feed$url, handle = handle),
      error = function(e) e
    )
    st$polls <- st$polls + 1L

    feed_dir <- file.path(dir, feed$feed_id)
    dir.create(feed_dir, showWarnings = FALSE, recursive = TRUE)
    manifest <- file.path(feed_dir, "manifest.csv")

    log_row <- function(status, bytes, stored, file = NA_character_) {
      row <- data.table::data.table(
        polled_at = format(polled_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        status = status,
        bytes = bytes,
        stored = stored,
        file = file
      )
      data.table::fwrite(row, manifest, append = file.exists(manifest))
    }

    if (inherits(resp, "error")) {
      st$errors <- st$errors + 1L
      log_row(status = -1L, bytes = 0L, stored = FALSE)
    } else if (resp$status_code >= 400L) {
      st$errors <- st$errors + 1L
      log_row(status = resp$status_code, bytes = length(resp$content), stored = FALSE)
    } else if (identical(resp$content, st$last_bytes)) {
      st$skipped <- st$skipped + 1L
      log_row(status = resp$status_code, bytes = length(resp$content), stored = FALSE)
    } else {
      day_dir <- file.path(feed_dir, format(polled_at, "%Y-%m-%d", tz = "UTC"))
      dir.create(day_dir, showWarnings = FALSE, recursive = TRUE)
      fname <- file.path(day_dir, paste0(format(polled_at, "%H%M%S", tz = "UTC"), ".pb"))
      tmp <- tempfile(tmpdir = day_dir)
      writeBin(resp$content, tmp)
      file.rename(tmp, fname)
      st$last_bytes <- resp$content
      st$stored <- st$stored + 1L
      log_row(
        status = resp$status_code,
        bytes = length(resp$content),
        stored = TRUE,
        file = basename(fname)
      )
    }
    st$next_due <- polled_at + feed$interval_s
    state[[feed$feed_id]] <<- st
  }

  repeat {
    due <- which(vapply(
      config$feed_id,
      function(id) {
        state[[id]]$polls < max_polls && state[[id]]$next_due <= Sys.time()
      },
      logical(1)
    ))
    for (i in due) {
      poll_one(i)
    }
    active <- vapply(
      config$feed_id,
      function(id) state[[id]]$polls < max_polls,
      logical(1)
    )
    if (!any(active)) {
      break
    }
    next_wakeup <- min(do.call(
      c,
      lapply(config$feed_id[active], function(id) state[[id]]$next_due)
    ))
    wait <- as.numeric(difftime(next_wakeup, Sys.time(), units = "secs"))
    if (wait > 0) {
      Sys.sleep(min(wait, 1))
    }
  }

  invisible(data.table::rbindlist(lapply(config$feed_id, function(id) {
    st <- state[[id]]
    data.table::data.table(
      feed_id = id,
      polls = st$polls,
      stored = st$stored,
      skipped_unchanged = st$skipped,
      errors = st$errors
    )
  })))
}

#' Roll Finished Archive Days into Daily ZIPs
#'
#' Zips every closed day directory (any date before today, UTC) of every feed
#' under the archive root into \code{<feed_id>/<YYYYMMDD>.zip} - exactly the
#' multi-file input \code{gtfsrealtime::read_gtfsrt_positions()} and
#' \code{read_gtfsrt_trip_updates()} ingest - and removes the day directory.
#'
#' @param dir Archive root directory (as used by \code{\link{rt2s_collect}}).
#' @return Invisibly, a character vector of the ZIP files written.
#' @export
rt2s_archive_rotate <- function(dir) {
  if (!nzchar(Sys.which("zip"))) {
    stop(
      "rt2s_archive_rotate() requires the 'zip' system utility on the PATH.",
      call. = FALSE
    )
  }
  today <- format(Sys.time(), "%Y-%m-%d", tz = "UTC")
  written <- character()
  for (feed_dir in list.dirs(dir, recursive = FALSE)) {
    day_dirs <- list.dirs(feed_dir, recursive = FALSE)
    day_dirs <- day_dirs[grepl("^\\d{4}-\\d{2}-\\d{2}$", basename(day_dirs))]
    day_dirs <- day_dirs[basename(day_dirs) < today]
    for (day_dir in day_dirs) {
      zip_path <- file.path(
        feed_dir,
        paste0(gsub("-", "", basename(day_dir)), ".zip")
      )
      files <- list.files(day_dir, full.names = TRUE)
      if (length(files) == 0L) {
        unlink(day_dir, recursive = TRUE)
        next
      }
      status <- utils::zip(zip_path, files, flags = "-q -j")
      if (status != 0L) {
        warning("zip failed for ", day_dir, " (status ", status, ").", call. = FALSE)
        next
      }
      unlink(day_dir, recursive = TRUE)
      written <- c(written, zip_path)
    }
  }
  invisible(written)
}

#' Archive Coverage and Gap Report
#'
#' Summarizes each feed's manifest: polls per day, how many were stored vs
#' skipped as unchanged, error counts, and the longest gap between
#' consecutive polls. Long \code{skipped_unchanged} streaks with an
#' advancing poll clock usually mean a frozen upstream feed.
#'
#' @param dir Archive root directory (as used by \code{\link{rt2s_collect}}).
#' @return A data.table with one row per feed and day.
#' @export
rt2s_archive_coverage <- function(dir) {
  manifests <- list.files(
    dir,
    pattern = "^manifest\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(manifests) == 0L) {
    stop("No manifest.csv files found under ", dir, ".", call. = FALSE)
  }
  out <- data.table::rbindlist(lapply(manifests, function(m) {
    dt <- data.table::fread(m)
    dt[, feed_id := basename(dirname(m))]
    dt[, polled_at := as.POSIXct(
      polled_at,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )]
    dt[, day := format(polled_at, "%Y-%m-%d", tz = "UTC")]
    data.table::setorderv(dt, "polled_at")
    dt[, .(
      polls = .N,
      stored = sum(stored),
      skipped_unchanged = sum(!stored & status >= 200 & status < 400),
      errors = sum(status < 0 | status >= 400),
      first_poll = min(polled_at),
      last_poll = max(polled_at),
      longest_gap_s = if (.N > 1L) {
        max(as.numeric(diff(polled_at), units = "secs"))
      } else {
        NA_real_
      }
    ), by = .(feed_id, day)]
  }))
  data.table::setkeyv(out, c("feed_id", "day"))
  out[]
}

#' Generate a Supervised-Service Template for the Collector
#'
#' Emits a ready-to-edit service definition that keeps
#' \code{\link{rt2s_collect}} running unattended: a systemd unit, a launchd
#' plist, a cron @reboot line, or a Dockerfile.
#'
#' @param type One of \code{"systemd"}, \code{"launchd"}, \code{"cron"},
#'   \code{"docker"}.
#' @param config_path Path to an R script or RDS/CSV holding the feeds
#'   config; referenced in the generated command.
#' @param dir Archive root directory used in the generated command.
#' @return The template as a character scalar (also printed with
#'   \code{cat()} for copy-pasting).
#' @export
rt2s_service_template <- function(
  type = c("systemd", "launchd", "cron", "docker"),
  config_path = "/etc/gtfsrt2static/feeds.csv",
  dir = "/var/lib/gtfsrt-archive"
) {
  type <- match.arg(type)
  cmd <- sprintf(
    "Rscript -e 'gtfsrt2static::rt2s_collect(read.csv(\"%s\"), dir = \"%s\")'",
    config_path,
    dir
  )
  tpl <- switch(
    type,
    systemd = sprintf(
      "[Unit]\nDescription=GTFS-Realtime feed collector (gtfsrt2static)\nAfter=network-online.target\n\n[Service]\nExecStart=%s\nRestart=always\nRestartSec=10\nUser=gtfsrt\n\n[Install]\nWantedBy=multi-user.target\n",
      cmd
    ),
    launchd = sprintf(
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n<plist version=\"1.0\">\n<dict>\n  <key>Label</key><string>org.gtfsrt2static.collector</string>\n  <key>ProgramArguments</key>\n  <array><string>/bin/sh</string><string>-c</string><string>%s</string></array>\n  <key>RunAtLoad</key><true/>\n  <key>KeepAlive</key><true/>\n</dict>\n</plist>\n",
      cmd
    ),
    cron = sprintf("@reboot %s >> /var/log/gtfsrt-collect.log 2>&1\n", cmd),
    docker = sprintf(
      "FROM rocker/r-ver:4.4\nRUN R -q -e 'install.packages(c(\"gtfsrt2static\"), repos = \"https://cloud.r-project.org\")'\nCOPY feeds.csv %s\nVOLUME %s\nCMD %s\n",
      config_path,
      dir,
      cmd
    )
  )
  cat(tpl)
  invisible(tpl)
}