# Continuously Archive GTFS-Realtime Feeds

GTFS-Realtime endpoints are ephemeral: one FeedMessage, overwritten
every few seconds - anything not fetched is lost forever.
`rt2s_collect()` polls a set of feeds and stores the raw responses on
disk, unparsed (fetch bytes, compare, write; archive fidelity never
depends on parser versions and malformed responses are preserved as
evidence).

## Usage

``` r
rt2s_collect(config, dir, max_polls = Inf)
```

## Arguments

- config:

  A data.frame with one row per feed: `feed_id`, `url`, `interval_s`
  (poll interval in seconds), and optionally `auth_header` (e.g.
  `"x-api-key: ..."` - prefer reading the secret from an environment
  variable when building the config).

- dir:

  Archive root directory (created if needed).

- max_polls:

  Maximum number of polls per feed before returning; the default `Inf`
  runs until interrupted. Finite values are mainly for testing and
  supervised restarts.

## Value

Invisibly, a data.table summary of the run (per feed: polls, stored,
skipped, errors).

## Details

Storage layout: `dir/<feed_id>/<YYYY-MM-DD>/<HHMMSS>.pb` for the open
day (file names in UTC), plus `dir/<feed_id>/manifest.csv` logging every
poll (time, HTTP status, bytes, stored/skipped). Roll finished days into
the daily ZIPs that `gtfsrealtime::read_gtfsrt_*()` ingest with
[`rt2s_archive_rotate`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_archive_rotate.md).

Responses identical to the previous poll of the same feed are logged but
not stored (`skipped_unchanged`); a long streak of unchanged responses
is also how you spot a frozen feed - see
[`rt2s_archive_coverage`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_archive_coverage.md).
