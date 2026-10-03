# Classify Times of Day into Named Service Windows

Assigns each time to a named window (e.g. "am_peak"), returning one
window name per input time. Use it for any time-of-day bucketing of
observed service; it is also the function that gives `windows=` its
meaning throughout the package, so a window definition that behaves as
you expect here behaves the same way in
[`rt2s_obs_headways`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_headways.md)
and
[`rt2s_frequencies`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_frequencies.md).

## Usage

``` r
rt2s_time_window(
  x,
  windows = NULL,
  service_date = NULL,
  tz = NULL,
  closed_last = FALSE,
  na_label = NA_character_
)
```

## Arguments

- x:

  A `POSIXct` vector (its own timezone is respected) or integer seconds
  since midnight.

- windows:

  Named list of length-2 character vectors `c(start, end)` in "HH:MM" or
  "HH:MM:SS", e.g.
  `list(am_peak = c("06:00", "09:00"), pm_peak = c("16:00", "19:00"))`.
  The window name `"other"` is reserved for unassigned service times.
  End values may exceed 24:00 for overnight windows, e.g.
  `c("22:00", "26:00")` (requires `service_date`). Intervals are
  half-open `[start, end)` - unless `closed_last = TRUE`, which closes
  the last window only - and the first matching window in list order
  wins, so overlaps resolve deterministically. `NULL` (default) places
  every non-missing time in a single `"all"` window.

- service_date:

  Optional `Date` vector (recycled to `x`). When supplied with POSIXct
  `x`, times are measured from the service day's GTFS origin (noon minus
  12h: midnight, except on a daylight-saving change day, where it keeps
  windows on the local clock; a time before that origin matches no
  window), so a post-midnight stop of a trip attributed to the previous
  service date classifies as e.g. 24:30 (88200 s) rather than 00:30 -
  the GTFS \>24:00:00 convention. Without it, POSIXct uses wall-clock
  time-of-day.

- tz:

  Timezone of the service day (defaults to `x`'s own timezone, else
  "UTC"). Only used when `service_date` is supplied.

- closed_last:

  Logical. When `FALSE` (default), every window is half-open
  `[start, end)`, so a time on the last window's end matches no window
  and becomes `"other"`. When `TRUE`, the *last window in list order* -
  and only that one - becomes closed, `[start, end]`; every earlier
  window stays half-open, so a time on an earlier window's end still
  belongs to whichever later window starts there. First match in list
  order still wins. Has no effect when `windows` is `NULL`. Use it when
  the configured windows are meant to cover a service span inclusive of
  its closing second.

- na_label:

  Length-1 character (`NA_character_` by default) used as the label
  wherever `x` is `NA`. The default reproduces the `NA` in / `NA` out
  behaviour. `"other"` is allowed - it is the unassigned label, not a
  window name - so `na_label = "other"` folds missing times in with
  times that match no window. It may not equal a window name. Applies in
  the `windows = NULL` branch too.

## Value

Character vector the length of `x`: the window name, `"other"` for a
non-missing time matching no window (never produced when `windows` is
`NULL`), and `na_label` - by default `NA` - where `x` is `NA`.

## Examples

``` r
rt2s_time_window(
  as.POSIXct(c("2026-07-14 07:30:00", "2026-07-14 12:00:00"), tz = "UTC"),
  list(am_peak = c("06:00", "09:00"))
)
#> [1] "am_peak" "other"  
# Overnight service attributed to the previous service date:
rt2s_time_window(
  as.POSIXct("2026-07-15 00:30:00", tz = "UTC"),
  list(overnight = c("22:00", "26:00")),
  service_date = as.Date("2026-07-14")
)
#> [1] "overnight"
# A departure on the last window's closing second, kept by closed_last:
rt2s_time_window(
  as.POSIXct("2026-07-14 23:00:00", tz = "UTC"),
  list(am_peak = c("06:00", "09:00"), pm_peak = c("16:00", "23:00")),
  closed_last = TRUE
)
#> [1] "pm_peak"
```
