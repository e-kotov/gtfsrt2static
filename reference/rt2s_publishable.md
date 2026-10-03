# Publish-Readiness of an Assembled Feed

Reports whether
[`rt2s_scaffold`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_scaffold.md)
/
[`rt2s_assemble`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_assemble.md)
produced a spec-compliant feed, and if not, why. This is the
programmatic counterpart to the assembly warnings: a publish/export step
can gate on it instead of relying on a human noticing a warning.

## Usage

``` r
rt2s_publishable(feed)
```

## Arguments

- feed:

  A feed object returned by
  [`rt2s_scaffold()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_scaffold.md)
  or
  [`rt2s_assemble()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_assemble.md).

## Value

A list with `publishable` (logical) and `blockers` (character vector;
empty when publishable). A feed with blockers is still a valid R object
for inspection/analysis - it just should not be published as standard
GTFS until the blockers are resolved (or built with `strict = TRUE`,
which refuses to produce one).

## Examples

``` r
if (FALSE) { # \dontrun{
feed <- rt2s_scaffold(events)
status <- rt2s_publishable(feed)
if (!status$publishable) stop(paste(status$blockers, collapse = "; "))
} # }
```
