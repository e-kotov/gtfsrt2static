# Validate an Observed Stop Events Table

Checks that a table conforms to the observed stop events schema (see
[observed-stop-events](https://e-kotov.github.io/gtfsrt2static/reference/observed-stop-events.md)).
Called internally by all converters and by the assembler; exported so
external producers can verify their own tables.

## Usage

``` r
rt2s_events_validate(events)
```

## Arguments

- events:

  A data.frame/data.table of observed stop events.

## Value

The validated events table as a keyed data.table (invisibly usable in
pipes); errors describe the first violation found.
