# Make Stop-Time Offsets Monotone

Rounds travel and dwell offsets to integer seconds, clamps negative
values to zero, and makes a stop pattern monotone with a forward pass.

## Usage

``` r
rt2s_monotone_offsets(travel, dwell)
```

## Arguments

- travel:

  Numeric vector of arrival offsets from trip start, in seconds.

- dwell:

  Numeric vector of dwell offsets, in seconds, with the same length as
  `travel`.

## Value

A list with integer vectors `arrival` and `departure`. Each departure is
its arrival plus its dwell, and each arrival is at least the previous
departure.

## Details

Inputs must be finite numeric vectors of equal length. Values are
rounded with [`base::round()`](https://rdrr.io/r/base/Round.html) before
negative values are clamped to zero. For each stop, the returned arrival
is the larger of its clamped travel offset and the previous returned
departure. This satisfies GTFS along-trip monotonicity even when
independently estimated offsets are out of order.

## Examples

``` r
rt2s_monotone_offsets(
  travel = c(-2.4, 300.6, 298.2),
  dwell = c(0, 30.6, 15)
)
#> $arrival
#> [1]   0 301 332
#> 
#> $departure
#> [1]   0 332 347
#> 
```
