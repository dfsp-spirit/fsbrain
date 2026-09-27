# Map values to a color layer and the matching colorbar metadata.

Map values to a color layer and the matching colorbar metadata.

## Usage

``` r
values.to.colorlayer(values, makecmap_options)
```

## Arguments

- values:

  numeric vector, the values to map to colors.

- makecmap_options:

  named list of parameters to pass to
  [`makecmap`](https://rdrr.io/pkg/squash/man/makecmap.html), see
  [`mkco.seq`](https://dfsp-spirit.github.io/fsbrain/reference/mkco.seq.md).
  Supported entries are 'colFn', 'n', 'symm' and 'col.na'.

## Value

named list with entries 'colors' (the hex color strings for the values)
and 'makecmap_options' (the input options, extended with the effective
range, for use as colorbar metadata).

## Note

The colors are computed from the color ramp that the colorbar uses for
the same options, i.e., the values are binned into the interval
boundaries of a ramp of 'n' colors over the value range, so the colorbar
always matches the data. Values which are not finite (NA/NaN/Inf) get
the color 'col.na'.
