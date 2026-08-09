# Perform NA mapping for transparency

Usually this is done so that the `NA` values are plotted transparently,
so your can see a background color through the respective colors.

## Usage

``` r
perform.na.mapping(data, map_to_NA)
```

## Arguments

- data:

  numeric vector

- map_to_NA:

  the value or value range that should be mapped to `NA`. If a single
  value, only exactly this value is used. If two values, they are
  interpreted as a range, and a values between them are mapped to `NA`.
  If you pass `NULL`, the data are returned as-is.

## Value

the mapped data
