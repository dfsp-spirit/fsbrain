# Scale values into a given range.

Linearly maps the values to the target range. Values which are all
identical (or a single value) are mapped to the middle of the range.

## Usage

``` r
values.to.range(x, target_range)
```

## Arguments

- x:

  numeric vector, the input values.

- target_range:

  numeric vector of length 2, the target range.

## Value

numeric vector of the same length as `x`, the scaled values.
