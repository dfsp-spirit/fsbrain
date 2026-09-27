# Normalize a region name for matching.

Removes all characters which are not letters or digits and lowercases
the result, so that region names which differ only in their formatting
(like 'LH_Vis_1' and 'lh vis 1') can be matched.

## Usage

``` r
normalize.region.names(x)
```

## Arguments

- x:

  vector of character strings.

## Value

vector of character strings, the normalized names.
