# Check for the given color strings whether they have transparency, i.e., an alpha channel value != fully opaque.

Check for the given color strings whether they have transparency, i.e.,
an alpha channel value != fully opaque.

## Usage

``` r
colors.have.transparency(col_strings, accept_col_names = TRUE)
```

## Arguments

- col_strings:

  vector of RGB(A) color strings, like `c("#FFFFFF", ("#FF00FF"))`.

- accept_col_names:

  logical, whether to accept color names like 'white'. Disables all
  sanity checks.

## Value

logical vector

## Examples

``` r
colors.have.transparency(c("#FFFFFF", "#FF00FF", "#FF00FF00", "red", "#FF00FFDD"));
#> [1] FALSE FALSE  TRUE FALSE  TRUE
all((colors.have.transparency(c("#FFFFFF00", "#ABABABAB"))));
#> [1] TRUE
```
