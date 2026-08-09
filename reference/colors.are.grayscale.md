# Check for the given color strings whether they represent gray scale colors.

Check for the given color strings whether they represent gray scale
colors.

## Usage

``` r
colors.are.grayscale(col_strings, accept_col_names = TRUE)
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
colors.are.grayscale(c("#FFFFFF", "#FF00FF"));
#> [1]  TRUE FALSE
all((colors.are.grayscale(c("#FFFFFF00", "#ABABABAB"))));
#> [1] TRUE
```
