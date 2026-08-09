# Check for values in nested named lists

Check for values in nested named lists

## Usage

``` r
hasIn(named_list, listkeys)
```

## Arguments

- named_list:

  a named list

- listkeys:

  vector of character strings, the nested names of the lists

## Value

whether a non-NULL value exists at the path

## Examples

``` r
   data = list("regions"=list("frontal"=list("thickness"=2.3, "area"=2345)));
   hasIn(data, c("regions", "nosuchregion"));   # FALSE
#> [1] FALSE
```
