# Retrieve values from nested named lists

Retrieve values from nested named lists

## Usage

``` r
getIn(named_list, listkeys, default = NULL)
```

## Arguments

- named_list:

  a named list

- listkeys:

  vector of character strings, the nested names of the lists

- default:

  the default value to return in case the requested value is `NULL`.

## Value

the value at the path through the lists, or `NULL` (or the 'default') if
no such path exists.

## Examples

``` r
   data = list("regions"=list("frontal"=list("thickness"=2.3, "area"=2345)));
   getIn(data, c("regions", "frontal", "thickness"));       # 2.3
#> [1] 2.3
   getIn(data, c("regions", "frontal", "nosuchentry"));     # NULL
#> NULL
   getIn(data, c("regions", "nosuchregion", "thickness"));  # NULL
#> NULL
   getIn(data, c("regions", "nosuchregion", "thickness"), default=14);  # 14
#> [1] 14
```
