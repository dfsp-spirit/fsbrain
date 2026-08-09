# Return the proper hemi string ('lh' or 'rh') for each vertex.

Return the proper hemi string ('lh' or 'rh') for each vertex.

## Usage

``` r
vertex.hemis(surface, vertices)
```

## Arguments

- surface:

  hemilist of surfaces or a single integer which will be interpreted as
  the vertex count of the left hemisphere.

- vertices:

  vector of positive integers, the query vertex indices. Can be in range
  `1..(nv(lh)+nv(rh))`, i.e., across the whole brain.

## Value

vector of character strings, each string is 'lh' or 'rh'.

## Note

It is not checked in any way whether the vertex indices are out of
bounds on the upper side (higher than the highest rh vertex index).

## Examples

``` r
  vertex.hemis(100L, vertices=c(99L, 100L, 101L));
#> [1] "lh" "lh" "rh"
```
