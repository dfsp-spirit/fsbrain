# Compute slice indices from slice definition.

Compute slice indices from slice definition.

## Usage

``` r
get.slice.indices(voldim, axis, slices)
```

## Arguments

- voldim:

  integer vector, the dimension of the volume

- axis:

  integer, the axis

- slices:

  slice index definition. If a vector of integers, interpreted as slice
  indices. If a single negative interger `-n`, interpreted as every
  `nth` slice, starting at slice 1. The character string 'all' or the
  value `NULL` will be interpreted as *all slices*.

## Value

integer vector, the computed slice indices. They are guaranteed to be
valid indices into the volume.
