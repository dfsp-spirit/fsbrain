# Computes geometric curvature-based descriptors.

Computes geometric curvature-based descriptors.

## Usage

``` r
shape.descriptors(pc, descriptors = shape.descriptor.names())
```

## Arguments

- pc:

  a 'principal_curvatures' data list, see
  [`principal.curvatures`](https://dfsp-spirit.github.io/fsbrain/reference/principal.curvatures.md)
  for details.

- descriptors:

  vector of character strings, the descriptors you want. See
  [`shape.descriptor.names`](https://dfsp-spirit.github.io/fsbrain/reference/shape.descriptor.names.md)
  for all available names.

## Value

dataframe of descriptor values, each columns contains one descriptor.

## References

Shimony et al. (2016). Comparison of cortical folding measures for
evaluation of developing human brain. Neuroimage, 125, 780-790.
