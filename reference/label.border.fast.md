# Compute border vertices of a label using Rvcg.

Compute border vertices of a label using Rvcg.

## Usage

``` r
label.border.fast(surface_mesh, label)
```

## Arguments

- surface_mesh:

  an fs.surface instance, see
  [`subject.surface`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md).

- label:

  an fs.label instance (see
  [`subject.label`](https://dfsp-spirit.github.io/fsbrain/reference/subject.label.md))
  or an integer vector, the vertex indices of the label.

## Value

named list with entry 'vertices' containing an integer vector with the
indices of the border vertices.

## Note

This is faster than using the
[`label.border`](https://dfsp-spirit.github.io/fsbrain/reference/label.border.md)
function, but it does not fully match its functionality (some parameter
are not implemented for this function), and it requires the `Rvcg`
package, which is an optional dependency.

## See also

[`label.border`](https://dfsp-spirit.github.io/fsbrain/reference/label.border.md),
which is slower but provides more options and does not require Rvcg.

## Examples
