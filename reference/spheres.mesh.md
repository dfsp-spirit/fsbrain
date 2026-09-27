# Compute the mesh of a set of spheres.

Merges one slightly refined icosphere per sphere into a single mesh,
i.e., the result is a single surface with one connected component per
input sphere. This is much cheaper to render (and to convert for the
scimesh backend) than a list of hundreds of separate sphere meshes.

## Usage

``` r
spheres.mesh(centers, radii = 1, subdivisions = 2L)
```

## Arguments

- centers:

  n x 3 numeric matrix, the sphere centers.

- radii:

  numeric vector of length n or a single number, the sphere radii.

- subdivisions:

  non-negative integer, the number of subdivisions of the unit
  icosphere, see
  [`unit.icosphere`](https://dfsp-spirit.github.io/fsbrain/reference/unit.icosphere.md).
  Defaults to 2L.

## Value

an fs.surface instance (a mesh) with one component per sphere.

## Examples

``` r
  m = fsbrain:::spheres.mesh(rbind(c(0, 0, 0), c(10, 0, 0)), c(1, 2));
  nrow(m$vertices);
#> [1] 324
```
