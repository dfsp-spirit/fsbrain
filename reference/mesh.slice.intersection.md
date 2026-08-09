# Compute intersection of a triangular surface mesh with an axis-aligned plane.

For a given axis and coordinate, finds all triangles that straddle the
plane and computes the line segment where each crossing triangle
intersects the plane. The input surface must have vertices in the same
coordinate space as the slice plane definition (typically 0-based CRS).

## Usage

``` r
mesh.slice.intersection(surface, axis, slice_crs_coord)
```

## Arguments

- surface:

  an `fs.surface` instance with vertices in CRS space (0-based), as
  produced by
  [`mesh.ras2crs`](https://dfsp-spirit.github.io/fsbrain/reference/mesh.ras2crs.md).

- axis:

  integer, 1, 2, or 3. The axis perpendicular to the slice plane
  (1=sagittal, 2=coronal, 3=axial in CRS convention).

- slice_crs_coord:

  numeric scalar, the coordinate along `axis` where the slice plane is
  located. In CRS space (0-based).

## Value

a list of 2×2 numeric matrices. Each matrix represents one line segment
with rows `[start, end]` and columns `[axis_val_1, axis_val_2]` giving
the coordinates in the two axes orthogonal to the slice axis, in 0-based
CRS units. Returns an empty list if no triangles intersect the plane.
