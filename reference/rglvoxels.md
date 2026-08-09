# Draw 3D boxes at locations using rgl.

Draw 3D boxes at all given coordinates using rgl, analogous to
[`spheres3d`](https://dmurdoch.github.io/rgl/dev/reference/spheres.html).
Constructs the coordinates for triangles making up the boxes, then uses
[`triangles3d`](https://dmurdoch.github.io/rgl/dev/reference/primitives.html)
to render them.

## Usage

``` r
rglvoxels(centers, r = 1, voxelcol = NULL, do_show = TRUE, ...)
```

## Arguments

- centers:

  numerical matrix with 3 columns. Each column represents the x, y, z
  coordinates of a center at which to create a cube.

- r:

  numerical vector or scalar, the cube edge length. This is the length
  of the axis-parallel edges of the cube. The vector must have length 1
  (same edge length for all cubes), or the length must be identical to
  the number of rows in parameter `centers`.

- voxelcol:

  vector of rgb color strings for the individual voxels. Its length must
  be identical to `nrow(centers)` if given.

- do_show:

  logical, whether to visualize the result in the current rgl scene

- ...:

  material properties, passed to
  [`triangles3d`](https://dmurdoch.github.io/rgl/dev/reference/primitives.html).
  Example: `color = "#0000ff", lit=FALSE`.

## Value

list of `fs.coloredvoxels` instances, invisible. The function is called
for the side effect of visualizing the data, and usually you can ignore
the return value.

## Examples

``` r
if (FALSE) { # \dontrun{
   # Plot a 3D cloud of 500 red voxels:
   centers = matrix(rnorm(500*3)*100, ncol=3);
   rglvoxels(centers, voxelcol="red");
} # }
```
