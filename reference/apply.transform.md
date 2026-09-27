# Apply affine transformation to input.

Apply an affine transformation, like a *vox2ras_tkr* transformation, to
input. This is just matrix multiplication for different input objects.
Supported input types are coordinate vectors, coordinate matrices,
`fs.surface` meshes (the vertex coordinates are transformed, the face
indices stay the same), renderable objects like `fs.coloredmesh`,
`fs.coloredvoxels`, `fs.coloredpaths` or *misc3d* `Triangles3D`, rgl
`mesh3d`/`tmesh3d` instances (including their normals, if any), and
(hemi-)lists of such objects (which are transformed element-wise).

## Usage

``` r
apply.transform(object, matrix_fun)
```

## Arguments

- object:

  numerical vector/matrix, `fs.surface`, `fs.coloredmesh`,
  `fs.coloredvoxels`, `fs.coloredpaths`, `Triangles3D`,
  `mesh3d`/`tmesh3d` instance, or a list (e.g., a hemilist) of such
  objects, the coordinates or objects to transform.

- matrix_fun:

  a 4x4 affine matrix or a function returning such a matrix. If `NULL`,
  the input is returned as-is. In many cases you way want to use a
  matrix computed from the header of a volume file, e.g., the `vox2ras`
  matrix of the respective volume. See the `mghheader.*` functions in
  the *freesurferformats* package to obtain these matrices. Registration
  files can be read with
  [`freesurferformats::read.fs.transform`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.transform.html)
  and friends, but note that such files often describe a *voxel* to
  *surface RAS* mapping, so you may have to compose them with a
  `vox2ras` matrix to get a transformation between RAS coordinates.

## Value

the input after application of the affine matrix (matrix multiplication)

## Note

The affine matrix is applied in the standard way: the coordinates are
interpreted as homogeneous *column* vectors, i.e., a vertex `v` is
transformed as `v' = M %*% v`. Note that rgl, and fsbrain functions that
are implemented on top of rgl (like the camera transforms used
internally for views), use the transposed convention for their rotation
matrices, see
[`rotationMatrix`](https://dmurdoch.github.io/rgl/dev/reference/matrices.html).
For pure translations and scalings, both conventions are identical.

Meshes keep their orientation: if the linear part of the matrix has a
negative determinant, the transformation mirrors the object (this is the
case for the FreeSurfer `vox2ras_tkr` matrix, which flips and permutes
axes), which would invert all surface normals and make the mesh render
inside-out. In that case, the vertex order within each face is reversed
to preserve the original orientation, and the stored normals (if any)
are transformed with the linear part of the matrix so that they stay
consistent with the faces.

## Examples

``` r
if (FALSE) { # \dontrun{
   # Transform the vertex coordinates of a surface mesh:
   cube_file = system.file("extdata", "cube.ply", package = "fsbrain");
   cube = freesurferformats::read.fs.surface(cube_file);
   translation = matrix(c(1,0,0,10, 0,1,0,20, 0,0,1,30, 0,0,0,1), nrow = 4L, byrow = TRUE);
   cube_moved = apply.transform(cube, translation);
} # }
```
