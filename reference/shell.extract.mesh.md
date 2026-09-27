# Extract the iso-surface mesh of a volume at one level.

The mesh is returned in the voxel space of the volume, using 1-based R
array indices (i.e., the first voxel of the volume is at index 1), no
matter which backend is used. Use
[`index2ras_tkr`](https://dfsp-spirit.github.io/fsbrain/reference/index2ras_tkr.md)
(not
[`vox2ras_tkr()`](https://dfsp-spirit.github.io/fsbrain/reference/vox2ras_tkr.md),
which expects 0-based CRS indices) to transform such a mesh to surface
RAS. The result is welded and free of degenerate faces, see
[`mesh.weld`](https://dfsp-spirit.github.io/fsbrain/reference/mesh.weld.md).

## Usage

``` r
shell.extract.mesh(volume, level, backend)
```

## Arguments

- volume:

  a 3D numerical array, in voxel space.

- level:

  numerical, the iso-level.

- backend:

  character string, 'Rvcg' or 'misc3d'.

## Value

a `mesh3d` instance with normals, in the voxel space of the volume, or
`NULL` if the level is not within the range of the data.
