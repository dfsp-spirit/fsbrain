# Compute simple version of center and radius of 2 meshes.

Compute simple version of center and radius of 2 meshes.

## Usage

``` r
surfs.props(lh, rh)
```

## Arguments

- lh:

  `fs.surface` instance, the mesh for the left hemisphere. If a string,
  assumed to be the path to a surface mesh file that should be loaded.

- rh:

  `fs.surface` instance, the mesh for the right hemisphere. If a string,
  assumed to be the path to a surface mesh file that should be loaded.

## Value

named list with entries 'center', 'radius', 'min' and 'max', each of
which are numerical vectors of length 3, holding the x, y and z
coordinates or values.

## Note

This function treats the 'lh' and 'rh' meshes as a single mesh, and
computes the properties for this combined mesh.
