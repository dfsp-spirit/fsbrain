# Draw a view label at a position, compensating for the camera rotation.

With camera-based views, geometry (and text) stays in world coordinates
while the camera applies the view rotation. To keep a label at the same
screen position relative to the (now unrotated) renderables as in the
legacy mesh-rotation views, the label offset is pre-rotated by the
view's mesh rotation matrix R_mesh.

## Usage

``` r
view_label3d(text, x, y, z, R_mesh = NULL)
```

## Arguments

- text:

  character string, the label text.

- x, :

  y, z numeric, the label offset in the rotated (view) frame.

- R_mesh:

  4x4 numeric rotation matrix, the view's mesh rotation (use
  [`rotation.matrix`](https://dfsp-spirit.github.io/fsbrain/reference/rotation.matrix.md)),
  or NULL for no rotation (e.g., the dorsal view).
