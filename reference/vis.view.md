# Render renderables for a static view and orient the camera (camera-based).

Renders the given renderables UNROTATED and orients the rgl camera so
the scene appears exactly as if the renderables had been rotated by
(rotation_angle, x, y, z) and then viewed from (theta, phi, fov=0). This
is the camera-based replacement for the legacy mesh-rotation approach
(`vis.rotated.coloredmeshes`); it is pixel-identical for the fsbrain
view angles (see tests/testthat/test-camera_unification.R).

## Usage

``` r
vis.view(
  renderables,
  style = "default",
  rotation_angle = 0,
  x = 1,
  y = 0,
  z = 0,
  theta = 0,
  phi = 0
)
```

## Arguments

- renderables:

  list of renderables (coloredmesh, coloredvoxels, Triangles3D) to
  render into the current scene.

- style:

  a rendering style, see
  [`get.rglstyle`](https://dfsp-spirit.github.io/fsbrain/reference/get.rglstyle.md).

- rotation_angle, :

  x, y, z the mesh rotation that the camera must reproduce (passed to
  [`rotation.matrix`](https://dfsp-spirit.github.io/fsbrain/reference/rotation.matrix.md)).

- theta, :

  phi the fixed-camera angles (passed to
  [`view3d`](https://dmurdoch.github.io/rgl/dev/reference/viewpoint.html)).
