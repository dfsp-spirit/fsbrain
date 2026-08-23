# Map an fsbrain view angle to a scimesh camera

Translates an fsbrain view_angle string (e.g., "lateral_lh", "dorsal",
etc.) to a scimesh camera specification. The returned object includes
the camera list and a hemi_filter field indicating which hemispheres to
render for this view.

## Usage

``` r
view_angle_to_scimesh_camera(scene, view_angle)
```

## Arguments

- scene:

  a named list of scimesh mesh descriptors with "lh" and/or "rh"
  entries, as returned by `coloredmeshes_to_scimesh`.

- view_angle:

  character string, a valid view angle. See
  [`get.view.angle.names`](https://dfsp-spirit.github.io/fsbrain/reference/get.view.angle.names.md)
  for all valid options.

## Value

a list with entries: `camera` (scimesh camera list from `camera_auto`),
and `hemi_filter` (one of "lh", "rh", or "both").
