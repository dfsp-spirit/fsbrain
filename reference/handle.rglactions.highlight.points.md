# Highlight requested points (if any), for camera-based views.

Places the requested highlight points as spheres at their native
coordinates. Camera-based views keep the meshes unrotated and apply the
view rotation via the camera, so highlight points must NOT be rotated
(this matches the scimesh backend). See
dev_tools/TODO_FSBRAIN_RGL_CAM.md.

## Usage

``` r
handle.rglactions.highlight.points(rglactions, hemi = "both")
```

## Arguments

- hemi:

  character string, one of 'lh', 'rh' or 'both'. If lh or rh, plots only
  points from that hemi (if hemi info is available for the points).
