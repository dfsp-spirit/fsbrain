# Filter a scimesh scene to the meshes visible from a given view

Filter a scimesh scene to the meshes visible from a given view

## Usage

``` r
filter_scene_by_view(scene, hemi_filter)
```

## Arguments

- scene:

  a named list of scimesh mesh descriptors (with "lh" and/or "rh"
  entries).

- hemi_filter:

  character string, one of "lh", "rh", or "both".

## Value

a flat list of scimesh mesh descriptors for the given view.
