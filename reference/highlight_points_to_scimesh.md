# Convert highlight points (rglactions) to scimesh sphere meshes

Convert highlight points (rglactions) to scimesh sphere meshes

## Usage

``` r
highlight_points_to_scimesh(rglactions, hemi_filter = "both")
```

## Arguments

- rglactions:

  named list; the entry 'highlight_points' is used if present.

- hemi_filter:

  character string, one of "lh", "rh", or "both".

## Value

list of scimesh mesh descriptors (spheres), possibly empty.
