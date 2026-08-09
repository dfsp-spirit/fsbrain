# Compute voxel colors based on colortable.

Use the intensity values of the voxels in volume and lookup the
respective colors in a colortable.

## Usage

``` r
vol.overlay.colors.from.colortable(
  volume,
  colortable,
  ignored_struct_indices = c(),
  ignored_struct_names = c("unknown", "Unknown")
)
```

## Arguments

- volume:

  numeric 3D array, the values should be integers present in the
  `struct_index` column of the colortable. All other values will be
  assigned `NA` as a color.

- colortable:

  a colortable, as returned by
  [`read.fs.colortable`](https://rdrr.io/pkg/freesurferformats/man/read.fs.colortable.html),
  or a character string representing a path to a colortable file.

- ignored_struct_indices:

  integer vector, `struct_index` entries in the colortable that should
  be ignored

- ignored_struct_names:

  vector of character strings, `struct_name` entries in the colortable
  that should be ignored. Can be combined with `ignored_struct_indices`.

## Value

character string 3D array, the colors. Voxels in the volume which were
not matched by the colortable are set to `NA` in it.
