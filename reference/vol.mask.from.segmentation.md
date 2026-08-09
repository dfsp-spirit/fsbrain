# Extract subset from a volume by value.

Extract subset from a volume by value, set all other voxel values to
`NA`. Typically used to extract a brain structure (like corpus callosum)
from a volume segmentation (like the `mri/aseg.mgz` file of a subject).
You should consider passing the volume and the include values as
integers.

## Usage

``` r
vol.mask.from.segmentation(volume, include_values)
```

## Arguments

- volume:

  numeric 3D array

- include_values:

  numerical vector, the intensity values which qualify a voxel to be
  part of the result (without being set to NA)

## Value

numerical array with same dimensions as the input volume. All values
which are not part of `include_values` replaced with `NA`.
