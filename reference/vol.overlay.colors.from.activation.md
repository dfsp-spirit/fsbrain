# Generate colors for a 3D volume, based on the activation data and a colormap.

Applies the colormap function to the data, then sets the alpha value
(transparency) to full in all areas without any activation. Feel free to
clip data or whatever before passing it, so that all your no-activation
data has the same value.

## Usage

``` r
vol.overlay.colors.from.activation(
  volume,
  colormap_fn = squash::blueorange,
  no_act_source_value = 0
)
```

## Arguments

- volume:

  a 3D array, the activation data (or p-values, effect sizes, or
  whatever)

- colormap_fn:

  function, a colormap function

- no_act_source_value:

  numerical scalar, the value from the data in 'volume' that means no
  activation. The output colors for this value will be set to `NA`. Set
  to NULL to not change anything.

## Value

a 3D matrix of color strings, with the same dimensions as the input
volume

## See also

Other volume utility:
[`vol.boundary.box()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.boundary.box.md),
[`vol.imagestack()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.imagestack.md),
[`vol.merge()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.merge.md),
[`vol.planes()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.planes.md),
[`vol.slice()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.slice.md)
