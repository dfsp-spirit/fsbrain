# Turn volume into an ImageMagick image stack.

Create an image from each slice along the axis, then stack those into an
ImageMagick image stack.

## Usage

``` r
vol.imagestack(volume, axis = 1L, intensity_scale = 255)
```

## Arguments

- volume:

  a 3D image volume. Can be numeric, or something that can be read
  directly by
  [`magick::image_read`](https://docs.ropensci.org/magick/reference/editing.html)
  in 2D matrices (slices along the axis), e.g., a 3D array of color
  strings. If a 2D matrix is passed, the resulting stack will contain a
  single image.

- axis:

  positive integer in range 1L..3L or an axis name, the axis to use.

- intensity_scale:

  integer, value by which to scale the intensities in the volume to the
  range `[0, 1]`. Only used for numeric volumes. Set to NULL for data
  that can be read directly by
  [`magick::image_read`](https://docs.ropensci.org/magick/reference/editing.html),
  and to 1 for intensity data that requires no scaling. Defaults to 255,
  which is suitable for 8 bit image data.

## Value

a vectorized ImageMagick image, containing one subimage per slice. This
can be interpreted as an animation or whatever.

## See also

Other volume utility:
[`vol.boundary.box()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.boundary.box.md),
[`vol.merge()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.merge.md),
[`vol.overlay.colors.from.activation()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.overlay.colors.from.activation.md),
[`vol.planes()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.planes.md),
[`vol.slice()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.slice.md)
