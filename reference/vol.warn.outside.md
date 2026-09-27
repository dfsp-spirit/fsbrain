# Warn about surface vertices which are outside the volume.

Warn about surface vertices which are outside the volume.

## Usage

``` r
vol.warn.outside(outside, num_verts, clamp)
```

## Arguments

- outside:

  named list of logical vectors, per hemisphere (or NULL entries if the
  check was disabled).

- num_verts:

  named list of integer scalars, the number of vertices per hemisphere.

- clamp:

  logical, whether outside vertices were clamped to the volume border
  instead of being set to NA.

## Value

NULL, called for the side effect of emitting a warning.
