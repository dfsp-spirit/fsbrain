# Highlight requested points (if any), but apply given view rotation before doing so.

Highlight requested points (if any), but apply given view rotation
before doing so.

## Usage

``` r
handle.rglactions.highlight.points(
  rglactions,
  angle_rad,
  x,
  y,
  z,
  hemi = "both"
)
```

## Arguments

- hemi:

  character string, one of 'lh', 'rh' or 'both'. If lh or rh, plots only
  points from that hemi (if hemi info is available for the points).
