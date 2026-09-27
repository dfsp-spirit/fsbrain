# Color line segments by their direction.

Computes the classic DTI orientation colors, i.e., segments running
left-right, anterior-posterior and superior-inferior get different
colors (red, green and blue for the 'axis' mode).

## Usage

``` r
segment.orientation.colors(from, to, mode = c("axis", "rgb"))
```

## Arguments

- from:

  matrix of segment start points, see
  [`fs.coloredpaths`](https://dfsp-spirit.github.io/fsbrain/reference/fs.coloredpaths.md).

- to:

  matrix of segment end points, same size as `from`.

- mode:

  character string, either 'axis' (every segment gets one fully
  saturated color, based on the dominant direction axis) or 'rgb' (the
  color channels are the absolute direction components, which gives
  smoother but paler colors).

## Value

vector of hex color strings, one per segment.
