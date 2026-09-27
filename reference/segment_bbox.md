# Compute the bounding box of line segments.

Compute the bounding box of line segments.

## Usage

``` r
segment_bbox(from, to)
```

## Arguments

- from:

  matrix of segment start points.

- to:

  matrix of segment end points.

## Value

numeric vector of length 6: `c(xmin, xmax, ymin, ymax, zmin, zmax)`.
