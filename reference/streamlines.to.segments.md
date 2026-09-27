# Compute the line segments of streamlines.

Converts streamlines (a list of point sequences) into the pair of point
matrices that describes the line segments between their consecutive
points, which is the representation used by
[`fs.coloredpaths`](https://dfsp-spirit.github.io/fsbrain/reference/fs.coloredpaths.md).
This is vectorized: the segments of all streamlines are computed in one
go, no matter how many there are.

## Usage

``` r
streamlines.to.segments(tracts)
```

## Arguments

- tracts:

  an `fs.tracts` instance (as returned in the `tracks` entry of
  [`freesurferformats::read.dti.trk`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.trk.html)
  and
  [`freesurferformats::read.dti.tck`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.tck.html)),
  an (n, 3) matrix of points (a single streamline), or a list of (n, 3)
  matrices (one per streamline).

## Value

named list with entries `from` (matrix of segment start points), `to`
(matrix of segment end points) and `lengths` (integer vector, the number
of segments of each streamline).
