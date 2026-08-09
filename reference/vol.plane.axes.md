# Get indices of the axes defining the given plane.

When using plane names, this function assumes that the volume is in the
standard FreeSurfer orientation, as returned by reading a conformed
volume with functions like
[`subject.volume`](https://dfsp-spirit.github.io/fsbrain/reference/subject.volume.md).

## Usage

``` r
vol.plane.axes(plane)
```

## Arguments

- plane:

  integer or string. If a string, one of "axial", "coronal", or
  "sagittal". If this is an integer vector of length 2 already, it is
  returned as given. If it is a single integer, it is interpreted as an
  axis index, and the plane orthogonal to the axis is returned. A
  warning on using the plane names: these only make sense if the volume
  is in the expected orientation, no checking whatsoever on this is
  performed.

## Value

integer vector of length 2, the axes indices.
