# Transform surface vertices from surface RAS to 0-based volume CRS space.

Applies the inverse of the FreeSurfer vox2ras_tkr matrix to all surface
vertices, converting them from surface RAS coordinates to 0-based CRS
(column, row, slice) indices. The resulting CRS coordinates can be used
to directly index into a brain volume array (after adding 1 for R's
1-based indexing).

## Usage

``` r
mesh.ras2crs(surface)
```

## Arguments

- surface:

  an `fs.surface` instance, as returned by
  [`read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html).

## Value

a modified copy of the input surface with `vertices` in 0-based CRS
space.
