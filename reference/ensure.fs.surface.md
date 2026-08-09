# Check whether parameter is an fs.surface instance.

Check whether parameter is an fs.surface instance.

## Usage

``` r
ensure.fs.surface(surface)
```

## Arguments

- surface:

  an fs.surface instance which will be returned as-is, a tmesh3d which
  will be converted to a surface using
  [`tmesh3d.to.fs.surface`](https://dfsp-spirit.github.io/fsbrain/reference/tmesh3d.to.fs.surface.md),
  or a character string which will be interpreted as a file system path
  and loaded with
  [`freesurferformats::read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html).
  Anything else will stop with an error.

## Value

an fs.surface instance, unless an error occurs.
