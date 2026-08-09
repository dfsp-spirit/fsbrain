# Try to extract a 3D volume from the input argument.

Check whether it already is such an array, whether it is a filename that
can be loaded with
[`freesurferformats::read.fs.volume`](https://rdrr.io/pkg/freesurferformats/man/read.fs.volume.html)
into such an array, etc.

## Usage

``` r
extract.volume.3D(stats, silent = getOption("fsbrain.silent", default = FALSE))
```

## Arguments

- stats:

  a 3D array, 4D array or a string that can be treated as a filename to
  a volume image containing such an array.

- silent:

  logical, whether to suppress file reading messages.

## Value

the obtained 3D array

## Note

This function stops with an error if the input cannot be returned as a
3D array.
