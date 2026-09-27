# Extract the 3D data array of a volume.

Accepts a 3D array or an `fs.volume` instance, and selects the requested
frame of a 4D volume.

## Usage

``` r
shell.volume.data(volume, frame = 1L)
```

## Arguments

- volume:

  a 3D numerical array or an `fs.volume` instance.

- frame:

  positive integer, the frame to use for a 4D volume.

## Value

a 3D numerical array.
