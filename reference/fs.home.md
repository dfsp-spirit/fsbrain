# Return FreeSurfer path.

Return FreeSurfer path.

## Usage

``` r
fs.home()
```

## Value

the FreeSurfer path, typically what the environment variable
`FREESURFER_HOME` points to.

## Note

This function will stop (i.e., raise an error) if the directory cannot
be found.
