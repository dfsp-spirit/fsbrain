# Return path to fsaverage dir.

Return path to fsaverage dir.

## Usage

``` r
fsaverage.path(allow_fetch = FALSE)
```

## Arguments

- allow_fetch:

  logical, whether to allow trying to download it.

## Value

the path to the fsaverage directory (NOT including the 'fsaverage' dir
itself).

## Note

This function will stop (i.e., raise an error) if the directory cannot
be found. The fsaverage template is part of FreeSurfer, and distributed
under the FreeSurfer software license.
