# List the bundle files of a tract atlas directory.

List the bundle files of a tract atlas directory.

## Usage

``` r
list.tract.bundle.files(atlas_dir)
```

## Arguments

- atlas_dir:

  character string, the path to the directory.

## Value

vector of character strings, the paths of the bundle files (sorted), or
an empty vector if the directory does not exist or contains no bundle
files.

## Note

The file name filter is important: the downloaded archives contain macOS
resource fork files ('.\_AC.trk' and friends), which are not tract
files.
