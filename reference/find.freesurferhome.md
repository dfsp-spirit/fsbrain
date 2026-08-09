# Find the FREESURFER_HOME directory on disk.

Try to find directory containing the FreeSurfer installation, based on
environment variables and *educated guessing*.

## Usage

``` r
find.freesurferhome(mustWork = FALSE)
```

## Arguments

- mustWork:

  logical. Whether the function should with an error stop if the
  directory cannot be found. If this is TRUE, the return value will be
  only the 'found_at' entry of the list (i.e., only the path of the
  FreeSurfer installation dir).

## Value

named list with the following entries: "found": logical, whether it was
found. "found_at": Only set if found=TRUE, the path to the FreeSurfer
installation directory (including the directory itself). See 'mustWork'
for important information.

## See also

[`fs.home`](https://dfsp-spirit.github.io/fsbrain/reference/fs.home.md)
