# Access a single file from the package cache by its file name.

Access a single file from the package cache by its file name.

## Usage

``` r
get_optional_data_filepath(filename, mustWork = TRUE)
```

## Arguments

- filename, :

  string. The filename of the file in the package cache.

- mustWork, :

  logical. Whether an error should be created if the file does not
  exist. If mustWork=FALSE and the file does not exist, the empty string
  is returned.

## Value

string. The full path to the file in the package cache or the empty
string if there is no such file available. Use this in your application
code to open the file.
