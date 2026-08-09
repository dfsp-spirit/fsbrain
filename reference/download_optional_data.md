# Download optional data for this package if required.

Ensure that the optioanl data is available locally in the package cache.
Will try to download the data only if it is not available. This data is
not required for the package to work, but it is used in the examples, in
the unit tests and also in the example code from the vignette.
Downloading it is highly recommended.

## Usage

``` r
download_optional_data(scheme = "https")
```

## Arguments

- scheme:

  character string, the URL scheme to use. Either `"https"` (the
  default) or `"http"`. Switching to `"http"` can be useful as a
  fallback if the HTTPS server is unreachable.

## Value

Named list. The list has entries: "available": vector of strings. The
names of the files that are available in the local file cache. You can
access them using get_optional_data_file(). "missing": vector of
strings. The names of the files that this function was unable to
retrieve.
