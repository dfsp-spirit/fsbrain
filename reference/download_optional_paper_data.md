# Download extra data to reproduce the figures from the fsbrain paper.

Download extra data to reproduce the figures from the fsbrain paper.

## Usage

``` r
download_optional_paper_data(scheme = "https")
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

## Note

Called for side effect of data download.
