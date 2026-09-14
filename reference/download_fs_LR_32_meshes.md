# Download surface meshes for the fs_LR 32k template.

Download a set of surface mesh files for the fs_LR 32k template (the
HCP-style surface space), based on a declarative manifest file shipped
with this package. The meshes include the white, pial, inflated, very
inflated, midthickness, sphere and flat surfaces for both hemispheres.
This data is not required for the package to work.

## Usage

``` r
download_fs_LR_32_meshes(scheme = "https")
```

## Arguments

- scheme:

  character string, the URL scheme to use. Either `"https"` (the
  default) or `"http"`. Switching to `"http"` can be useful as a
  fallback if the HTTPS server is unreachable.

## Value

Named list. The list has entries: "available": vector of strings. The
names of the files that are available in the local file cache. You can
access them using get_optional_data_filepath(). "missing": vector of
strings. The names of the files that this function was unable to
retrieve.
