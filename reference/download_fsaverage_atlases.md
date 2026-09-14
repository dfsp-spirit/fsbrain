# Download atlas files for the fsaverage template subject.

Download a set of cortical atlas files (annotations) defined in the
space of the fsaverage template subject, based on a declarative manifest
file shipped with this package. The atlases (e.g., Schaefer 100-1000,
Brainnetome, HCP-MMP1, AAL3) are defined in fsaverage space, but they
are not part of FreeSurfer and are not subject to the FreeSurfer
license. This data is not required for the package to work.

## Usage

``` r
download_fsaverage_atlases(scheme = "https")
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
