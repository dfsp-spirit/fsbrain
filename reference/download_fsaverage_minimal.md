# Download only essential FreeSurfer v6 fsaverage files for quick visualization.

Download a minimal subset of files from the FreeSurfer v6 fsaverage
subject, containing only the white surfaces, cortex labels, and
curvature files needed for basic 3D visualization (e.g., in a Shiny
app). This is much faster than
[`download_fsaverage`](https://dfsp-spirit.github.io/fsbrain/reference/download_fsaverage.md)
which downloads 19 files. The files are subject to the FreeSurfer
software license, see parameter 'accept_freesurfer_license' for details.

## Usage

``` r
download_fsaverage_minimal(accept_freesurfer_license = FALSE, scheme = "https")
```

## Arguments

- accept_freesurfer_license:

  logical, whether you accept the FreeSurfer license for fsaverage,
  available at
  https://surfer.nmr.mgh.harvard.edu/fswiki/FreeSurferSoftwareLicense.
  Defaults to FALSE.

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

This downloads approximately 8 files instead of the 19 downloaded by
`download_fsaverage`, making it suitable for environments with startup
time constraints (e.g., Posit Connect Cloud, shinyapps.io).
