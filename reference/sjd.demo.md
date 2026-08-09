# Download optional demo data if needed and return its path.

This is a wrapper around
[`download_optional_data()`](https://dfsp-spirit.github.io/fsbrain/reference/download_optional_data.md)
and `get_optional_data_filepath("subjects_dir")`. It will download the
optional fsbrain demo data unless it already exists locally.

## Usage

``` r
sjd.demo(accept_freesurfer_license = FALSE)
```

## Arguments

- accept_freesurfer_license:

  logical, whether you want to also download fsaverage and fsaverage3,
  and accept the FreeSurfer license for fsaverage and fsaverage3,
  available at
  https://surfer.nmr.mgh.harvard.edu/fswiki/FreeSurferSoftwareLicense.
  Defaults to FALSE. If FALSE, only the demo data from fsbrain itself
  ('subject1') will be downloaded.

## Value

character string, the path to the 'subjects_dir' directory within the
downloaded optional data directory.

## Note

This function will stop if the data cannot be accessed, i.e., the
'subjects_dir' does not exist after trying to download the data.
