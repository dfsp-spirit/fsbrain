# Download label files for the fs_LR 32k template.

Download a set of label files for the fs_LR 32k template (the HCP-style
surface space), based on a declarative manifest file shipped with this
package. Currently this is the cortex label (`?h.cortex.label`), which
defines the medial wall: all vertices which are *not* part of the label
are medial wall vertices. It can be used to mask the medial wall when
projecting volume data to the fs_LR 32k surface, see
[`template.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/template.vol2surf.md)
(parameter `cortex_only`). This data is not required for the package to
work.

## Usage

``` r
download_fs_LR_32_labels(scheme = "https")
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

## See also

Other fs_LR 32k template functions:
[`download_fs_LR_32_atlases()`](https://dfsp-spirit.github.io/fsbrain/reference/download_fs_LR_32_atlases.md),
[`download_fs_LR_32_meshes()`](https://dfsp-spirit.github.io/fsbrain/reference/download_fs_LR_32_meshes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_fs_LR_32_labels();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   cortex_lh = subject.label(subjects_dir, "fs_LR_32", "cortex.label", "lh");
} # }
```
