# Download a white matter tract atlas (streamlines).

Download one of the tract atlases that are distributed as streamlines in
TrackVis TRK format, one file per white matter bundle. The bundle files
(and the provenance and attribution files that document them) are
downloaded into the fsbrain file cache, where
[`get_optional_data_filepath`](https://dfsp-spirit.github.io/fsbrain/reference/get_optional_data_filepath.md)
can be used to access them. The bundles can then be read with
[`read.tract.bundles`](https://dfsp-spirit.github.io/fsbrain/reference/read.tract.bundles.md)
and plotted with
[`vis.tracts`](https://dfsp-spirit.github.io/fsbrain/reference/vis.tracts.md).

## Usage

``` r
download_xtract_tracts(
  atlas = "xtract_medium",
  download = TRUE,
  scheme = "https",
  silent = FALSE
)
```

## Arguments

- atlas:

  character string, the atlas to download. One of 'xtract_tiny' (the
  smallest one, useful for quick tests), 'xtract_small', 'xtract_medium'
  (the default) or 'xtract_large' (the most detailed one).

- download:

  logical, whether to download the files if they are not in the cache.
  If FALSE, the function only reports the status of the files.

- scheme:

  character string, the URL scheme to use, see the 'scheme' parameter of
  [`download_fs_LR_32_meshes`](https://dfsp-spirit.github.io/fsbrain/reference/download_fs_LR_32_meshes.md).

- silent:

  logical, whether to suppress the progress messages.

## Value

named list. The list has the entries "available" (vector of character
strings, the paths of the bundle files that are available in the local
file cache) and "missing" (vector of character strings, the files that
could not be retrieved).

## Details

The atlases are the XTRACT atlas of the 42 major white matter tracts
(Warrington et al., 2020,
[doi:10.1126/sciadv.aba8245](https://doi.org/10.1126/sciadv.aba8245) ),
whose streamlines were converted from the probabilistic tract atlases
and are available in four levels of spatial detail. Note that the
bundles are defined in MNI152 space, while the fs_LR_32 and fsaverage
templates of fsbrain use a different (fsaverage-like) space: the two
spaces are very similar, so the tracts align with the template surfaces
well enough for visualization, but they are not identical. If you need
an exact alignment, register the template to MNI152 and pass the
resulting matrix to the parameter `transform_matrix` of
[`vis.tracts`](https://dfsp-spirit.github.io/fsbrain/reference/vis.tracts.md).

The files are hosted on the rcmd.org server of this project, in the same
way as the other optional data of the package, see
[`download_optional_data`](https://dfsp-spirit.github.io/fsbrain/reference/download_optional_data.md).
They are redistributed from the archives of the MIT licensed 'yabplot'
Python package, which uses the same streamlines; the attribution and
provenance files that come with them document the origin (see
`tracts/xtract.attribution.json` and the per-level
`tracts/xtract_<level>/xtract_<level>.provenance.json` in the file
cache). This data is not required for the package to work.

## Note

The levels of detail do not only differ in the number of streamlines,
but also in the set of bundles they contain: 'xtract_tiny' has 37
bundles, 'xtract_small' and 'xtract_medium' have 40, and 'xtract_large'
has 42 (the bundles 'SLF1_L' and 'Cing_PeriGen_L' are only present in
the large one). Since the bundles are selected by name (see the
parameter 'bundle_values' of
[`vis.tracts`](https://dfsp-spirit.github.io/fsbrain/reference/vis.tracts.md)),
data that is mapped to bundles has to match the atlas that is actually
used.

## See also

Other tracts functions:
[`read.tract.bundles()`](https://dfsp-spirit.github.io/fsbrain/reference/read.tract.bundles.md),
[`vis.tracts()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.tracts.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Download the small version of the XTRACT atlas:
  download_xtract_tracts("xtract_small");

  # The bundle files are now in the cache:
  atlas_dir = file.path(get_optional_data_filepath("tracts"), "xtract_small");
  list.files(atlas_dir);
} # }
```
