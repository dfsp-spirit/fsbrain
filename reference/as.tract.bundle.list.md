# Turn the various supported tract inputs into a list of bundles.

Turn the various supported tract inputs into a list of bundles.

## Usage

``` r
as.tract.bundle.list(
  tracts,
  coords = "ras",
  transform_matrix = NULL,
  max_tracks = Inf,
  skip_tracks = 0L,
  bbox = NULL,
  silent = FALSE
)
```

## Arguments

- tracts:

  see
  [`vis.tracts`](https://dfsp-spirit.github.io/fsbrain/reference/vis.tracts.md).

- coords, transform_matrix, max_tracks, skip_tracks, bbox, silent:

  parameters passed on to
  [`read.tract.bundles`](https://dfsp-spirit.github.io/fsbrain/reference/read.tract.bundles.md)
  if `tracts` is a file path or directory.

## Value

named list of `fs.tracts` instances.
