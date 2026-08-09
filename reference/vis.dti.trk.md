# Visualize DTI tracks from Diffusion Toolkit/TrackVis TRK format file.

Visualize DTI tracks from Diffusion Toolkit/TrackVis TRK format file.

## Usage

``` r
vis.dti.trk(
  trk,
  filter_tracks = list(min_length = 15, min_segment_count = 6),
  color_by_orientation = FALSE
)
```

## Arguments

- trk:

  character string, the path to a TRK file that should be loaded.
  Alternatively, a loaded `trk` instance as returned by
  [`freesurferformats::read.dti.trk`](https://rdrr.io/pkg/freesurferformats/man/read.dti.trk.html).

- filter_tracks:

  optional, named list of filters. Can contain fields `min_length` and
  `min_segment_count`. Set the whole thing to `NULL` or an entry to 0
  for no filtering.

- color_by_orientation:

  logical, whether to color the tracks by orientation. Slower, but may
  make the resulting visualization easier to interprete.

## Value

The (loaded or received) `trk` instance. Note that this function is
typically called for the side effect of visualization.

## Note

The current simple implementation is very slow if the number of tracks
becomes large (several thousand tracks).

## Examples

``` r
if (FALSE) { # \dontrun{
# Create the following file with Diffusion Toolkit from your DTI data:
trk = freesurferformats::read.dti.trk("~/data/tim_only/tim/DICOM/dti.trk");
vis.dti.trk(trk);
} # }
```
