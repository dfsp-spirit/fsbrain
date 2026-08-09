# Get cmap and colorlayer from data and makecmap_options.

Applies a requested 'range' setting if present in makecmap_options. A
shared colormap is used for the data of both hemispheres (if present).

## Usage

``` r
common.makecmap.range(
  makecmap_options,
  lh_data = NULL,
  rh_data = NULL,
  return_metadata = FALSE
)
```

## Arguments

- makecmap_options:

  named list of parameters to pass to
  [`makecmap`](https://rdrr.io/pkg/squash/man/makecmap.html). Must not
  include the unnamed first parameter, which is derived from 'measure'.

- lh_data:

  numeric vector, data for left hemisphere.

- rh_data:

  numeric vector, data for right hemisphere.

- return_metadata:

  logical, whether to return additional metadata as entry 'metadata' in
  the returned list

## Value

named list, with entries 'map': named list, the squash cmap, and
'collayer': hemilist of color vectors
