# Determine whether colorbar can be plotted with given metadata.

Determine whether colorbar can be plotted with given metadata.

## Usage

``` r
can.plot.colorbar(combined_data_range, makecmap_options)
```

## Arguments

- combined_data_range:

  numerical vector of length 2, the combined data range of the meshes as
  returned by `coloredmeshes.combined.data.range`

- makecmap_options:

  the 'makecmap_options' from the metadata field of the 'coloredmeshes',
  see `coloredmeshes.get.md`

## Value

logical, whether the metadata suffices to plot a colorbar
