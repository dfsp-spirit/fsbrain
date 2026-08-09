# Compute the border vertices for each region in an annot.

Compute the border vertices for each region in an annot.

## Usage

``` r
annot.outline.border.vertices(
  annotdata,
  surface_mesh,
  silent = TRUE,
  expand_inwards = 0L,
  limit_to_regions = NULL
)
```

## Arguments

- annotdata:

  an annotation, as returned by functions like
  [`subject.annot`](https://dfsp-spirit.github.io/fsbrain/reference/subject.annot.md).
  If a character string, interpreted as a path to a file containing such
  data, and loaded with
  [`freesurferformats::read.fs.annot`](https://rdrr.io/pkg/freesurferformats/man/read.fs.annot.html)

- surface_mesh:

  brain surface mesh, as returned by functions like
  [`subject.surface`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md)
  or
  [`read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html).
  If a character string, interpreted as a path to a file containing such
  data, and loaded with
  [`freesurferformats::read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html)

- silent:

  logical, whether to suppress status messages.

- expand_inwards:

  integer, additional thickness of the borders. Increases computation
  time, defaults to 0L.

- limit_to_regions:

  vector of character strings or NULL, a list of regions for which to
  draw the outline (see
  [`get.atlas.region.names`](https://dfsp-spirit.github.io/fsbrain/reference/get.atlas.region.names.md)).
  If NULL, all regions will be used. If (and only if) this parameter is
  used, the 'outline_color' parameter can be a vector of color strings,
  one color per region.

## Value

named list, the keys are the region names and the values are vectors of
integers encoding vertex indices.
