# Compute outline vertex colors from annotation.

For each region in an atlas, compute the outer border and color the
respective vertices in the region-specific color from the annot's
colortable.

## Usage

``` r
annot.outline(
  annotdata,
  surface_mesh,
  background = "white",
  silent = TRUE,
  expand_inwards = 0L,
  outline_color = NULL,
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

- background:

  color, the background color to assign to the non-border parts of the
  regions. Defaults to 'white'.

- silent:

  logical, whether to suppress status messages.

- expand_inwards:

  integer, additional thickness of the borders. Increases computation
  time, defaults to 0L.

- outline_color:

  NULL or a color string (like 'black' or '#000000'), the color to use
  for the borders. If left at the default value `NULL`, the colors from
  the annotation color lookup table will be used.

- limit_to_regions:

  vector of character strings or NULL, a list of regions for which to
  draw the outline (see
  [`get.atlas.region.names`](https://dfsp-spirit.github.io/fsbrain/reference/get.atlas.region.names.md)).
  If NULL, all regions will be used. If (and only if) this parameter is
  used, the 'outline_color' parameter can be a vector of color strings,
  one color per region.

## Value

vector of colors, one color for each mesh vertex

## Note

Sorry for the computational time, the mesh datastructure is not ideal
for neighborhood search.
