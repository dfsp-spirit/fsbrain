# Merge several labels into an annotation

Merge several labels and a colortable into an annotation.

## Usage

``` r
label.to.annot(
  label_vertices_by_region,
  num_vertices_in_surface,
  colortable_df = NULL,
  index_of_unknown_region = 1L
)
```

## Arguments

- label_vertices_by_region:

  named list of integer vectors, the keys are strings which define
  region names, and the values are integer vectors: the vertex indices
  of the region.

- num_vertices_in_surface:

  integer, total number of vertices in the surface mesh

- colortable_df:

  NULL or dataframe, a colortable. It must contain the columns
  'struct_name', 'r', 'g', 'b', and 'a'. All other columns will be
  derived if missing. The entries in 'struct_name' must match keys from
  the 'label_vertices_by_region' parameter. There must be one more row
  in here than there are labels. This row identifies the 'unknown'
  region (see also parameter 'index_of_unknown_region'). If NULL, a
  colortable will be auto-generated.

- index_of_unknown_region:

  positive integer, the index of the row in 'colortable_df' that defines
  the 'unknown' or 'background' region to which all vertices will be
  assigned which are *not* part of any of the given labels.

## Value

an annotation, see
[`read.fs.annot`](https://rdrr.io/pkg/freesurferformats/man/read.fs.annot.html)
for details.

## See also

Other atlas functions:
[`get.atlas.region.names()`](https://dfsp-spirit.github.io/fsbrain/reference/get.atlas.region.names.md),
[`group.agg.atlas.native()`](https://dfsp-spirit.github.io/fsbrain/reference/group.agg.atlas.native.md),
[`group.agg.atlas.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/group.agg.atlas.standard.md),
[`group.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/group.annot.md),
[`group.label.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/group.label.from.annot.md),
[`label.from.annotdata()`](https://dfsp-spirit.github.io/fsbrain/reference/label.from.annotdata.md),
[`regions.to.ignore()`](https://dfsp-spirit.github.io/fsbrain/reference/regions.to.ignore.md),
[`spread.values.over.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/spread.values.over.annot.md),
[`spread.values.over.hemi()`](https://dfsp-spirit.github.io/fsbrain/reference/spread.values.over.hemi.md),
[`spread.values.over.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/spread.values.over.subject.md),
[`subject.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.annot.md),
[`subject.atlas.agg()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.atlas.agg.md),
[`subject.label.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.label.from.annot.md),
[`subject.lobes()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.lobes.md)

## Examples

``` r
  # Create two labels. Real-word labels would have more vertices, of course.
  label1 = c(46666, 467777);
  label2 = c(99888, 99889);
  label_vertices = list("region1"=label1, "region2"=label2);
  colortable_df = data.frame("struct_index"=seq(0, 2),
   "struct_name"=c("unknown", "region1", "region2"),
   "r"=c(255L, 255L, 0L), "g"=c(255L, 0L, 255L), "b"=c(255L, 0L, 0L), "a"=c(0L, 0L, 0L));
  annot = label.to.annot(label_vertices, 100000, colortable_df);
```
