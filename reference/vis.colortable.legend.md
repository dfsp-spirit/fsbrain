# Create a separate legend plot for a colortable or an annotation.

This plots a legend for a colortable or an atlas (annotation), showing
the region names and their assigned colors. This function creates a new
plot.

## Usage

``` r
vis.colortable.legend(colortable, ncols = 1L, plot_struct_index = TRUE)
```

## Arguments

- colortable:

  dataframe, a colortable as returned by
  [`read.fs.colortable`](https://rdrr.io/pkg/freesurferformats/man/read.fs.colortable.html)
  or the inner 'colortable_df' returned by
  [`subject.annot`](https://dfsp-spirit.github.io/fsbrain/reference/subject.annot.md).
  One can also pass an annotation (*fs.annot* instance).

- ncols:

  positive integer, the number of columns to use when plotting

- plot_struct_index:

  logical, whether to plot the region index from tge 'struct_index'
  field. If there is no such field, this is silently ignored.

## Note

This function plots one or more legends (see
[`legend`](https://rdrr.io/r/graphics/legend.html)). You may have to
adapt the device size before calling this function if you inted to plot
a large colortable.

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   annot = subject.annot(subjects_dir, 'subject1', 'lh', 'aparc');
   vis.colortable.legend(annot$colortable_df, ncols=3);
} # }
```
