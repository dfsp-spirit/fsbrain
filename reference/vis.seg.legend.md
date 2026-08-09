# Plot legend for a brain volume segmentation based on colorLUT.

Plot legend for a brain volume segmentation based on colorLUT.

## Usage

``` r
vis.seg.legend(colortable, segvol, ...)
```

## Arguments

- colortable:

  a colortable data.frame, or a character string, which will be treated
  as a filename and loaded with
  [`read.fs.colortable`](https://rdrr.io/pkg/freesurferformats/man/read.fs.colortable.html).
  Typically `FS_HOME/FreeSurferColorLUT.txt`.

- segvol:

  optional 3D or 4D array of integer data, the brain segmentation. Or a
  character string, which will be treated as a filename and loaded with
  [`read.fs.volume`](https://rdrr.io/pkg/freesurferformats/man/read.fs.volume.html).
  If given, only colortable entries which actually occur in the volume
  data are plotted. If `NULL`, all entries are plotted, which may be a
  lot.

- ...:

  passed on to
  [vis.colortable.legend](https://dfsp-spirit.github.io/fsbrain/reference/vis.colortable.legend.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ct = file.path(fs.home(), "FreeSurferColorLUT.txt");
seg = file.path(fs.home(), "subjects", "fsaverage", "mri", "aseg.mgz");
vis.seg.legend(ct, seg);

} # }
```
