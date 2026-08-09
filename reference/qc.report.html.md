# Create visual quality check report from QC result.

Create visual quality check report from QC result.

## Usage

``` r
qc.report.html(
  subjects_dir,
  subjects_list,
  out_dir = "fsbrain_qc_report",
  subjects_metadata = list(),
  qc = NULL,
  ...
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subjects_list:

  string vector. A vector of subject identifiers that match the
  directory names within subjects_dir.

- out_dir:

  character string, path to output dir. The last directory part will be
  created if it does not exist (but not recursively).

- subjects_metadata:

  named list, keys can be subjects from subjects_list. Each key can hold
  another named list of strings, represeting arbitrary metadata for that
  subject that will be displayed in the report.

- qc:

  optional qc result. If NULL, a QC report is created using standard
  settings 'aparc' and 'thickness'.

- ...:

  passed on to `subject.report.html`.

## Examples

``` r
if (FALSE) { # \dontrun{
s = sjld("~/data/IXI_min/mri/freesurfer");
s$l = s$l[1:100]; # first few subjects are enough
fsbrain:::qc.report.html(s$d, s$l);
} # }
```
