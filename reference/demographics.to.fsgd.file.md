# Write FreeSurfer Group Descriptor (FSGD) file from demographics dataframe.

Write FreeSurfer Group Descriptor (FSGD) file from demographics
dataframe.

## Usage

``` r
demographics.to.fsgd.file(
  filepath,
  demographics_df,
  group_column_name = "group",
  subject_id_column_name = "id",
  var_columns = NULL,
  ftitle = "OSGM",
  fsgd_flag_lines = c("DeMeanFlag 1", "ReScaleFlag 1")
)
```

## Arguments

- filepath:

  character string, the path to the output file in FSGD format

- demographics_df:

  data.frame, as returned by `read.md.demographics` or created manually.
  Note that the data.frame must not contain any character columns, they
  should be converted to factors.

- group_column_name:

  character string, the column name of the group column in the
  'demographics_df'

- subject_id_column_name:

  character string, the column name of the subject identifier column in
  the 'demographics_df'

- var_columns:

  vector of character strings, the column names to include as variables
  in the FSGD file. If NULL (the default), all columns will be included
  (with the exception of the group column and the subject id column).

- ftitle:

  character string, freeform title for the FSGD file

- fsgd_flag_lines:

  vector of character strings, extra flag lines to write to the file.
  The default setting will activate de-meaning and rescaling.

## Value

vector of character strings, the lines written to the 'filepath',
invisible.

## See also

Other metadata functions:
[`read.md.demographics()`](https://dfsp-spirit.github.io/fsbrain/reference/read.md.demographics.md),
[`read.md.subjects()`](https://dfsp-spirit.github.io/fsbrain/reference/read.md.subjects.md),
[`report.on.demographics()`](https://dfsp-spirit.github.io/fsbrain/reference/report.on.demographics.md)
