# Read demographics file

Load a list of subjects and metadata from a demographics file, i.e., a
tab-separated file containing an arbitrary number of columns, one of
which must be the subject id.

## Usage

``` r
read.md.demographics(
  demographics_file,
  column_names = NULL,
  header = FALSE,
  scale_and_center = FALSE,
  sep = "",
  report = FALSE,
  stringsAsFactors = TRUE,
  group_column_name = NULL
)
```

## Arguments

- demographics_file, :

  string. The path to the file.

- column_names, :

  vector of strings. The column names to set in the returned dataframe.
  The length must match the number of columns in the file.

- header, :

  logical. Whether the file starts with a header line.

- scale_and_center, :

  logical. Whether to center and scale the data. Defaults to FALSE.

- sep, :

  string. Separator passed to
  [`read.table`](https://rdrr.io/r/utils/read.table.html), defaults to
  tabulator.

- report, :

  logical. Whether to write an overview, i.e., some descriptive
  statistics for each column, to STDOUT. Defaults to FALSE. See
  [`report.on.demographics`](https://dfsp-spirit.github.io/fsbrain/reference/report.on.demographics.md).

- stringsAsFactors, :

  logical. Whether to convert strings in the input data to factors.
  Defaults to TRUE.

- group_column_name, :

  string or NULL. If given, the column name of the group column. It must
  be a factor column with 2 levels. Enables group-comparison tests.
  Defaults to NULL.

## Value

a dataframe. The data in the file. String columns will be returned as
factors, which you may want to adapt afterwards for the subject
identifier column.

## See also

Other metadata functions:
[`demographics.to.fsgd.file()`](https://dfsp-spirit.github.io/fsbrain/reference/demographics.to.fsgd.file.md),
[`read.md.subjects()`](https://dfsp-spirit.github.io/fsbrain/reference/read.md.subjects.md),
[`report.on.demographics()`](https://dfsp-spirit.github.io/fsbrain/reference/report.on.demographics.md)

## Examples

``` r
   demographics_file =
   system.file("extdata", "demographics.tsv", package = "fsbrain", mustWork = TRUE);
   column_names = c("subject_id", "group", "age");
   demographics = read.md.demographics(demographics_file,
   header = TRUE, column_names = column_names, report = FALSE);
```
