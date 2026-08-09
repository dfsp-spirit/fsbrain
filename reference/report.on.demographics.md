# Print a demographics report

Print a demographics report

## Usage

``` r
report.on.demographics(
  demographics_df,
  group_column_name = NULL,
  paired = FALSE
)
```

## Arguments

- demographics_df:

  a demographics data.frame, as returned by
  [`read.md.demographics`](https://dfsp-spirit.github.io/fsbrain/reference/read.md.demographics.md).

- group_column_name, :

  string or NULL. If given, the column name of the group column. It must
  be a factor column with 2 levels. Enables group-comparison tests.
  Defaults to `NULL`.

- paired:

  Whether the data of the two groups if paired (repeated measurements).
  Only relevant if group_column_name is given and tests for group
  differences are included in the report. Defaults to `FALSE`.

## Value

vector of character strings, the lines of the demographics report.

## See also

Other metadata functions:
[`demographics.to.fsgd.file()`](https://dfsp-spirit.github.io/fsbrain/reference/demographics.to.fsgd.file.md),
[`read.md.demographics()`](https://dfsp-spirit.github.io/fsbrain/reference/read.md.demographics.md),
[`read.md.subjects()`](https://dfsp-spirit.github.io/fsbrain/reference/read.md.subjects.md)
