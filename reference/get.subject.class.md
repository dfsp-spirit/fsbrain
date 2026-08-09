# Construct FSGD Class name from group and non-continuous covariate columns.

Construct FSGD Class name from group and non-continuous covariate
columns.

## Usage

``` r
get.subject.class(demographics_df, row_idx, class_columns, collapse = "_")
```

## Arguments

- demographics_df:

  data.frame, as returned by `read.md.demographics` or created manually.
  Note that the data.frame must not contain any character columns, they
  should be converted to factors.

- row_idx:

  integer, the row in the df that belongs to this subject

- class_columns:

  the column names to use

- collapse:

  character string, the separator

## Value

character string, the Class name for this subject, derived from the
values in the 'class_columns'.
