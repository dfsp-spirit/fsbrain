# Filter QDEC long table for subjects.

Filter QDEC long table for subjects.

## Usage

``` r
qdec.table.filter(qdec_file, subjects_list, output_qdec_file = NULL)
```

## Arguments

- qdec_file:

  the source QDEC table

- subjects_list:

  the subjects to extract from the QDEC file

- output_qdec_file:

  optional character string, a file name to which to write the
  resulting, filtered table. If not given, no file is created.

## Value

the data.frame containing the subset of subjects from the original QDEC
file.

## Note

This assumes that there are 2 time points per subject and warns if not
all requested subjects were found.
