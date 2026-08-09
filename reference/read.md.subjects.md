# Read subjects file

Load a list of subjects from a subjects file, i.e., a simple text file
containing one subject name per line.

## Usage

``` r
read.md.subjects(subjects_file, header)
```

## Arguments

- subjects_file:

  character string, the path to the subjects file.

- header:

  logical, whether the file starts with a header line.

## Value

vector of strings, the subject identifiers.

## See also

Other metadata functions:
[`demographics.to.fsgd.file()`](https://dfsp-spirit.github.io/fsbrain/reference/demographics.to.fsgd.file.md),
[`read.md.demographics()`](https://dfsp-spirit.github.io/fsbrain/reference/read.md.demographics.md),
[`report.on.demographics()`](https://dfsp-spirit.github.io/fsbrain/reference/report.on.demographics.md)

## Examples

``` r
   subjects_file = system.file("extdata", "subjects.txt", package = "fsbrain", mustWork = TRUE);
   subjects_list = read.md.subjects(subjects_file, header = FALSE);
```
