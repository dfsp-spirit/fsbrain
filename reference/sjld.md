# Get subjects list from subjects.txt file in dir.

Get subjects list from subjects.txt file in dir.

## Usage

``` r
sjld(subjects_dir)
```

## Arguments

- subjects_dir:

  character string, existing subjects dir with a subjects.txt file
  containing one subject per line and no header line.

## Value

named list with entries: 'd', the query subjects_dir (repeated from the
parameter), 'l', vector of character strings, the subjects_list read
from the file, 'f', the subjects_file.

## Note

This function stops if the file does not exist or cannot be read.
