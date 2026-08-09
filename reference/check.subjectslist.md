# Check whether the subjects_list looks good, warn if not.

Check whether the subjects_list looks good, warn if not.

## Usage

``` r
check.subjectslist(
  subjects_list,
  subjects_dir = NULL,
  report_name = "subjects_list"
)
```

## Arguments

- subjects_list:

  vector of character strings, the subject IDs.

- subjects_dir:

  optional, character string. The full path to the subjects directory
  containing all subjects. If given, extra checks will be performed,
  e.g., whether the subjects exist in this directory.

- report_name:

  character string, the variable name that should be used for the list
  in the messages.

## Note

This function will stop if subjects dirs are missing.
