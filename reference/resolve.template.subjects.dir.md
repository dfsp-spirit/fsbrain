# Resolve the subjects dir which contains a template subject.

Searches for a directory which contains the surface files of the given
template subject, first in the user-supplied directory (if any), then in
the fsbrain package data cache, and finally in the FreeSurfer
installation. Stops with an error explaining how to obtain the missing
data if the template cannot be found.

## Usage

``` r
resolve.template.subjects.dir(template, subjects_dir = NULL)
```

## Arguments

- template:

  character string, the template subject identifier, e.g., 'fsaverage'
  or 'fs_LR_32'.

- subjects_dir:

  character string or NULL, a user-supplied subjects dir which is
  checked first.

## Value

character string, the subjects dir which contains the template subject.
