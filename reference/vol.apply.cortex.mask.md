# Apply the cortex mask to projected data.

Apply the cortex mask to projected data.

## Usage

``` r
vol.apply.cortex.mask(data, subjects_dir, subject_id, cortex_label = "cortex")
```

## Arguments

- data:

  named list of numeric vectors, per hemisphere.

- subjects_dir:

  character string, the subjects dir holding the label files.

- subject_id:

  character string, the subject/template identifier.

- cortex_label:

  character string, the name of the cortex label file (or the loaded
  label data).

## Value

named list of numeric vectors, with medial wall values set to NA.
