# Write deepcopy list for longitudinal subjects.

Write deepcopy list for longitudinal subjects.

## Usage

``` r
deepcopylist.long(
  measures = c("thickness", "area", "volume"),
  fwhms = c("5", "10", "15"),
  hemis = c("lh", "rh"),
  long_measures = c("avg", "rate", "spc", "pc1"),
  template = "fsaverage",
  has_stacked_file = TRUE,
  output_file = NULL
)
```

## Value

vector of character strings, the file entries. Set ouput_file to also
write them to a file.
