# Get list of valid view angle names.

The returned strings are used as constants to identify a view of type
`sd_<angle>`. They can be used to construct entries for the parameter
`views` of functions like
[`vis.subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md),
or directly as parameter 'view_angles' for functions like
[`vislayout.from.coloredmeshes`](https://dfsp-spirit.github.io/fsbrain/reference/vislayout.from.coloredmeshes.md).

## Usage

``` r
get.view.angle.names(angle_set = "all", add_sd_prefix = TRUE)
```

## Arguments

- angle_set:

  string, which view subset to return. Available subsets are: 'all' (or
  alias 't9'): for all 9 angles. 't4': for the t4 views. 'medial': the 2
  medial views, one for each hemi. 'lateral': the 2 lateral views, one
  for each hemi. 'lh': medial and laterial for the left hemisphere.
  'rh': medial and laterial for the right hemisphere.

- add_sd_prefix:

  logical, whether the prefix 'sd\_' should be added to the string. This
  will construct full view names. If set to false, only the substring
  after the prefix 'sd\_' will be returned. This is used internally only
  and should not be needed in general.

## Value

vector of character strings, all valid view angle strings.
