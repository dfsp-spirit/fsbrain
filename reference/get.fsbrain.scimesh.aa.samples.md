# Get the anti-aliasing factor for the scimesh backend

Determines the anti-aliasing (supersampling) factor that fsbrain passes
to scimesh. The value is taken from the global option
'fsbrain.scimesh.aa_samples'; when that option is unset, an explicitly
set scimesh-wide option 'scimesh.aa_samples' is used instead, so that a
session-wide scimesh setting is honored. If neither is set, fsbrain uses
`FSBRAIN_SCIMESH_DEFAULT_AA` (2, i.e. 2x2 supersampling).

## Usage

``` r
get.fsbrain.scimesh.aa.samples()
```

## Value

single positive integer.

## Details

The order of precedence is: `fsbrain.scimesh.aa_samples` \>
`scimesh.aa_samples` \> `2` (the fsbrain default). The option is read
for every render call, so it can be changed at any time with
[`options()`](https://rdrr.io/r/base/options.html).

## Examples

``` r
if (FALSE) { # \dontrun{
  # Higher quality (4x4 supersampling) for all scimesh renders:
  options(fsbrain.scimesh.aa_samples = 4);

  # Back to the fsbrain default (2x2), ignoring a scimesh-wide setting:
  options(fsbrain.scimesh.aa_samples = 2);

  # No anti-aliasing, for fast drafts:
  options(fsbrain.scimesh.aa_samples = 1);
} # }
```
