# The default anti-aliasing factor of the scimesh backend

The anti-aliasing factor used for scimesh renders when neither the
fsbrain option 'fsbrain.scimesh.aa_samples' nor the scimesh-wide option
'scimesh.aa_samples' is set. scimesh renders without anti-aliasing by
default, which is most visible on thin lines (they show a staircase
pattern, unlike the hardware-drawn lines of the rgl backend), so fsbrain
requests 2x2 supersampling. Set 'fsbrain.scimesh.aa_samples' to 1 to
turn anti-aliasing off, or to 4 for higher quality.

## Usage

``` r
FSBRAIN_SCIMESH_DEFAULT_AA
```
