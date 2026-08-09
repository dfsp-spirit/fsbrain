# Perform Gaussian smoothing

Perform Gaussian smoothing

## Usage

``` r
pervertexdata.smoothgaussian(
  spherical_surface,
  data,
  fwhm = 15,
  truncfactor = 3.5
)
```

## Arguments

- spherical_surface:

  an fs.surface instance representing the spherical version (`lh.sphere`
  or `rh.sphere` of the subject).

- data:

  numerical vector of per-vertex data for the surface

- fwhm:

  double, the full width at half maximum for the Gaussian kernel

- truncfactor:

  the factor after how many stddevs to truncate the Gaussian kernel

## Value

the smoothed data

## Note

This function has been adapted from FreeSurfer and it is subject to the
FreeSurfer software license.

## Examples

``` r
if (FALSE) { # \dontrun{
sjd = fsaverage.path();
spherical_surface = subject.surface(sjd, "fsaverage3",
  surface="sphere", hemi="lh");
vdata = subject.morph.native(sjd, "fsaverage3", "thickness", hemi="lh");
vdata_smoothed = pervertexdata.smoothgaussian(spherical_surface,
 vdata, fwhm = 15);
vis.data.on.subject(sjd, "fsaverage3", morph_data_lh = vdata);
vis.data.on.subject(sjd, "fsaverage3", morph_data_lh = vdata_smoothed);
} # }
```
