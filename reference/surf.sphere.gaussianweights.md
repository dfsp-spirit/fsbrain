# Compute Gaussian weights

Compute Gaussian weights

## Usage

``` r
surf.sphere.gaussianweights(spherical_surface, sphere_dists, gstd)
```

## Arguments

- spherical_surface:

  an fs.surface instance representing the spherical version (`lh.sphere`
  or `rh.sphere` of the subject).

- sphere_dists:

  list of vectors, as returned by surf.sphere.dist

- gstd:

  double, Gaussian standard deviation, can be computed from the FWHM as
  `gstd = fwhm / sqrt(log(256.0))`.

## Value

vector of Gaussian weights for vertices

## Examples

``` r
if (FALSE) { # \dontrun{
fwhm = 20.0; truncfactor = 3.5; sjd = fsaverage.path();
gstd = fwhm / sqrt(log(256.0)); maxdist = truncfactor * gstd;
spherical_surface = subject.surface(sjd, "fsaverage3", surface="sphere", hemi="lh");
sphere_dists = surf.sphere.dist(spherical_surface, maxdist = maxdist);
gaussian_weights = fsbrain:::surf.sphere.gaussianweights(spherical_surface,
 sphere_dists, gstd);
morph_data = rep(NA, nrow(spherical_surface$vertices));
morph_data[sphere_dists$neigh[[500]]] = gaussian_weights[[500]];
vis.data.on.subject(sjd, "fsaverage3", morph_data_lh=morph_data);
} # }
```
