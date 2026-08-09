# Transform spherical coordinates to FreeSurfer surface space to plot things around a brain.

Transform spherical coordinates to FreeSurfer surface space to plot
things around a brain.

## Usage

``` r
sph2fs(
  lon,
  lat,
  radius = surf.radius.fsaverage(),
  center = surf.center.fsaverage(),
  deg = TRUE
)
```

## Arguments

- lon:

  numerical vector, the longitudes, passed to
  [`sphereplot::sph2car`](https://rdrr.io/pkg/sphereplot/man/sph2car.html).
  See 'deg' for unit information.

- lat:

  numerical vector, the latitudes, passed to
  [`sphereplot::sph2car`](https://rdrr.io/pkg/sphereplot/man/sph2car.html).
  See 'deg' for unit information.

- radius:

  numerical vector, the radii, passed to
  [`sphereplot::sph2car`](https://rdrr.io/pkg/sphereplot/man/sph2car.html).
  Defaults to the radius of the combined mesh from the fsaverage lh and
  rh surfaces.

- center:

  numerical vector of length 3, the x, y, and z coordinates of the
  target center. The
  [`sphereplot::sph2car`](https://rdrr.io/pkg/sphereplot/man/sph2car.html)
  function operates on the unit sphere, and this parameter is used to
  translate the resulting cartesian coordinates to a new center,
  typically the center of the surface meshes or MRI volume or
  substructures. If you want no translation, pass `c(0,0,0)`.

- deg:

  logical, whether to use degrees (as opposed to radians) as the unit
  for 'lat' and 'lon'. Passed to
  [`sphereplot::sph2car`](https://rdrr.io/pkg/sphereplot/man/sph2car.html).

## Note

This function can be used to plot things in FreeSurfer space using
spherical coordinates, as commonly used in EEG to define electrode
positions. Requires the 'sphereplot' package.

## Examples

``` r
if (FALSE) { # \dontrun{
    # Draw voxels on a sphere around fsaverage:
    lat = seq.int(from=0, to=360, by=30);
    lon = rep(0, length(lat));
    vis.fs.surface('~/software/freesurfer/subjects/fsaverage/surf/lh.white');
    fsbrain::rglvoxels(sph2fs(lat, lon), voxelcol = 'red');
    fsbrain::rglvoxels(sph2fs(lon, lat), voxelcol = 'green');
} # }
```
