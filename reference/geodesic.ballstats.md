# Compute geodesic ball area and perimeter at location defined by geodists for all radii.

Compute geodesic ball area and perimeter at location defined by geodists
for all radii.

## Usage

``` r
geodesic.ballstats(mesh, geodist, sample_at_radii)
```

## Arguments

- mesh:

  tmesh3d instance

- geodist:

  vector of geodesic distance for current mesh vertex under
  consideration (no need for the index)

- sample_at_radii:

  double vector, the different ball radii for which to perform the
  computation

## Value

named list with entries: 'ball_area': vector of double, ball area at
each sample radius. 'ball_perimeter': vector of double, ball perimeter
at each sample radius.

## Note

This is called from `geodesic.circles`, there should be no need to call
it directly.
