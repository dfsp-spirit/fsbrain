# Internal function to get some demo EEG electrode coordinates. Will be removed from public API. Do not use this.

Internal function to get some demo EEG electrode coordinates. Will be
removed from public API. Do not use this.

## Usage

``` r
eeg_coords(label_subset = NULL)
```

## Arguments

- label_subset:

  vector of character strings, electrode names like 'Nz' or 'RPA'.
  ÖLeave at the default value `NULL` to get all available ones.

## Value

data.frame with one row per electrode, and the following 3 columns:
'label': the electrode name, 'theta': the azimuth in degrees, 'phi': the
latitude in degrees.

## Note

There are lots of different naming conventions for spherical
coordinates, see
https://en.wikipedia.org/wiki/Spherical_coordinate_system.

## References

See http://wiki.besa.de/index.php?title=Electrodes_and_Surface_Locations
