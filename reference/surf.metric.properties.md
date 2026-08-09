# Compute metric surface properties.

Compute metric surface properties.

## Usage

``` r
surf.metric.properties(surface, is_template, template_scale_factor = 1.56)
```

## Arguments

- surface:

  an fs.surface instance, and for the typical use case of this function,
  a spherical surface.

- is_template:

  logical, whether the surface comes from a template subject.

- template_scale_factor:

  double, the template scale factor

## Value

named list of metric surface properties.

## Examples

``` r
if (FALSE) { # \dontrun{
surface = subject.surface(fsaverage.path(), "fsaverage3", hemi="lh");
mp = surf.metric.properties(surface, is_template = TRUE);
} # }
```
