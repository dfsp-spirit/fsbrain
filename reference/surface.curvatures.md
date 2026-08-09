# Compute the k1 and k2 principal curvatures of a mesh.

Compute the k1 and k2 principal curvatures of a mesh.

## Usage

``` r
surface.curvatures(surface)
```

## Arguments

- surface:

  an fs.surface instance, as returned by `subject.surface`.

## Value

named list, the entries 'K1' and 'K2' contain the principal curvatures.

## Note

Require the optional dependency package 'Rvcg'.
