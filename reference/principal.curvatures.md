# Computes principal curvatures according to 2 definitions from raw k1 and k2 values.

Computes principal curvatures according to 2 definitions from raw k1 and
k2 values.

## Usage

``` r
principal.curvatures(k1_raw, k2_raw)
```

## Arguments

- k1_raw:

  numerical vector, one of the two principal curvatures, one value per
  vertex

- k2_raw:

  numerical vector, the other one of the two principal curvatures, one
  value per vertex

## Value

a named 'principal_curvatures' list, with entries
'principal_curvature_k1': larger value of k1_raw, k2_raw.
'principal_curvature_k2': smaller value of k1_raw, k2_raw.
'principal_curvature_k_major': larger value of abs(k1_raw), abs(k2_raw).
'principal_curvature_k_minor': smaller value of abs(k1_raw),
abs(k2_raw).

## Note

To obtain k1_raw and k2_raw, use `surface.curvatures` to compute it from
a mesh, or load the FreeSurfer files `'surf/?h.white.max'` and
`'surf/?h.white.min'`.
