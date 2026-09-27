# Combine two bounding boxes.

Combine two bounding boxes.

## Usage

``` r
combine_bboxes(bbox1, bbox2)
```

## Arguments

- bbox1:

  numeric vector of length 6 or NULL, see
  [`segment_bbox`](https://dfsp-spirit.github.io/fsbrain/reference/segment_bbox.md).

- bbox2:

  numeric vector of length 6.

## Value

numeric vector of length 6, the box that contains both input boxes.
