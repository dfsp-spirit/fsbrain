# Convert an edge list to a connectivity matrix.

Convert an edge list to a connectivity matrix.

## Usage

``` r
connectivity.matrix.from.edge.list(edge_list)
```

## Arguments

- edge_list:

  data.frame with the columns 'source', 'target' and 'weight'.

## Value

named list with entries 'matrix' (the connectivity matrix) and 'names'
(the node names).
