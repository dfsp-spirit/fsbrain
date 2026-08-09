# Turn surface mesh into a igraph and return its adjacency list representation.

Turn surface mesh into a igraph and return its adjacency list
representation.

## Usage

``` r
fs.surface.as.adjacencylist(surface)
```

## Arguments

- surface:

  an fs.surface instance as returned by `subject.surface`, an existing
  igraph (which will be returned as-is) or a string which is interpreted
  as a path to a surface file.

## Value

list of integer vectors, the adjacency list.
