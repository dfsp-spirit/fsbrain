# Create igraph undirected graph from a brain surface mesh.

Create igraph undirected graph from a brain surface mesh.

## Usage

``` r
fs.surface.to.igraph(surface)
```

## Arguments

- surface:

  an fs.surface instance as returned by `subject.surface`, an existing
  igraph (which will be returned as-is) or a string which is interpreted
  as a path to a surface file.

## Value

igraph::graph instance

## Examples

``` r
if (FALSE) { # \dontrun{
  # Find the one-ring neighbors of vertex 15 on the fsaverage left hemi:
  sf = subject.surface(fsaverage.path(T), "fsaverage", "white", "lh");
  g = fs.surface.to.igraph(sf);
  igraph::neighborhood(g, order = 1, nodes = 15);
} # }
```
