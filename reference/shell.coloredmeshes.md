# Create the coloredmeshes of the shells.

Creates one `fs.coloredmesh` per shell, each with its own color and
alpha value stored in the mesh style.

## Usage

``` r
shell.coloredmeshes(
  meshes,
  levels,
  colors = NULL,
  alphas = NULL,
  palette = "grey_context",
  alpha_range = NULL
)
```

## Arguments

- meshes:

  list of `mesh3d` instances, one per shell.

- levels:

  numerical vector of the iso-levels, ascending.

- colors:

  character vector of colors or `NULL` to use the colors of the
  `palette`.

- alphas:

  numerical vector of alpha values or `NULL` to use the alphas of the
  `palette`.

- palette:

  character string, the name of the palette, see
  [`shell.palette`](https://dfsp-spirit.github.io/fsbrain/reference/shell.palette.md).

- alpha_range:

  numerical vector of length 2 or `NULL`, passed to
  [`shell.palette`](https://dfsp-spirit.github.io/fsbrain/reference/shell.palette.md).

## Value

list of `fs.coloredmesh` instances, one per shell.
