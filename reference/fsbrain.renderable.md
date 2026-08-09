# Check whether object can be rendered by fsbrain

Check whether object can be rendered by fsbrain

## Usage

``` r
fsbrain.renderable(x)
```

## Arguments

- x:

  any `R` object

## Value

TRUE if *x* is an instance of a class that can be rendered by fsbrain
visualization functions, and FALSE otherwise. Currently, the following
types are renderable: `fs.coloredvoxels`, `fs.coloredmesh`,
`Triangles3D`.

## See also

[`is.Triangles3D`](https://dfsp-spirit.github.io/fsbrain/reference/is.Triangles3D.md)
