# Get the current fsbrain renderer backend

Returns the current fsbrain renderer backend setting. When set to
`"scimesh"`, fsbrain will use the scimesh software renderer instead of
rgl/OpenGL for image export. The default is `"rgl"`. Set it with
`options(fsbrain.renderer_backend = "scimesh")` at the start of your R
session.

## Usage

``` r
get.fsbrain.renderer.backend()
```

## Value

character string, either "rgl" or "scimesh".

## Note

Only functions that produce static PNG output are affected. Interactive
viewers, animations, and the rglwidget (WebGL) always use rgl.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Switch to the scimesh software renderer for headless environments
  options(fsbrain.renderer_backend = "scimesh")

  # Check current backend
  get.fsbrain.renderer.backend()

  # Switch back to rgl
  options(fsbrain.renderer_backend = "rgl")
} # }
```
