# Take screenshot of rgl scene, with fallback for systems without X11.

Takes a screenshot of the current rgl scene. On systems with working
X11/OpenGL, uses the standard rgl.snapshot() method. On systems without
X11 (e.g., recent macOS versions where XQuartz is broken), falls back to
exporting as PDF via rgl.postscript() and converting to PNG using
ImageMagick.

## Usage

``` r
take.screenshot(output_image, silent = TRUE)
```

## Arguments

- output_image:

  character string, path to the output PNG file.

- silent:

  logical, whether to suppress messages. Default TRUE.

## Value

invisible NULL, called for side effect of writing the image file.
