# Visualization on macOS Tahoe and Sonoma

## The problem

Recent macOS versions (Tahoe 26.x, Sonoma 14.x) changed how they handle
OpenGL and X11. This breaks [XQuartz](https://www.xquartz.org/) — the
X11 environment that [rgl](https://CRAN.R-project.org/package=rgl) needs
to open graphics windows. rgl has been the mesh renderer used by fbrain
since fsbrain was first released.

As a result, fsbrain may produce blank plots or fail to open
visualization windows.

This is not a problem with fsbrain itself, but with the underlying
graphics stack (rgl / XQuartz). The issue is tracked upstream:

- [rgl issue \#488](https://github.com/dmurdoch/rgl/issues/488)
- [rgl issue \#423](https://github.com/dmurdoch/rgl/issues/423)

…but currently only workarounds exist (like rendering into a web view),
and given the situation on MacOS, that is unlikely to change.

## Solution 1 (recommended): Use the scimesh backend

The [scimesh](https://CRAN.R-project.org/package=scimesh) package
provides a headless, CPU-based software renderer that produces
publication-quality static images. It requires **no X11, no OpenGL, no
XQuartz, and no GPU**.

### Installation

Scimesh is suggested by fsbrain, meaning you already have it if you
installed fsbrain with all optional dependencies, like this:

``` r

install.packages("fsbrain", dependencies=TRUE)
```

If in doubt, just install it:

``` r

install.packages("scimesh")
```

### Usage

``` r

library(fsbrain)
options(fsbrain.renderer_backend = "scimesh")

# Download example data (first time only)
subjects_dir <- sjd.demo()

# Build visualization data (no rendering yet)
cm <- vis.subject.morph.native(subjects_dir, "subject1", "sulc",
                                views = NULL)

# Export publication-ready figure with 4 views
vislayout.from.coloredmeshes(
    cm,
    view_angles = get.view.angle.names("t4"),
    output_img  = "sulc_t4.png"
)

# Export with colorbar
export(cm,
       colorbar_legend = "Sulcal depth [mm]",
       output_img      = "sulc_figure.png",
       draw_colorbar   = "horizontal")
```

### What works

- All `vis.*` functions (`vis.subject.morph.native`,
  `vis.subject.morph.standard`, `vis.subject.annot`,
  `vis.subject.label`, `vis.symmetric.data.on.subject`,
  `vis.region.values.on.subject`, etc.)
- Multi-view layouts (4-view t4, 9-view t9, single views)
- Colorbars (horizontal and vertical)
- All rendering styles (`"default"`, `"shiny"`, `"semitransparent"`,
  `"glass"`, `"edges"`)
- Publication-quality PNG output at any resolution
- Volume visualisation functions (already rgl-free)

### Limitations

In practice, these limitations are rarely relevant: most users use
fsbrain to create static figures for presentations and publications, for
which scimesh works perfectly.

The limitations of scimesh are those inherent to all software renderers:

- No interactive 3D windows (`views = "si"`)
- No real-time rotation (`views = "sr"`, `vis.coloredmeshes.rotating`)
- No browser-based 3D widgets (`vis.rglwidget`) — see Solution 2 below
- No animated GIF export via rgl (`movie3d`)

The reason for these limitations is that a CPU can render a beautiful
brain image in about 2 seconds, while a graphics card with OpenGL driver
stack can do the same in 0.02 seconds – and thus produce so many frames
per second that interactive viewing becomes possible.

## Solution 2: Browser-based interactive visualization

If you need interactive 3D viewing, you can use
[`vis.rglwidget()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.rglwidget.md),
which renders the scene in your web browser. Note that this does **not**
include colorbars and cannot produce multi-view layouts.

``` r

library(fsbrain)

# Download example data (only needed once)
fsbrain::download_optional_data()
subjects_dir <- fsbrain::get_optional_data_filepath("subjects_dir")

# Create visualization data without opening a window
cm <- vis.subject.morph.native(subjects_dir, "subject1", "sulc",
                                views = NULL)

# Create interactive browser-based widget
widget <- vis.rglwidget(cm)
widget  # displays in RStudio viewer or web browser
```

## Quick comparison

| Feature                     | scimesh backend | rglwidget  | rgl (XQuartz) |
|-----------------------------|:---------------:|:----------:|:-------------:|
| Static image export (PNG)   |        ✓        |     —      |       ✓       |
| Multi-view layouts (t4, t9) |        ✓        |     —      |       ✓       |
| Colorbars                   |        ✓        |     —      |       ✓       |
| All vis.\* functions        |        ✓        | via no_vis |       ✓       |
| Interactive 3D window       |        —        |     —      |       ✓       |
| Interactive 3D in browser   |        —        |     ✓      | via rglwidget |
| Real-time rotation          |        —        |     —      |       ✓       |
| Requires X11 / XQuartz      |       No        |     No     |    **Yes**    |
| Requires GPU / OpenGL       |       No        |     No     |      Yes      |
| Publication-quality output  |        ✓        |     —      |       ✓       |

**Bottom line**: For creating static figures (the most common use case),
use the scimesh backend. If you need interactive 3D exploration, use
[`vis.rglwidget()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.rglwidget.md).
If you have a working XQuartz installation (older MacOS versions), the
default rgl backend also works.
