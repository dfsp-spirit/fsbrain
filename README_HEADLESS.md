# fsbrain without a display: headless and macOS

fsbrain's default [rgl](https://CRAN.R-project.org/package=rgl) backend
opens an interactive 3D window and therefore needs a working X11/OpenGL
display stack. This document is for everyone who does **not** have one:
headless machines (HPC clusters, CI runners, servers, containers) and
recent macOS systems where the X11 environment
([XQuartz](https://www.xquartz.org/)) is broken.

## The problem

Two common situations lead to the same root cause — no working
X11/OpenGL/GPU stack — and both break the interactive rgl windows that
fsbrain opens for visualization:

* **Headless environments**: HPC clusters, CI runners, and containers
  typically have no display and often no GPU. There is simply no X11
  server to open a window on.
* **Recent macOS versions** (Tahoe 26.x, Sonoma 14.x): macOS changed how
  it handles OpenGL and X11, which breaks XQuartz. As a result, fsbrain
  may produce blank plots or fail to open visualization windows.

This is not a problem with fsbrain itself, but with the underlying
graphics stack (rgl / XQuartz). On macOS, the issue is tracked upstream:

- [rgl issue #488](https://github.com/dmurdoch/rgl/issues/488)
- [rgl issue #423](https://github.com/dmurdoch/rgl/issues/423)

...but currently only workarounds exist (like rendering into a web view),
and given the situation on macOS, that is unlikely to change.

One more option exists on Linux: keep the rgl backend and hand it a display
and an OpenGL implementation in *software*, by running R under a virtual X
server (Xvfb) with Mesa's software rasterizer (llvmpipe). It works, but it
needs extra system packages and gives up GPU acceleration, and it is not
available on macOS, where rgl depends on exactly the XQuartz/X11 stack that
is broken. See [solution 3](#solution-3-advanced-linux-only-rgl-under-a-virtual-x-display)
below.

## Solution 1 (recommended): Use the scimesh backend

The [scimesh](https://CRAN.R-project.org/package=scimesh) package provides
a headless, CPU-based software renderer that produces publication-quality
static images. It requires **no X11, no OpenGL, no XQuartz, and no GPU**,
so it works on headless machines and broken-X11 macOS systems alike.

To use it, install it and switch the backend for your R session:

```r
install.packages("scimesh")
options(fsbrain.renderer_backend = "scimesh")
```

Static image export (`export()`, `vislayout.from.coloredmeshes()`) then
renders with scimesh. See the package vignette —
`vignette("fsbrain_with_scimesh")` — for the full documentation: what
works, the limitations, and many worked examples. In short, all static
image export, multi-view layouts, and colorbars work; interactive 3D
windows, real-time rotation, and `vis.rglwidget()` do not.

Note that scimesh itself renders without anti-aliasing, which is most
visible on thin lines (they show a hard staircase pattern, unlike the lines
drawn by rgl), so fsbrain turns on 2x2 supersampling by default. Change that
with:

```r
options(fsbrain.scimesh.aa_samples = 4)   # better quality, slower
options(fsbrain.scimesh.aa_samples = 1)   # no anti-aliasing, fastest
```

This affects all subsequent fsbrain renders with the scimesh backend. The
scimesh-wide equivalent is `options(scimesh.aa_samples = 2)`, which is honored
as long as the fsbrain option is not set.

## Solution 2: Browser-based interactive visualization

If you need interactive 3D viewing, you can use `vis.rglwidget()`, which
renders the scene in your web browser using WebGL instead of the missing system OpenGL stack. Note that this does **not** include an option to display colorbars and cannot produce multi-view layouts.

While it can be used for interactive data analysis during development
and iterative research workflows, it is not suited for generating publication
quality plots.

```r
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

## Solution 3 (advanced, Linux only): rgl under a virtual X display

If you need the rgl backend itself — e.g., because you rely on a feature that
scimesh does not support — and you are on Linux, you can give rgl what it
wants in software: a virtual X server ([Xvfb](https://www.x.org/releases/X11R7.7/doc/man/man1/Xvfb.1.xhtml))
for the display and Mesa's software rasterizer (llvmpipe) for OpenGL.

```sh
# Debian/Ubuntu
sudo apt-get install xvfb libgl1-mesa-dri libglu1-mesa
xvfb-run -a Rscript my_fsbrain_analysis.R
```

This is how the fsbrain Docker images run R (`xvfb-run -a R`, see
[docker/README.md](./docker/README.md)) and how
[README_DEVELOPMENT.md](./README_DEVELOPMENT.md) runs the test suite locally,
including the tests that need a real rgl window. Note the downsides:

* it adds system packages (X11, Mesa, GLU) — rgl needs `libGL`/`libGLU`
  regardless, so this does not help on a system where you cannot install them,
* rendering is done on the CPU, which is slower than a GPU,
* it is not an option on macOS, where rgl uses the very XQuartz/X11 stack
  that is broken and Xvfb is unavailable.

For static image export, the scimesh backend (solution 1) is the simpler and
recommended way.

## Quick comparison

| Feature | scimesh backend | rglwidget | rgl (XQuartz) |
|---------|:---:|:---:|:---:|
| Static image export (PNG) | ✓ | — | ✓ |
| Multi-view layouts (t4, t9) | ✓ | — | ✓ |
| Colorbars | ✓ | — | ✓ |
| All vis.* functions | ✓ | via no_vis | ✓ |
| Interactive 3D window | — | — | ✓ |
| Interactive 3D in browser | — | ✓ | via rglwidget |
| Real-time rotation | — | — | ✓ |
| Requires X11 / XQuartz | No | No | **Yes** |
| Requires GPU / OpenGL | No | No | Yes |
| Publication-quality output | ✓ | — | ✓ |

**Bottom line**:

* For creating static figures (the most common use case) without a display, use the scimesh backend.
* If you need interactive 3D exploration, use `vis.rglwidget()` (uses WebGL).
* If you have a working X11/OpenGL stack (most Linux desktops, older macOS versions), the default rgl backend works.
* If you need the rgl backend itself on a display-less Linux machine, you can run it under Xvfb with Mesa's software OpenGL ([solution 3](#solution-3-advanced-linux-only-rgl-under-a-virtual-x-display)) — advanced, and not an option on macOS.
* If you are running headless in containers, CI, etc, interactive plots make no sense anyway. Use scimesh.
