# fsbrain Shiny Demo

An interactive 3D brain visualization demo using
[fsbrain](https://github.com/dfsp-spirit/fsbrain),
[rgl](https://cran.r-project.org/package=rgl), and
[Shiny](https://shiny.posit.co).

The app renders a subject's cortical surface in your web browser via **WebGL**
— no X11 display or GPU required on the server. You can rotate, zoom, and pan
the 3D model and switch between two morphometry data layers:

- **Cortical Thickness**
- **Sulcal Depth**

The demo is deliberately minimal: it downloads only 6 files (~16 MB total) on
first launch — 2 white surfaces and 4 morphometry overlays for a single subject.
No fsaverage template, parcellation data, or MRI volumes are needed. Startup
completes well within Posit Connect Cloud's 60-second timeout.

## Running locally

### Prerequisites

- **R** (≥ 4.0) installed on your system ([download](https://cran.r-project.org))
- The following R packages:

```r
install.packages(c("shiny", "rgl", "pkgfilecache"))
install.packages("fsbrain")   # from CRAN, or remotes::install_github("dfsp-spirit/fsbrain") for dev version
```

### Launch the app

From a terminal (any directory):

```bash
Rscript web/shiny_demo/app.R
```

Or from within R:

```r
shiny::runApp("web/shiny_demo")
```

Then open the URL printed in the console (usually
[http://127.0.0.1:PORT](http://127.0.0.1:PORT)) in your browser.

> **Note:** On first launch, the app downloads the required data files from
> `rcmd.org` (~16 MB). This happens once and is cached for subsequent runs.

### Running on a headless server / without a display

The app **does not need X11**. It uses `rgl`'s off-screen WebGL renderer.
If you are on a Linux server without a graphical environment, you may still
want to set:

```bash
export RGL_USE_NULL=TRUE
```

to prevent `rgl` from even attempting to open a window.

## Deploying to the web

The easiest way to share this demo publicly is
**[Posit Connect Cloud](https://connect.posit.cloud)** (the successor to
shinyapps.io). It offers a free tier for publishing Shiny apps, Python apps,
Quarto docs, and more.

### From R / the command line

1. Install `rsconnect`:

   ```r
   install.packages("rsconnect")
   ```

2. From your [Posit Connect Cloud dashboard](https://connect.posit.cloud/home),
   copy your API key. Then authenticate once:

   ```r
   rsconnect::connectApiUser(
       account = "<your-account-name>",
       server  = "connect.posit.cloud",
       apiKey  = "<your-api-key>"
   )
   ```

3. Deploy the app:

   ```r
   rsconnect::deployApp("web/shiny_demo")
   ```

### From GitHub

Posit Connect Cloud can also deploy directly from a GitHub repository. In the
dashboard, click **New deployment → From GitHub repository**, select the
`dfsp-spirit/fsbrain` repo, and set the content path to `web/shiny_demo`.

### Other options

| Option | Effort | Notes |
|---|---|---|
| **[Posit Connect Cloud](https://connect.posit.cloud)** | Low | Free tier available. Recommended. |
| **Docker + fly.io / Render** | Medium | Adapt the Dockerfiles in `docker/`. |
| **VPS (Hetzner, DigitalOcean, …)** | Medium | Install R + Shiny Server. Full control. |
| **GitHub Codespaces** | Low | Run `Rscript app.R` and share the forwarded port. Ephemeral but good for live demos. |

> **Note:** GitHub Pages will **not** work for this — it serves only static
> files, and a Shiny app needs a running R process on the server.


## Public instance by dfsp-spirit on posit cloud

A public version is available here: https://connect.posit.cloud/timschaefer/content/019f40eb-f687-4aaa-6413-4f7b18f1b292.

Please allow it some time to load, those meshes are huge.

