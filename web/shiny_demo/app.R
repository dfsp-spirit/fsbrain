#!/usr/bin/env Rscript
#'
#' fsbrain Shiny Demo — Interactive 3D brain visualization in a web browser.
#'
#' This app demonstrates how to embed fsbrain's 3D brain renderings
#' into an R Shiny application using the vis.rglwidget() function.
#'
#' The app downloads only 6 data files (~16 MB total): 2 white surfaces and
#' 4 morphometry overlays (thickness + sulcal depth) for a single subject.
#' No fsaverage template is needed — rendering is on the native subject surface.
#' Startup completes well within Posit Connect Cloud's 60-second timeout.
#'
#' Requirements:
#'   - R packages: shiny, fsbrain (>= 0.5.6), rgl, pkgfilecache
#'   - An X11 display is NOT required (rendering is headless).
#'
#' Usage:
#'   Rscript app.R
#'   # Then open http://127.0.0.1:PORT in your browser.
#'
#' Or from within R:
#'   shiny::runApp("web/shiny_demo")
#'

library("shiny")
library("fsbrain")
library("rgl")
library("pkgfilecache")

# ── One-time setup (cached) ──────────────────────────────────────────────────

cat("[setup] Downloading minimal subject data (6 files, ~16 MB)...\n")
pkg_info <- pkgfilecache::get_pkg_info("fsbrain")

subj_base  <- c("subjects_dir", "subject1")
base_url   <- "https://rcmd.org/projects/nitestdata/subjects_dir/subject1"

# Only the files strictly needed: 2 surfaces + 4 morphometry overlays.
dl_files <- list(
    c(subj_base, "surf", "lh.white"),
    c(subj_base, "surf", "rh.white"),
    c(subj_base, "surf", "lh.thickness"),
    c(subj_base, "surf", "rh.thickness"),
    c(subj_base, "surf", "lh.sulc"),
    c(subj_base, "surf", "rh.sulc")
)
dl_md5 <- c(
    "b6d2cdb9793aae3b76c2dcbf03491988",  # lh.white
    "8034395bc9fcfa02c05c6cf6559ab97e",  # rh.white
    "96d6350a6b158453a0231a1f01cfbd58",  # lh.thickness
    "4ec315e8daa6c3bbda46c36b9188b60f",  # rh.thickness
    "bc268800c1cb102a43e99a3ab061ca94",  # lh.sulc
    "13aec75d8712f14345688ce5ad53f648"   # rh.sulc
)
dl_urls <- paste0(base_url, "/",
    c("surf/lh.white", "surf/rh.white",
      "surf/lh.thickness", "surf/rh.thickness",
      "surf/lh.sulc", "surf/rh.sulc"))

pkgfilecache::ensure_files_available(pkg_info, dl_files, dl_urls, md5sums = dl_md5)
cat("[setup] Data download complete.\n")

subjects_dir <- fsbrain::get_optional_data_filepath("subjects_dir")

cat("[setup] Pre-building coloredmeshes (cached for all sessions)...\n")

# Build coloredmeshes once — they're expensive to compute and don't change.
cm_thickness <- vis.subject.morph.native(
    subjects_dir, "subject1", "thickness", "both",
    cortex_only = TRUE,
    rglactions = list("no_vis" = TRUE)
)

cm_sulc <- vis.subject.morph.native(
    subjects_dir, "subject1", "sulc", "both",
    cortex_only = TRUE,
    rglactions = list("no_vis" = TRUE)
)

# ── Shiny UI ─────────────────────────────────────────────────────────────────

ui <- fluidPage(
    titlePanel("fsbrain — Interactive 3D Brain in Shiny"),

    sidebarLayout(
        sidebarPanel(
            width = 3,
            h4("Data Layer"),
            radioButtons("data_layer", NULL,
                choices = c(
                    "Cortical Thickness" = "thickness",
                    "Sulcal Depth"       = "sulc"
                ),
                selected = "thickness"
            ),
            hr(),
            p("Use mouse to rotate (drag), zoom (scroll), and pan (right-drag)."),
            p(em("No X11 required — renders via WebGL in your browser.")),
            hr(),
            p("Powered by ", a("fsbrain", href = "https://github.com/dfsp-spirit/fsbrain"),
              " + ", a("rgl", href = "https://cran.r-project.org/package=rgl"),
              " + ", a("Shiny", href = "https://shiny.posit.co"), ".")
        ),

        mainPanel(
            width = 9,
            rgl::rglwidgetOutput("brain3d", width = "100%", height = "650px")
        )
    )
)

# ── Shiny Server ─────────────────────────────────────────────────────────────

server <- function(input, output, session) {

    output$brain3d <- rgl::renderRglwidget({
        cm <- switch(input$data_layer,
            "thickness" = cm_thickness,
            "sulc"      = cm_sulc
        )

        fsbrain::vis.rglwidget(
            cm,
            background = "white",
            style      = "default"
        )
    })
}

# ── Run ──────────────────────────────────────────────────────────────────────

cat("[app] Starting Shiny server...\n")
shinyApp(ui, server)
