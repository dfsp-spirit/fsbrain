#!/usr/bin/env Rscript
#'
#' fsbrain Shiny Demo — Interactive 3D brain visualization in a web browser.
#'
#' This app demonstrates how to embed fsbrain's 3D brain renderings
#' into an R Shiny application using the vis.rglwidget() function.
#'
#' Requirements:
#'   - R packages: shiny, fsbrain (>= 0.5.6 with vis.rglwidget), rgl
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

cat("[setup] Downloading example subject data...\n")
fsbrain::download_optional_data()

cat("[setup] Downloading minimal fsaverage template (surfaces + cortex labels only)...\n")
# Download only the essential fsaverage files needed for visualization:
# white surfaces, curvature, cortex labels, and license.
# This is ~7 files instead of 19, making it much faster on constrained
# platforms like Posit Connect Cloud (avoids the 60s startup timeout).
pkg_info <- pkgfilecache::get_pkg_info("fsbrain")
fsavg_base <- c("subjects_dir", "fsaverage")
fsavg_files <- list(
    c(fsavg_base, "surf",  "lh.white"),
    c(fsavg_base, "surf",  "rh.white"),
    c(fsavg_base, "surf",  "lh.curv"),
    c(fsavg_base, "surf",  "rh.curv"),
    c(fsavg_base, "label", "lh.cortex.label"),
    c(fsavg_base, "label", "rh.cortex.label"),
    c(fsavg_base, "LICENSE")
)
fsavg_md5 <- c(
    "cbffce8198e0e10c17f79f6ae0454af5",  # lh.white
    "1159a9ee160b1b0c76e0bb9ae789b9be",  # rh.white
    "3e81598a5ac0546443ec37d0ac477c80",  # lh.curv
    "76ad91d2488de081392313ad5a87fafb",  # rh.curv
    "578f81e9946a76eb1c42d897d07da4a7",  # lh.cortex.label
    "c8f59de23e9f90f18e96e9d037e42799",  # rh.cortex.label
    "b39610adfe02fdce2ad9d30797c567b3"   # LICENSE
)
fsavg_urls <- paste0("https://rcmd.org/projects/nitestdata/subjects_dir/fsaverage/",
    c("surf/lh.white", "surf/rh.white",
      "surf/lh.curv", "surf/rh.curv",
      "label/lh.cortex.label", "label/rh.cortex.label",
      "LICENSE"))
pkgfilecache::ensure_files_available(pkg_info, fsavg_files, fsavg_urls, md5sums = fsavg_md5)

subjects_dir <- fsbrain::get_optional_data_filepath("subjects_dir")

cat("[setup] Pre-building coloredmeshes (cached for all sessions)...\n")

# Build coloredmeshes once — they're expensive to compute and don't change.
cm_annot <- vis.subject.annot(
    subjects_dir, "subject1", "aparc", "both",
    rglactions = list("no_vis" = TRUE)
)

cm_morph <- vis.subject.morph.standard(
    subjects_dir, "subject1", "thickness", "both",
    fwhm = "10", cortex_only = TRUE,
    rglactions = list("no_vis" = TRUE)
)

cm_sulc <- vis.subject.morph.standard(
    subjects_dir, "subject1", "sulc", "both",
    fwhm = "10", cortex_only = TRUE,
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
                    "Cortical Parcellation (aparc)" = "annot",
                    "Cortical Thickness"           = "thickness",
                    "Sulcal Depth"                 = "sulc"
                ),
                selected = "annot"
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
            "annot"     = cm_annot,
            "thickness" = cm_morph,
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
