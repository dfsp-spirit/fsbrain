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

# ── One-time setup (cached) ──────────────────────────────────────────────────

cat("[setup] Downloading example data and fsaverage template...\n")
fsbrain::download_optional_data()
fsbrain::download_fsaverage(accept_freesurfer_license = TRUE)
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
