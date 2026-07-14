#!/usr/bin/env Rscript
#
# Camera Verification Script: rgl vs scimesh
# ============================================
#
# Renders the same brain surface views with both the rgl and scimesh
# renderer backends and saves the output images for visual comparison.
#
# Two test modes:
#   1. Single views — each of the 9 anatomical angles rendered
#      individually and saved with backend-specific suffixes.
#   2. Composite export — a tiled 4-view figure with colour bar,
#      using the export() function that produces the final
#      publication-ready output.
#
# Usage:
#   Rscript examples/rgl_vs_scimesh/camera_verification.R
#
# The script uses the optional demo subject 'subject1' shipped with
# fsbrain.  If the data hasn't been downloaded yet it will be fetched
# automatically.

library(fsbrain)


# -------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------

measure <- "sulc"
surface <- "white"
hemi    <- "both"
subject <- "subject1"

single_view_angles <- get.view.angle.names(angle_set = "all")
composite_view_angles <- get.view.angle.names(angle_set = "t4")

output_basename <- paste0(measure, "_", surface, "_both")


# -------------------------------------------------------------------
# Build coloured meshes (no rendering yet)
# -------------------------------------------------------------------
message("--- Preparing data ---")
subjects_dir <- sjd.demo()
message("Subjects dir: ", subjects_dir)

coloured_meshes <- vis.subject.morph.native(
    subjects_dir, subject, measure,
    hemi      = hemi,
    views     = NULL,          # suppress immediate rendering
    surface   = surface,
    cortex_only = FALSE
)


# ===================================================================
# Test 1: Single views (all 9 angles)
# ===================================================================
message("\n=== Test 1: Single views (9 angles) ===")

for (renderer in c("rgl", "scimesh")) {

    options(fsbrain.renderer_backend = renderer)
    cat(sprintf("\n--- Rendering with %s ---\n", renderer))

    for (view in single_view_angles) {
        view_name <- sub("^sd_", "", view)
        cat(sprintf("  %s...\n", view_name))

        out_path <- sprintf("%s_%s_%s.png",
                            output_basename, view_name, renderer)

        vislayout.from.coloredmeshes(
            coloured_meshes,
            view_angles   = view,
            output_img    = out_path,
            style         = "default",
            background_color = "white",
            silent        = TRUE,
            grid_like     = FALSE
        )
    }
}


# ===================================================================
# Test 2: Composite export with colour bar (4-view tile)
# ===================================================================
message("\n=== Test 2: Composite 4-view + colour bar ===")

for (renderer in c("rgl", "scimesh")) {

    options(fsbrain.renderer_backend = renderer)
    cat(sprintf("\n--- Exporting with %s ---\n", renderer))

    out_path <- sprintf("%s_t4_%s.png", output_basename, renderer)

    export(
        coloured_meshes,
        colorbar_legend = "Sulcal depth [mm]",
        view_angles     = composite_view_angles,
        output_img      = out_path,
        draw_colorbar   = "horizontal",
        style           = "default",
        background_color = "white",
        quality         = 1L,
        large_legend    = FALSE,
        silent          = TRUE
    )

    cat(sprintf("  -> %s\n", out_path))
}


# ===================================================================
# Summary: list all outputs and their sizes
# ===================================================================
message("\n=== Output files ===\n")
system(sprintf("identify %s_*_rgl.png %s_*_scimesh.png %s_t4_*.png",
               output_basename, output_basename, output_basename))
