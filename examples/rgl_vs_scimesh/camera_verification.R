#!/usr/bin/env Rscript
#
# Camera Verification Script: rgl vs scimesh
# ============================================
#
# Renders the same brain surface views with both the rgl and scimesh
# renderer backends and saves the output images for visual comparison.
#
# Each view is rendered twice, once per backend.  Output files carry
# an _rgl or _scimesh suffix before the .png extension.
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

# Which data to visualise
measure <- "sulc"
surface <- "white"
hemi    <- "both"
subject <- "subject1"

# Which view angles to compare.  Single view is best for initial
# verification; use get.view.angle.names("all") to test everything.
# The default below is the standard 4-view (t4) layout angles.
view_angles <- get.view.angle.names(angle_set = "t4")

# Output image filename (without extension — suffixes are appended).
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


# -------------------------------------------------------------------
# Render with rgl backend
# -------------------------------------------------------------------
message("--- Rendering with rgl ---")
options(fsbrain.renderer_backend = "rgl")
cat(sprintf("  Backend: %s\n", get.fsbrain.renderer.backend()))

for (view in view_angles) {
    cat(sprintf("  Rendering view '%s'...\n", view))

    out_path <- sprintf("%s_%s_rgl.png", output_basename,
                        sub("^sd_", "", view))

    vislayout.from.coloredmeshes(
        coloured_meshes,
        view_angles   = view,
        output_img    = out_path,
        style         = "default",
        background_color = "white",
        silent        = TRUE,
        grid_like     = FALSE
    )

    cat(sprintf("    -> %s\n", out_path))
}


# -------------------------------------------------------------------
# Render with scimesh backend
# -------------------------------------------------------------------
message("--- Rendering with scimesh ---")
options(fsbrain.renderer_backend = "scimesh")
cat(sprintf("  Backend: %s\n", get.fsbrain.renderer.backend()))

for (view in view_angles) {
    cat(sprintf("  Rendering view '%s'...\n", view))

    out_path <- sprintf("%s_%s_scimesh.png", output_basename,
                        sub("^sd_", "", view))

    vislayout.from.coloredmeshes(
        coloured_meshes,
        view_angles   = view,
        output_img    = out_path,
        style         = "default",
        background_color = "white",
        silent        = TRUE,
        grid_like     = FALSE
    )

    cat(sprintf("    -> %s\n", out_path))
}


# -------------------------------------------------------------------
# Summary
# -------------------------------------------------------------------
message("\n--- Done. Output files ---")
for (view in view_angles) {
    view_name <- sub("^sd_", "", view)
    cat(sprintf("%s_%s_rgl.png\n",     output_basename, view_name))
    cat(sprintf("%s_%s_scimesh.png\n", output_basename, view_name))
}
