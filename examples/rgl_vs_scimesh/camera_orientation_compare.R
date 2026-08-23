#!/usr/bin/env Rscript
#
# Camera orientation / framing comparison: rgl vs scimesh backends.
# ================================================================
#
# Renders a real brain (demo subject1, morphometry data) with BOTH renderer
# backends for all 8 fsbrain view angles and saves the images side by side for
# inspection:
#
#   <outdir>/<view>_brain_rgl.png
#   <outdir>/<view>_brain_scimesh.png
#   <outdir>/<view>_brain_SIDE.png     (montage: rgl top, scimesh bottom)
#
# Optionally (--cube) also renders a face-coloured cube (6 faces, distinct
# colours, correct outward winding) for orientation debugging.
#
# KNOWN FINDING (2026-08-23): rgl and scimesh agree on the view direction for
# all views EXCEPT rostral and caudal, which are SWAPPED between the backends:
# rgl points the "rostral" camera at -y (it shows the caudal/posterior surface,
# since +y = anterior in FreeSurfer RAS), while scimesh points it at +y
# (anatomically correct). See dev_tools/TODO_FSBRAIN_RGL_CAM.md (Step 2).
#
# Usage:
#   Rscript examples/rgl_vs_scimesh/camera_orientation_compare.R [outdir] [--cube]
#   (outdir defaults to ~/fsbrain_camera_compare2)
#
# Related: examples/rgl_vs_scimesh/camera_verification.R (older, framing-only).
# See dev_tools/TODO_FSBRAIN_RGL_CAM.md (Step 2).

suppressPackageStartupMessages({library(fsbrain); library(magick)})

args <- commandArgs(trailingOnly = TRUE)
do_cube <- any(args == "--cube")
arg_outdir <- args[args != "--cube"]
outdir <- if (length(arg_outdir) >= 1L) arg_outdir[1] else file.path(path.expand("~"), "fsbrain_camera_compare2")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
cat("Output directory:", outdir, "\n")


# ---------------------------------------------------------------------------
# 1. Face-coloured cube.
#    Six faces, each a single distinct colour. Vertices are duplicated per
#    face (24 vertices / 12 triangles) so every face can carry its own colour.
#    Winding is counter-clockwise seen from OUTSIDE (verified via the outward
#    normal = cross(v1-v0, v2-v0)), so both renderers agree on back-face
#    culling and every outside view shows exactly the faces whose normal
#    points toward the camera.
# ---------------------------------------------------------------------------
build_face_cube <- function() {
    h <- 0.5
    faces <- list(
        "+z" = rbind(c(-h, -h,  h), c( h, -h,  h), c( h,  h,  h), c(-h,  h,  h)),
        "-z" = rbind(c(-h, -h, -h), c(-h,  h, -h), c( h,  h, -h), c( h, -h, -h)),
        "-x" = rbind(c(-h, -h, -h), c(-h, -h,  h), c(-h,  h,  h), c(-h,  h, -h)),
        "+x" = rbind(c( h, -h, -h), c( h,  h, -h), c( h,  h,  h), c( h, -h,  h)),
        "+y" = rbind(c(-h,  h, -h), c(-h,  h,  h), c( h,  h,  h), c( h,  h, -h)),
        "-y" = rbind(c(-h, -h, -h), c( h, -h, -h), c( h, -h,  h), c(-h, -h,  h))
    )
    face_colors <- c("+z" = "#FF0000", "-z" = "#00FF00", "-x" = "#0000FF",
                     "+x" = "#FFFF00", "+y" = "#00FFFF", "-y" = "#FF00FF")
    V <- do.call(rbind, faces)
    it <- do.call(rbind, lapply(seq_along(faces), function(f) {
        off <- (f - 1L) * 4L
        rbind(c(off + 1L, off + 2L, off + 3L), c(off + 1L, off + 3L, off + 4L))
    }))
    col <- rep(face_colors, each = 4L)
    tmesh <- rgl::tmesh3d(t(cbind(V, 1)), it)
    return(structure(list(mesh = tmesh, col = col, render = TRUE),
                     class = "fs.coloredmesh"))
}


# ---------------------------------------------------------------------------
# 2. Real brain colouredmeshes (demo subject1).
# ---------------------------------------------------------------------------
build_brain <- function() {
    subjects_dir <- fsbrain::get_optional_data_filepath("subjects_dir")
    if (!dir.exists(file.path(subjects_dir, "subject1"))) {
        stop("Demo subject1 not found. Run fsbrain::download_optional_data() first.")
    }
    return(fsbrain::vis.subject.morph.native(
        subjects_dir, "subject1", "sulc", hemi = "both",
        views = NULL, surface = "white"))
}


# ---------------------------------------------------------------------------
# Rendering helpers.
# ---------------------------------------------------------------------------
render_view <- function(coloredmeshes, view, backend, outdir, tag) {
    options(fsbrain.renderer_backend = backend)
    f <- file.path(outdir, sprintf("%s_%s.png", tag, backend))
    fsbrain::vislayout.from.coloredmeshes(
        coloredmeshes, view_angles = view, output_img = f, silent = TRUE,
        rgloptions = list("windowRect" = c(0, 0, 800, 800)))
    while (rgl::cur3d() > 0) rgl::close3d()
    return(f)
}


views <- fsbrain::get.view.angle.names(angle_set = "all")
brain <- build_brain()

for (view in views) {
    tag <- sub("^sd_", "", view)
    fr <- render_view(brain, view, "rgl", outdir, paste0(tag, "_brain"))
    fs_ <- render_view(brain, view, "scimesh", outdir, paste0(tag, "_brain"))
    a <- image_scale(image_read(fr), "600x")
    b <- image_scale(image_read(fs_), "600x")
    image_write(image_append(c(a, b), stack = TRUE),
                file.path(outdir, sprintf("%s_brain_SIDE.png", tag)))
    cat("brain ", tag, " written.\n")
}

if (do_cube) {
    cube <- build_face_cube()
    for (view in views) {
        tag <- sub("^sd_", "", view)
        fr <- render_view(list(cube), view, "rgl", outdir, paste0(tag, "_cube"))
        fs_ <- render_view(list(cube), view, "scimesh", outdir, paste0(tag, "_cube"))
        a <- image_scale(image_read(fr), "600x")
        b <- image_scale(image_read(fs_), "600x")
        image_write(image_append(c(a, b), stack = TRUE),
                    file.path(outdir, sprintf("%s_cube_SIDE.png", tag)))
        cat("cube  ", tag, " written.\n")
    }
}

cat("Done. Inspect images in:", outdir, "\n")
