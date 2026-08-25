#!/usr/bin/env Rscript
#
# vol_slices.R
# ============
#
# Demo for the volume-slice upscaling feature added in commit 2eb824e
# (fsbrain 0.8.0): exports LARGER brain volume slices via the new `scale`
# parameter of `volvis.slices.with.surface()` and `volvis.lb.with.surface()`.
#
# With `scale=2L` (used here) the MRI slice images are nearest-neighbor
# upscaled 2x in both dimensions, which keeps the pixels sharp/blocky (ideal
# for QA). The surface contour lines are drawn on a high-resolution transparent
# overlay which is downsampled back to the final size, so the lines stay thin
# and smooth on top of the enlarged images.
#
# The script uses the 'subject1' demo subject (downloaded with
# download_optional_data()) and writes all output PNGs into the directory of
# this script, so the images land next to the R file no matter where the script
# is invoked from.
#
# Usage (from anywhere):
#   Rscript examples/vol_slices/vol_slices.R
#
# Optional arguments:
#   --slices N     slice spacing: a negative integer N means "every Nth slice",
#                  a numeric vector gives explicit 1-based slice indices
#                  (default: -5)
#   --no-skip      do NOT skip slices without any surface contour
#                  (default: skip empty slices)
#   --no-lightbox  do not write the per-axis lightbox overview images
#
# Requirements: the current fsbrain development version (with the `scale`
# parameter) must be installed, and the magick package (an fsbrain dependency).

suppressPackageStartupMessages(library(fsbrain));
suppressPackageStartupMessages(library(magick));

# ---------------------------------------------------------------------------
# Determine the directory of this script, so output images always land next to
# the R file regardless of the working directory the script is launched from.
# ---------------------------------------------------------------------------
get_script_dir <- function() {
    args <- commandArgs(trailingOnly = FALSE);
    file_arg <- sub("^--file=", "", args[grep("^--file=", args)]);
    if(length(file_arg) > 0L) {
        return(dirname(normalizePath(file_arg)));
    }
    return(getwd());
}
setwd(get_script_dir());
cat(sprintf("Working directory (output images go here): %s\n", getwd()));

# ---------------------------------------------------------------------------
# Command line arguments
# ---------------------------------------------------------------------------
slices <- -5L;
skip_empty <- TRUE;
write_lightbox <- TRUE;
args <- commandArgs(trailingOnly = TRUE);
i <- 1L;
while(i <= length(args)) {
    a <- args[[i]];
    if(a == "--slices") {
        i <- i + 1L;
        if(i > length(args)) { stop("Missing value for '--slices'."); }
        slices <- as.integer(strsplit(args[[i]], ",")[[1L]]);
    } else if(a == "--no-skip") {
        skip_empty <- FALSE;
    } else if(a == "--no-lightbox") {
        write_lightbox <- FALSE;
    } else {
        stop(sprintf("Unknown argument '%s'.", a));
    }
    i <- i + 1L;
}

# ---------------------------------------------------------------------------
# Ensure the optional demo data (subject1) is available, get subjects_dir.
# ---------------------------------------------------------------------------
download_optional_data();
subjects_dir <- get_optional_data_filepath("subjects_dir");
cat(sprintf("Using subjects_dir: %s\n", subjects_dir));

subject_id <- "subject1";

# ---------------------------------------------------------------------------
# Configuration for the slice visualization
# ---------------------------------------------------------------------------
volume        <- "brain";                                 # MRI volume from subject's mri/
surface       <- c("white", "pial");                      # contours from both surfaces
# Color assignment (see volvis.lb.with.surface docs): for surface=c("white","pial")
# and hemi="both", colors are assigned in order: white lh, white rh, pial lh, pial rh.
surface_color <- c("#FF0000", "#0000FF", "#00FF00", "#FF8800");
scale         <- 2L;                                      # <-- the upscaling feature
axis_names    <- c("sagittal", "coronal", "axial");

cat(sprintf("scale=%d, slices=%s, skip_empty=%s\n",
    scale, paste(slices, collapse = ","), skip_empty));

# ---------------------------------------------------------------------------
# 1) Export individual slices (with labeled surface contours) per axis.
# ---------------------------------------------------------------------------
for(axis in 1:3) {
    out_files <- volvis.slices.with.surface(subjects_dir, subject_id,
        volume = volume, surface = surface, hemi = "both",
        surface_color = surface_color,
        slices = slices, axis = axis, scale = scale,
        output_dir = ".", label_slices = TRUE, skip_empty = skip_empty,
        silent = FALSE);
    cat(sprintf("  %s (axis %d): exported %d slice image(s).\n",
        axis_names[axis], axis, length(out_files)));
}

# ---------------------------------------------------------------------------
# 2) Lightbox overview per axis (quick visual inspection of the full stack).
# ---------------------------------------------------------------------------
if(write_lightbox) {
    for(axis in 1:3) {
        lb <- volvis.lb.with.surface(subjects_dir, subject_id,
            volume = volume, surface = surface, hemi = "both",
            surface_color = surface_color,
            slices = slices, axis = axis, scale = scale,
            silent = FALSE);
        fname <- sprintf("subject1_lightbox_%s_axis%d_scl%d.png",
            axis_names[axis], axis, scale);
        magick::image_write(lb, path = fname, format = "png");
        cat(sprintf("  Wrote lightbox overview: %s (%dx%d)\n",
            fname, magick::image_info(lb)$width, magick::image_info(lb)$height));
    }
}

cat(sprintf("Done. All output images were written to: %s\n", getwd()));
