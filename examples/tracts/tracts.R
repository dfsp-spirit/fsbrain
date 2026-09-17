#!/usr/bin/env Rscript
#
# tracts.R -- Visualize white matter tracts (streamlines) on the cortical surface.
#
# This is an example script that comes with 'fsbrain': https://github.com/dfsp-spirit/fsbrain
#
# The function 'vis.tracts' draws tractography streamlines as lines, with a
# semi-transparent brain surface as context. The streamlines can come from a
# tract file (TrackVis TRK or MRtrix TCK format), from a directory of such files
# (a tract atlas with one file per bundle), or from in-memory data. This is the R
# equivalent of the 'plot_tracts' function of the Python package 'yabplot'.
#
# The demo data is the XTRACT atlas of the 42 major white matter tracts
# (Warrington et al., 2020, https://doi.org/10.1126/sciadv.aba8245), which is
# downloaded into the package cache on demand. The streamlines are defined in
# MNI152 space, the template surfaces of fsbrain use a very similar (fsaverage
# like) space, so the overlay looks right; use the 'transform_matrix' parameter
# of 'vis.tracts' if you have an accurate transformation between the two spaces.
#
# Dependencies:
#   Requires the 'fsbrain' package. The fs_LR_32 template meshes are downloaded
#   into the package cache on demand (they are not part of FreeSurfer and not
#   subject to the FreeSurfer license). For the headless PNG rendering (the
#   default), the 'scimesh' package is required as well.
#
# USAGE: ./tracts.R <output_dir> [--renderer <rgl|scimesh>] [--atlas <name>]
#                              [--tck <file>] [--max-tracks <n>] [--orientation]
#
# OPTIONS:
#   --renderer <backend> : the renderer backend to use for image export, either 'rgl'
#                          (needs a working display) or 'scimesh' (headless software
#                          renderer that writes PNG images; requires the scimesh package).
#   --atlas <name>       : the tract atlas to use, one of 'xtract_tiny' (the default,
#                          a small download), 'xtract_small', 'xtract_medium' or
#                          'xtract_large'.
#   --tck <file>         : the path to a whole-brain tractogram in TCK format (e.g. from
#                          MRtrix3 or QSIRecon), optionally gzip-compressed. Such a
#                          tractogram contains millions of streamlines, so only the
#                          first ones are drawn (see '--max-tracks'). The template
#                          surfaces are the wrong context for a subject-space
#                          tractogram, so this figure is drawn without the context.
#   --max-tracks <n>     : the number of streamlines to read from a tractogram given
#                          with '--tck' (default: 20000).
#   --orientation        : color the streamlines by their direction (the classic DTI
#                          look) instead of by bundle value.
#
# Written by Tim Schaefer

library("fsbrain");

args = commandArgs(trailingOnly = TRUE);

# --- Parse command line arguments ---------------------------------------------

output_dir = ".";
renderer = NULL;
atlas = "xtract_tiny";
tck_file = NULL;
max_tracks = 20000L;
color_by_orientation = FALSE;

arg_idx = 1L;
while(arg_idx <= length(args)) {
    arg = args[arg_idx];
    if(arg == "--renderer") {
        arg_idx = arg_idx + 1L;
        renderer = args[arg_idx];
    } else if(arg == "--atlas") {
        arg_idx = arg_idx + 1L;
        atlas = args[arg_idx];
    } else if(arg == "--tck") {
        arg_idx = arg_idx + 1L;
        tck_file = args[arg_idx];
    } else if(arg == "--max-tracks") {
        arg_idx = arg_idx + 1L;
        max_tracks = as.integer(args[arg_idx]);
    } else if(arg == "--orientation") {
        color_by_orientation = TRUE;
    } else {
        output_dir = arg;
    }
    arg_idx = arg_idx + 1L;
}

if(! is.null(renderer)) {
    options(fsbrain.renderer_backend = renderer);
}
cat(sprintf("Renderer backend: %s\n", fsbrain::get.fsbrain.renderer.backend()));

# --- Data: the template meshes and the tract atlas -----------------------------

# The XTRACT atlas and the fs_LR_32 template meshes are downloaded into the package cache
# (this is a no-op if they are already there).
atlas_result = fsbrain::download_xtract_tracts(atlas, silent = TRUE);
atlas_dir = dirname(atlas_result$available[1L]);
cat(sprintf("Tract atlas '%s': %d bundles in '%s'.\n", atlas, length(atlas_result$available), atlas_dir));

if(! file.exists(file.path(fsbrain::get_optional_data_filepath("subjects_dir"), "fs_LR_32", "surf", "lh.midthickness"))) {
    cat("Downloading the fs_LR_32 template meshes.\n");
    fsbrain::download_fs_LR_32_meshes();
}

# --- Data: the tracts and the values to color them with ------------------------

bundles = fsbrain::read.tract.bundles(atlas_dir, silent = FALSE);
cat(sprintf("Read %d bundles with %d streamlines in total.\n", length(bundles), sum(sapply(bundles, length))));

# Any value per bundle can be mapped to colors, e.g. the mean fractional anisotropy of the
# bundle, or a statistical value like a group difference. This demo uses a sine wave over
# the alphabetical bundle order, just like the 'plot_tracts' demo of 'yabplot'.
bundle_values = sin(seq(0.0, 2.0 * pi, length.out = length(bundles)));
names(bundle_values) = names(bundles);
cat(sprintf("Bundle values: range [%.2f, %.2f] over %d bundles.\n", min(bundle_values), max(bundle_values), length(bundle_values)));

# --- Figure 1: the bundles, colored by value, in 3 views ----------------------

# 'views = NULL' only builds the renderables (the tract lines and the context surface) and
# does not render anything. The returned list is a regular fsbrain renderable list, so it
# can be passed to 'export()' (which also handles the colorbar) or to any 'vis.*' function.
tracts = fsbrain::vis.tracts(bundles, bundle_values = bundle_values, template_id = "fs_LR_32",
    context = list("surface" = "midthickness", "alpha" = 0.08, "color" = "#B0B0B0"),
    tract_width = 1.0, views = NULL, silent = TRUE);

img_bundles = file.path(output_dir, "tracts_bundles.png");
fsbrain::export(tracts, view_angles = c("sd_lateral_lh", "sd_rostral", "sd_dorsal"),
    draw_colorbar = "horizontal", colorbar_legend = "Value",
    output_img = img_bundles, silent = TRUE);
cat(sprintf("Wrote '%s'.\n", img_bundles));

# --- Figure 2: the same bundles, colored by orientation ------------------------

# The classic DTI look: red for left-right, green for anterior-posterior and blue for
# superior-inferior segments. The colors do not encode a value, so there is no colorbar.
tracts_orient = fsbrain::vis.tracts(bundles, color_by_orientation = TRUE, template_id = "fs_LR_32",
    context = list("surface" = "midthickness", "alpha" = 0.08, "color" = "#B0B0B0"),
    views = NULL, silent = TRUE);

img_orient = file.path(output_dir, "tracts_orientation.png");
fsbrain::export(tracts_orient, view_angles = c("sd_lateral_lh", "sd_rostral", "sd_dorsal"),
    draw_colorbar = FALSE, output_img = img_orient, silent = TRUE);
cat(sprintf("Wrote '%s'.\n", img_orient));

# --- Figure 3: without the context surface -------------------------------------

# Without a brain surface, the camera is placed based on the tract geometry alone. This
# is much faster than the figures above and gives an unobstructed view of the tracts.
tracts_bare = fsbrain::vis.tracts(bundles, bundle_values = bundle_values, context = NULL,
    tract_width = 1.5, views = NULL, silent = TRUE);
img_bare = file.path(output_dir, "tracts_no_context.png");
fsbrain::export(tracts_bare, view_angles = c("sd_lateral_lh", "sd_dorsal"),
    draw_colorbar = FALSE, output_img = img_bare, silent = TRUE);
cat(sprintf("Wrote '%s'.\n", img_bare));

# --- Figure 4 (optional): a whole-brain tractogram ------------------------------

# A subject tractogram from a tractography pipeline (e.g. MRtrix3, QSIRecon) contains
# millions of streamlines and is defined in the space of the subject, so the template
# surface is not a valid context for it. Only a subset of the streamlines is drawn.
if(! is.null(tck_file)) {
    cat(sprintf("Reading at most %d streamlines of the tractogram '%s'.\n", max_tracks, tck_file));
    tractogram = fsbrain::vis.tracts(tck_file, max_tracks = max_tracks, context = NULL,
        color_by_orientation = TRUE, views = NULL, silent = TRUE);
    img_tractogram = file.path(output_dir, "tractogram.png");
    fsbrain::export(tractogram, view_angles = c("sd_lateral_lh", "sd_rostral", "sd_dorsal"),
        draw_colorbar = FALSE, output_img = img_tractogram, silent = TRUE);
    cat(sprintf("Wrote '%s'.\n", img_tractogram));
}

cat("Done. See the function documentation of 'vis.tracts' and 'read.tract.bundles' for the details.\n");
