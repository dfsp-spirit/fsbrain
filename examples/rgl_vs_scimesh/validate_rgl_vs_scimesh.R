#!/usr/bin/env Rscript
#
# validate_rgl_vs_scimesh.R
# =========================
#
# High-level VISUAL validation of the important fsbrain features exercised in
# web/Rmd_web_examples/fsbrain_with_scimesh.Rmd. This is a faithful plain-R port
# of that document: it runs each feature with a SINGLE renderer backend and
# writes one numbered PNG per feature into the current working directory (or
# --outdir), named '<NN>_<feature>_<backend>.png'.
#
# Run it ONCE PER BACKEND, then compare the two image sets in your OS viewer:
#
#   Rscript examples/rgl_vs_scimesh/validate_rgl_vs_scimesh.R --backend rgl
#   Rscript examples/rgl_vs_scimesh/validate_rgl_vs_scimesh.R --backend scimesh
#
# Optional: build side-by-side montages (rgl top, scimesh bottom) from the
# already-rendered _rgl.png / _scimesh.png pairs, named '<NN>_<feature>_SIDE.png':
#
#   Rscript examples/rgl_vs_scimesh/validate_rgl_vs_scimesh.R --montage
#
# (--montage can also be combined with a backend run, e.g. '--backend scimesh
# --montage', in which case it runs after rendering that backend's images.)
#
# Parameters:
#   --backend rgl|scimesh   the renderer backend to use (default: 'rgl')
#   --montage               build _SIDE.png montages from existing _rgl/_scimesh pairs
#   --outdir <dir>          directory for output images (default: '.', the CWD)
#
# Requirements:
#   - The current fsbrain development version must be installed
#     (R CMD build . && R CMD INSTALL ./fsbrain_*.tar.gz).
#   - magick is required (image composition in export(); it is an fsbrain dependency).
#   - scimesh must be installed when using --backend scimesh (it is in fsbrain Suggests).
#   - The rgl backend needs a display or Xvfb; scimesh is fully headless.
#   - Runs download_optional_data() and download_fsaverage() (which implies
#     accepting the FreeSurfer license) if the demo data is missing. Both are
#     no-ops if the data is already cached.
#
# Background: this validates the camera-based view unification work
# (dev_tools/TODO_FSBRAIN_RGL_CAM.md) after the rgl backend was migrated from
# mesh-rotation to camera positioning (see CHANGES 0.8.0). Both backends now
# share the same framing, so the images should look essentially identical
# (modulo antialiasing / material differences); this script makes that visible.

suppressPackageStartupMessages(library(fsbrain));

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
OUTPUT_DIMS <- c(1000L, 1000L);   # matches rgl export() quality=1L per-tile resolution

# ---------------------------------------------------------------------------
# Command line arguments
# ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE);
backend <- "rgl";
do_montage <- FALSE;
outdir <- ".";
i <- 1L;
while(i <= length(args)) {
  a <- args[[i]];
  if(a == "--backend") {
    i <- i + 1L;
    if(i > length(args)) { stop("Missing value for '--backend'."); }
    backend <- args[[i]];
  } else if(a == "--montage") {
    do_montage <- TRUE;
  } else if(a == "--outdir") {
    i <- i + 1L;
    if(i > length(args)) { stop("Missing value for '--outdir'."); }
    outdir <- args[[i]];
  } else {
    stop(sprintf("Unknown argument '%s'.", a));
  }
  i <- i + 1L;
}
if(! backend %in% c("rgl", "scimesh")) {
  stop(sprintf("Invalid backend '%s'. Use '--backend rgl' or '--backend scimesh'.", backend));
}
dir.create(outdir, showWarnings = FALSE, recursive = TRUE);
cat(sprintf("Backend: %s | Output dir: %s\n", backend, outdir));

# ---------------------------------------------------------------------------
# Set up the renderer backend
# ---------------------------------------------------------------------------
options(fsbrain.renderer_backend = backend);
if(backend == "scimesh") {
  if(! requireNamespace("scimesh", quietly = TRUE)) {
    stop("Backend is 'scimesh' but the scimesh package is not installed. Install it with install.packages('scimesh').");
  }
  options(fsbrain.scimesh.output_dims = OUTPUT_DIMS);
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
close.rgl.windows <- function() {
  if(requireNamespace("rgl", quietly = TRUE)) {
    while(rgl::cur3d() > 0L) rgl::close3d();
  }
}

# Render one feature with the current backend to '<NN>_<feature>_<backend>.png'.
render.export <- function(num, feature, coloredmeshes, ...) {
  f <- file.path(outdir, sprintf("%02d_%s_%s.png", num, feature, backend));
  export(coloredmeshes, output_img = f, silent = TRUE, ...);
  cat(sprintf("  %02d_%s -> %s\n", num, feature, f));
  close.rgl.windows();
  return(invisible(f));
}

# Build side-by-side montages (rgl top, scimesh bottom) from existing pairs.
build.montages <- function(outdir) {
  if(! requireNamespace("magick", quietly = TRUE)) {
    warning("magick is not installed, cannot build montages.");
    return(invisible(NULL));
  }
  rgl_files <- sort(list.files(outdir, pattern = "_rgl\\.png$", full.names = TRUE));
  sm_files <- sort(list.files(outdir, pattern = "_scimesh\\.png$", full.names = TRUE));
  rgl_base <- sub("_rgl\\.png$", "", basename(rgl_files));
  sm_base <- sub("_scimesh\\.png$", "", basename(sm_files));
  common <- intersect(rgl_base, sm_base);
  if(length(common) == 0L) {
    cat("No _rgl/_scimesh pairs found, no montages built.\n");
    return(invisible(NULL));
  }
  for(base in common) {
    r <- file.path(outdir, sprintf("%s_rgl.png", base));
    s <- file.path(outdir, sprintf("%s_scimesh.png", base));
    side <- file.path(outdir, sprintf("%s_SIDE.png", base));
    a <- magick::image_scale(magick::image_read(r), "1000x");
    b <- magick::image_scale(magick::image_read(s), "1000x");
    magick::image_write(magick::image_append(c(a, b), stack = TRUE), side);
    cat(sprintf("  %s -> %s\n", base, side));
  }
  missing_r <- setdiff(sm_base, rgl_base);
  missing_s <- setdiff(rgl_base, sm_base);
  if(length(missing_r) > 0L) cat("Skipped (no _rgl image):", paste(missing_r, collapse = ", "), "\n");
  if(length(missing_s) > 0L) cat("Skipped (no _scimesh image):", paste(missing_s, collapse = ", "), "\n");
  return(invisible(NULL));
}

# ---------------------------------------------------------------------------
# Data: demo subject 'subject1' and the 'fsaverage' template
# ---------------------------------------------------------------------------
cat("--- Preparing data (downloads only if missing) ---\n");
download_optional_data();
download_fsaverage(accept_freesurfer_license = TRUE);
sjd <- get_optional_data_filepath("subjects_dir");
sj <- "subject1";
cat("Subjects dir:", sjd, "\n");

# ---------------------------------------------------------------------------
# Part 1: the export() API
# ---------------------------------------------------------------------------
cat("\n=== Part 1: the export() API ===\n");

# 01 -- basic export (t4 grid + colourbar legend)
cat("01 basic_export...\n");
cm <- vis.subject.morph.standard(sjd, sj, 'thickness', fwhm='10', cortex_only=TRUE, views=NULL);
render.export(1L, "basic_export", cm, colorbar_legend = 'Cortical thickness [mm]');

# 02 -- custom view angles (annotation, only the two medial views)
cat("02 annot_medial_views...\n");
cm <- vis.subject.annot(sjd, sj, 'aparc', views=NULL);
render.export(2L, "annot_medial_views", cm, view_angles = c("sd_medial_lh", "sd_medial_rh"));

# 03 -- t8 grid with clipped curvature
cat("03 curv_t8_clip...\n");
cm <- vis.subject.morph.native(sjd, sj, 'curv', cortex_only=TRUE, views=NULL, rglactions=list('trans_fun'=clip.data));
render.export(3L, "curv_t8_clip", cm, view_angles = get.view.angle.names(angle_set = "t8"), colorbar_legend = 'Mean curvature [mm^-1]');

# 04-07 -- sulc in standard space; same coloredmeshes reused for the variants
cat("04-07 sulc_standard (coloredmeshes reused)...\n");
cm_sulc <- vis.subject.morph.standard(sjd, sj, 'sulc', fwhm='10', cortex_only=TRUE, views=NULL);

# 04 -- vertical strip instead of a grid
render.export(4L, "sulc_vertical_strip", cm_sulc, view_angles = get.view.angle.names("t4"), grid_like = FALSE, colorbar_legend = 'Sulcal depth [mm]');

# 05 -- vertical colourbar
render.export(5L, "sulc_colorbar_vertical", cm_sulc, colorbar_legend = 'Sulcal depth [mm]', draw_colorbar = 'vertical');

# 06 -- black background, no colourbar
render.export(6L, "sulc_black_bg", cm_sulc, view_angles = c("sd_medial_lh", "sd_medial_rh"), background_color = '#000000', draw_colorbar = FALSE);

# 07 -- transparent background
render.export(7L, "sulc_transparent_bg", cm_sulc, view_angles = c("sd_medial_lh", "sd_medial_rh"), transparency_color = '#FFFFFF');

# ---------------------------------------------------------------------------
# Part 2: region-based and vertex-based results
# ---------------------------------------------------------------------------
cat("\n=== Part 2: region-based and vertex-based results ===\n");

# 08 -- one value per atlas region (Desikan / aparc), simulated data
cat("08 region_values_aparc...\n");
atlas <- 'aparc';
lh_region_value_list <- list("bankssts"=0.9, "precuneus"=0.7, "postcentral"=0.8, "lingual"=0.6);
atlas_region_names <- get.atlas.region.names(atlas, template_subjects_dir = sjd, template_subject = sj);
set.seed(42);   # fixed seed: the simulated RH region values are random (rnorm), so seed them
                # to make the image reproducible across backend runs / re-runs
rh_region_value_list <- rnorm(length(atlas_region_names), 0.8, 0.2);
names(rh_region_value_list) <- atlas_region_names;
cm <- vis.region.values.on.subject(sjd, sj, atlas, lh_region_value_list, rh_region_value_list, views=NULL);
render.export(8L, "region_values_aparc", cm, colorbar_legend = 'Effect size (simulated data)');

# 09 -- symmetric data (t-values) on fsaverage with curvature background
cat("09 symmetric_clusters...\n");
subjects_dir <- get_optional_data_filepath("subjects_dir");
subject_id <- 'fsaverage';
lh_demo_cluster_file <- system.file("extdata", "lh.clusters_fsaverage.mgz", package = "fsbrain", mustWork = TRUE);
rh_demo_cluster_file <- system.file("extdata", "rh.clusters_fsaverage.mgz", package = "fsbrain", mustWork = TRUE);
lh_clust <- freesurferformats::read.fs.morph(lh_demo_cluster_file);
rh_clust <- freesurferformats::read.fs.morph(rh_demo_cluster_file);
cm <- vis.symmetric.data.on.subject(subjects_dir, subject_id, lh_clust, rh_clust, bg="curv_light", views=NULL);
render.export(9L, "symmetric_clusters", cm, colorbar_legend = 't-value (simulated data)');

# ---------------------------------------------------------------------------
# Part 3: a workflow with manually loaded meshes and data
# ---------------------------------------------------------------------------
cat("\n=== Part 3: manual workflow with preloaded data ===\n");

# 10 -- no subjects_dir at all: load meshes + data manually, build coloredmeshes
cat("10 manual_preloaded...\n");
lh_surf_file <- get_optional_data_filepath(file.path("subjects_dir", "subject1", "surf", "lh.white"));
rh_surf_file <- get_optional_data_filepath(file.path("subjects_dir", "subject1", "surf", "rh.white"));
lh_thick_file <- get_optional_data_filepath(file.path("subjects_dir", "subject1", "surf", "lh.thickness"));
rh_thick_file <- get_optional_data_filepath(file.path("subjects_dir", "subject1", "surf", "rh.thickness"));
lh_surf <- freesurferformats::read.fs.surface(lh_surf_file);
rh_surf <- freesurferformats::read.fs.surface(rh_surf_file);
lh_thick <- freesurferformats::read.fs.morph(lh_thick_file);
rh_thick <- freesurferformats::read.fs.morph(rh_thick_file);
cm_lh <- coloredmesh.from.preloaded.data(lh_surf, morph_data = lh_thick, hemi = "lh");
cm_rh <- coloredmesh.from.preloaded.data(rh_surf, morph_data = rh_thick, hemi = "rh");
render.export(10L, "manual_preloaded", list("lh" = cm_lh, "rh" = cm_rh), colorbar_legend = 'Cortical thickness [mm]');

# ---------------------------------------------------------------------------
# Optional: build side-by-side montages
# ---------------------------------------------------------------------------
if(do_montage) {
  cat("\n=== Building montages ===\n");
  build.montages(outdir);
}

cat(sprintf("\nDone. Inspect the %s images in '%s'.\n", backend, outdir));
