#!/usr/bin/env Rscript
#
# run_fsbrain_example.R
# =====================
#
# Plain-R port of the example code from the 'fsbrain with the scimesh
# rendering backend' vignette (vignettes/fsbrain_with_scimesh.Rmd and its
# rendered companion web/Rmd_web_examples/fsbrain_with_scimesh.Rmd). It is
# meant to run inside the fsbrain docker image, mounted at /home/input, and
# writes one numbered PNG per feature to /home/output.
#
# Invoked by the docker test harness (test_image.sh in this directory):
#   docker run --rm \
#     -v .../input:/home/input:ro \
#     -v .../output:/home/output \
#     -v <host-cache>:/fsbrain_data/fsbrain \
#     <image> Rscript /home/input/run_fsbrain_example.R
#
# Data downloaded by download_optional_data() and download_fsaverage() is
# cached in the directory given by the 'pkgfilecache.cachedir' option. Note
# that pkgfilecache appends the package name ('fsbrain') to that option
# value, so the final cache path is /fsbrain_data/fsbrain -- that is the
# container path the harness bind-mounts from the host (see test_image.sh).
#
# The scimesh software renderer backend is used (as also configured in this
# image's .Rprofile), so this runs fully headless without X11/OpenGL/GPU.

suppressPackageStartupMessages(library('fsbrain'));

# --- Configuration ------------------------------------------------------------
OUTPUT_DIR <- "/home/output";   # mounted from host, see test_image.sh
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE);

# Use the scimesh backend explicitly (matches the image defaults, but makes
# this script self-contained and independent of any .Rprofile settings).
options(fsbrain.renderer_backend = "scimesh");
options(fsbrain.scimesh.output_dims = c(2560, 1440));
cat(sprintf("Renderer backend: %s\n", get.fsbrain.renderer.backend()));

# The data cache lives at /fsbrain_data (bind-mounted from the host). Setting
# 'pkgfilecache.cachedir' makes pkgfilecache use exactly this root, no matter
# which HOME/user the container runs as. pkgfilecache appends the package
# name, so the actual cache dir is /fsbrain_data/fsbrain -- that is the
# container path the harness mounts from the host (see test_image.sh).
options(pkgfilecache.cachedir = "/fsbrain_data");
dir.create("/fsbrain_data", showWarnings = FALSE, recursive = TRUE);

# --- Example data -------------------------------------------------------------
cat("Preparing example data (downloads only if not cached)...\n");
download_optional_data();
download_fsaverage(accept_freesurfer_license = TRUE);
sjd <- get_optional_data_filepath("subjects_dir");
sj <- 'subject1';

# Helper: render a coloredmeshes instance to a numbered PNG in OUTPUT_DIR.
render <- function(num, feature, cm, ...) {
    outfile <- file.path(OUTPUT_DIR, sprintf("%02d_%s.png", num, feature));
    export(cm, output_img = outfile, ...);
    cat(sprintf("  wrote %s\n", outfile));
}

# --- Part 1: Different ways to use the export API ------------------------------
cat("\n[Part 1] The export API\n");

cat("  basic export: standard-space cortical thickness (t4)\n");
cm <- vis.subject.morph.standard(sjd, sj, 'thickness', fwhm = '10', cortex_only = TRUE, views = NULL);
render(1, "thickness_t4", cm, colorbar_legend = 'Cortical thickness [mm]');

cat("  custom view angles: annotation, two medial views\n");
cm <- vis.subject.annot(sjd, sj, 'aparc', views = NULL);
render(2, "annot_medial", cm, view_angles = c("sd_medial_lh", "sd_medial_rh"));

cat("  larger grid: all 8 views of native-space mean curvature (clipped)\n");
cm <- vis.subject.morph.native(sjd, sj, 'curv', cortex_only = TRUE, views = NULL, rglactions = list('trans_fun' = clip.data));
render(3, "curv_t8", cm, view_angles = get.view.angle.names(angle_set = "t8"), colorbar_legend = 'Mean curvature [mm^-1]');

cat("  forced vertical strip: standard-space sulcal depth (t4)\n");
cm <- vis.subject.morph.standard(sjd, sj, 'sulc', fwhm = '10', cortex_only = TRUE, views = NULL);
render(4, "sulc_t4_strip", cm, view_angles = get.view.angle.names("t4"), grid_like = FALSE, colorbar_legend = 'Sulcal depth [mm]');

cat("  vertical colorbar\n");
cm <- vis.subject.morph.standard(sjd, sj, 'sulc', fwhm = '10', cortex_only = TRUE, views = NULL);
render(5, "sulc_vertical_colorbar", cm, colorbar_legend = 'Sulcal depth [mm]', draw_colorbar = 'vertical');

cat("  black background\n");
cm <- vis.subject.morph.standard(sjd, sj, 'sulc', fwhm = '10', cortex_only = TRUE, views = NULL);
render(6, "sulc_black_bg", cm, view_angles = c("sd_medial_lh", "sd_medial_rh"), background_color = '#000000', draw_colorbar = FALSE);

cat("  transparent background\n");
cm <- vis.subject.morph.standard(sjd, sj, 'sulc', fwhm = '10', cortex_only = TRUE, views = NULL);
render(7, "sulc_transparent", cm, view_angles = c("sd_medial_lh", "sd_medial_rh"), transparency_color = '#FFFFFF');

# --- Part 2: Region-based and vertex-based results -----------------------------
cat("\n[Part 2] Region-based and vertex-based results\n");

cat("  region-based: simulated values on the Desikan atlas (subject1)\n");
atlas <- 'aparc';   # Desikan atlas
lh_region_value_list <- list("bankssts" = 0.9, "precuneus" = 0.7, "postcentral" = 0.8, "lingual" = 0.6);
atlas_region_names <- get.atlas.region.names(atlas, template_subjects_dir = sjd, template_subject = sj);
rh_region_value_list <- rnorm(length(atlas_region_names), 0.8, 0.2);
names(rh_region_value_list) <- atlas_region_names;
cm <- vis.region.values.on.subject(sjd, sj, atlas, lh_region_value_list, rh_region_value_list, views = NULL);
render(8, "region_values", cm, colorbar_legend = 'Effect size (simulated data)');

cat("  vertex-based: demo clusters on fsaverage with symmetric colormap\n");
subjects_dir <- get_optional_data_filepath("subjects_dir");
subject_id <- 'fsaverage';
lh_demo_cluster_file <- system.file("extdata", "lh.clusters_fsaverage.mgz", package = "fsbrain", mustWork = TRUE);
rh_demo_cluster_file <- system.file("extdata", "rh.clusters_fsaverage.mgz", package = "fsbrain", mustWork = TRUE);
lh_clust <- freesurferformats::read.fs.morph(lh_demo_cluster_file);   # a single positive cluster (activation), the other values are 0
rh_clust <- freesurferformats::read.fs.morph(rh_demo_cluster_file);   # two negative clusters
cm <- vis.symmetric.data.on.subject(subjects_dir, subject_id, lh_clust, rh_clust, bg = "curv_light", views = NULL);
render(9, "vertex_clusters", cm, colorbar_legend = 't-value (simulated data)');

# --- Part 3: A workflow with manually loaded meshes and data -------------------
cat("\n[Part 3] Manually loaded meshes and data\n");
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
render(10, "manual_thickness_t4", list("lh" = cm_lh, "rh" = cm_rh), colorbar_legend = 'Cortical thickness [mm]');

# --- Done ----------------------------------------------------------------------
cat(sprintf("\nDone. Output files in '%s':\n", OUTPUT_DIR));
print(list.files(OUTPUT_DIR));
