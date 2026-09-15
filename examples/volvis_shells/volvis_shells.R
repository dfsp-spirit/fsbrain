#!/usr/bin/env Rscript
#
# volvis_shells.R
# ===============
#
# Demo for the `volvis.shells()` function: visualizes a volume as a set of nested,
# semi-transparent iso-surface shells. The demo subject 'subject1' is downloaded with the
# optional data, and its T1 volume is rendered in the 4 standard views, once as-is and once
# cut open from the right so that one can look inside the brain.
#
# The images are written into the current working directory (the run.sh script of this example
# changes into the directory of this file before running it).
#
# Usage (from anywhere):
#   Rscript examples/volvis_shells/volvis_shells.R
#
# Optional arguments:
#   --levels N    the number of shells (default: 4)
#   --downsample N  subsampling factor for the volume, reduces the number of triangles (default: 2)
#
# Requirements: the current fsbrain development version (with `volvis.shells`), the
# `Rvcg` package (optional, but recommended: it extracts the shells much faster), and either
# the `rgl` or the `scimesh` renderer backend for the `export` calls.

args = commandArgs(trailingOnly = TRUE);

get.arg.value = function(name, default) {
    idx = which(args == name);
    if(length(idx) == 0L) {
        return(default);
    }
    if(idx == length(args)) {
        stop(sprintf("Missing value for argument '%s'.\n", name));
    }
    return(args[idx + 1L]);
}

num_levels = as.integer(get.arg.value("--levels", 4L));
downsample = as.integer(get.arg.value("--downsample", 2L));

subjects_dir = Sys.getenv("FSBRAIN_DEMO_SUBJECTS_DIR", unset = "");
if(nchar(subjects_dir) == 0L) {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
}

cat(sprintf("Using subjects_dir '%s'.\n", subjects_dir));
brain = fsbrain::subject.volume(subjects_dir, "subject1", "brain");

views = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh");

# 1) The volume as nested shells, in the 4 standard views. The default palette ('grey_context')
#    renders the context shells in grey and the innermost (most interesting) one in a warm color.
cat(sprintf("Extracting %d nested shells ...\n", num_levels));
shells = fsbrain::volvis.shells(brain, num_levels = num_levels, downsample = downsample,
    views = NULL, silent = TRUE);
fsbrain::export(shells, view_angles = views, output_img = "shells_views.png", silent = TRUE);
cat("Wrote 'shells_views.png'.\n");

# 2) The same shells, cut open from the right, so the inner shells are visible from the side.
shells_cut = fsbrain::volvis.shells(brain, num_levels = num_levels, downsample = downsample,
    cut_away = "right", views = NULL, silent = TRUE);
fsbrain::export(shells_cut, view_angles = views, output_img = "shells_cut_views.png", silent = TRUE);
cat("Wrote 'shells_cut_views.png'.\n");

# 3) The same scene with an alternative palette: 'sequential' uses a single hue, getting lighter
#    and more opaque towards the inside, which emphasizes the nested structure. Use 'viridis' for
#    the (colorful) ramp that encodes the iso-level in the color of the shell.
shells_seq = fsbrain::volvis.shells(brain, num_levels = num_levels, downsample = downsample,
    cut_away = "right", palette = "sequential", views = NULL, silent = TRUE);
fsbrain::export(shells_seq, view_angles = views, output_img = "shells_sequential_views.png", silent = TRUE);
cat("Wrote 'shells_sequential_views.png'.\n");
