#!/usr/bin/env Rscript
#
# subcortical.R -- Visualize values on the subcortical atlas of a subject.
#
# This is an example script that comes with 'fsbrain': https://github.com/dfsp-spirit/fsbrain
#
# The subcortical atlas contains 8 subcortical structures per hemisphere (accumbens area,
# amygdala, caudate, hippocampus, pallidum, putamen, thalamus and lateral ventricle). It is
# derived from the ENIGMA subcortical meshes, see 'dev_tools/subcortial/enigma_aseg/'. In
# contrast to the standard FreeSurfer atlases, it is not bound to the cortical surface: the
# atlas ships its own surface mesh (the structures) and the corresponding annotation file.
#
# The atlas files for the fsaverage template subject are not part of FreeSurfer, but they can
# be downloaded into the package cache with 'fsbrain::download_optional_data()' (or with
# 'fsbrain::download_fsaverage_atlases()'), see the 'run.sh' script of this example. In
# combination with the cortical surfaces of fsaverage, which can be downloaded with
# 'fsbrain::download_fsaverage(accept_freesurfer_license = TRUE)', this allows you to render
# the structures on their own and inside a semi-transparent cortex of the same subject.
#
# Dependencies:
#   Requires the 'fsbrain' package to be installed.
#
# USAGE: ./subcortical.R <subjects_dir> <subject> [<output_dir>] [--renderer <rgl|scimesh>]
#
# OPTIONS:
#   --renderer <backend> : the renderer backend to use for image export, either 'rgl'
#                          (default, needs a working display) or 'scimesh' (headless
#                          software renderer that writes PNG images; requires the
#                          scimesh package).
#
# Written by Tim Schaefer

library("fsbrain");
args = commandArgs(trailingOnly=TRUE);


# The regions of the subcortical atlas, in the order in which they occur in the annotation files.
subcortical_atlas_regions = list(
    "lh" = c("Left-Accumbens-area", "Left-Amygdala", "Left-Caudate", "Left-Hippocampus",
             "Left-Pallidum", "Left-Putamen", "Left-Thalamus-Proper", "Left-Lateral-Ventricle"),
    "rh" = c("Right-Accumbens-area", "Right-Amygdala", "Right-Caudate", "Right-Hippocampus",
             "Right-Pallidum", "Right-Putamen", "Right-Thalamus-Proper", "Right-Lateral-Ventricle")
);


# Generate one random value in the range [0, 1] per region. The seed is fixed, so the example
# produces the same image every time it is run. Returns a named list of values, as required by
# 'vis.region.values.on.subject'.
get.region.values <- function(region_names, seed) {
    set.seed(seed);
    region_values = runif(length(region_names), min = 0.0, max = 1.0);
    names(region_values) = region_names;
    return(as.list(region_values));
}


# Compute the values for all regions of both hemispheres of the subcortical atlas. Two different
# seeds are used for the two hemispheres, so that the left and right hemisphere values differ.
get.subcortical.region.value.lists <- function(seed_lh = 42L, seed_rh = 43L) {
    return(list("lh" = get.region.values(subcortical_atlas_regions$lh, seed_lh),
                "rh" = get.region.values(subcortical_atlas_regions$rh, seed_rh)));
}


# Visualize one random value per region of the subcortical atlas and save the resulting images
# into 'output_dir'. Two images are written: one showing the structures on their own, and one
# showing them inside a semi-transparent cortex of the same subject.
vis.subcortical.example <- function(subjects_dir, subject_id, output_dir = ".",
    views = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"), silent = FALSE) {

    region_value_lists = get.subcortical.region.value.lists();

    if(! silent) {
        cat(sprintf("Visualizing %d random region values (%d regions per hemisphere) on the subcortical atlas of subject '%s'.\n",
            length(region_value_lists$lh) + length(region_value_lists$rh), length(region_value_lists$lh), subject_id));
    }

    # The structures on their own. Note that the atlas mesh is not a cortical surface, so the
    # name of the surface that belongs to the atlas ('subcortical') has to be passed explicitly.
    output_img = file.path(output_dir, "subcortical_region_values.png");
    cm = fsbrain::vis.subcortical.region.values(subjects_dir, subject_id,
        lh_region_value_list = region_value_lists$lh, rh_region_value_list = region_value_lists$rh,
        surface = 'subcortical', makecmap_options = fsbrain::mkco.seq(),
        rglactions = list('no_vis' = TRUE), silent = silent);
    fsbrain::export(cm, view_angles = views, colorbar_legend = "random value",
        output_img = output_img, silent = silent);
    if(! silent) {
        cat(sprintf("Wrote image '%s'.\n", output_img));
    }

    # The same values, but with the structures rendered inside a semi-transparent cortex. The
    # context mesh has to be defined in the same space as the atlas mesh, so it must come from
    # the same subject (fsaverage here, which provides the 'white' surface).
    output_img_ctx = file.path(output_dir, "subcortical_region_values_in_cortex.png");
    cm_ctx = fsbrain::vis.subcortical.region.values(subjects_dir, subject_id,
        lh_region_value_list = region_value_lists$lh, rh_region_value_list = region_value_lists$rh,
        surface = 'subcortical', cortex = list('surface' = 'white', 'color' = '#B0B0B0', 'alpha' = 0.15),
        makecmap_options = fsbrain::mkco.seq(),
        rglactions = list('no_vis' = TRUE), silent = silent);
    fsbrain::export(cm_ctx, view_angles = views, colorbar_legend = "random value",
        output_img = output_img_ctx, silent = silent);
    if(! silent) {
        cat(sprintf("Wrote image '%s'.\n", output_img_ctx));
    }

    return(invisible(list('no_context' = cm, 'with_context' = cm_ctx)));
}


# --- Command line handling ----------------------------------------------------

parse_args <- function(args) {
    settings = list("subjects_dir" = NULL, "subject_id" = NULL,
                    "output_dir" = ".", "renderer" = "rgl");
    positional = character(0);
    idx = 1L;
    while(idx <= length(args)) {
        arg = args[idx];
        if(arg == "--renderer") {
            if(idx + 1 > length(args)) {
                stop("Option '--renderer' requires an argument ('rgl' or 'scimesh').");
            }
            settings$renderer = args[idx + 1];
            if(!(settings$renderer %in% c("rgl", "scimesh"))) {
                stop(sprintf("Invalid renderer '%s'. Must be one of 'rgl' or 'scimesh'.", settings$renderer));
            }
            idx = idx + 2;
        } else {
            positional = c(positional, arg);
            idx = idx + 1;
        }
    }

    if(length(positional) < 2 || length(positional) > 3) {
        stop("USAGE: ./subcortical.R <subjects_dir> <subject> [<output_dir>] [--renderer <rgl|scimesh>]");
    }
    settings$subjects_dir = positional[1];
    settings$subject_id = positional[2];
    if(length(positional) == 3) {
        settings$output_dir = positional[3];
    }
    return(settings);
}


main <- function(args) {
    settings = parse_args(args);

    if(settings$renderer == "scimesh") {
        if(!requireNamespace("scimesh", quietly = TRUE)) {
            stop("Renderer backend 'scimesh' selected, but the 'scimesh' package is not installed.");
        }
        options(fsbrain.renderer_backend = "scimesh");
        options(fsbrain.scimesh.output_dims = c(2560, 1440));
    }

    if(! dir.exists(settings$output_dir)) {
        dir.create(settings$output_dir, recursive = TRUE);
    }

    vis.subcortical.example(settings$subjects_dir, settings$subject_id, settings$output_dir);
}


main(args);
