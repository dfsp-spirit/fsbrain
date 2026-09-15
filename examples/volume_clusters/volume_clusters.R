#!/usr/bin/env Rscript
#
# volume_clusters.R -- Visualize the clusters of a volume inside a translucent cortex.
#
# This is an example script that comes with 'fsbrain': https://github.com/dfsp-spirit/fsbrain
#
# The function 'vis.volume.clusters' renders the supra-threshold regions of a volume (e.g., the
# clusters of a statistical map) as nested, smoothly shaded iso-surfaces inside a single,
# semi-transparent anatomical mesh. The outermost shell of a cluster is the iso-surface at the
# threshold (the cluster boundary), the innermost one is at the cluster peak, and the shells get more
# opaque towards the peak. This makes both the extent of a cluster and the location of its maximum
# visible, without the voxel staircases that a voxel-based rendering shows.
#
# This demo does not need a statistical map: it computes a synthetic one (three Gaussian blobs,
# two positive and one negative) in the grid of a FreeSurfer *conformed* volume (1 mm voxels,
# 256 x 256 x 256), which is the space of the fsaverage template subject. To use your own results,
# load your map with 'freesurferformats::read.fs.volume()' (it supports NIfTI and MGH/MGZ), make sure
# it is in the space of the subject you render the context mesh of, and pass it as the 'volume'
# parameter.
#
# Dependencies:
#   Requires the 'fsbrain' package, the 'Rvcg' package (to extract the iso-surfaces), and the
#   surfaces of the fsaverage template subject (downloaded into the package cache on demand).
#
# USAGE: ./volume_clusters.R <subjects_dir> [<output_dir>] [--renderer <rgl|scimesh>] [--downsample <n>]
#
# OPTIONS:
#   --renderer <backend> : the renderer backend to use for image export, either 'rgl'
#                          (default, needs a working display) or 'scimesh' (headless
#                          software renderer that writes PNG images; requires the
#                          scimesh package).
#   --downsample <n>     : the subsampling factor for the volume (default: 2). Higher values
#                          are faster, the default of the function is 1.
#
# Written by Tim Schaefer

library("fsbrain");
args = commandArgs(trailingOnly = TRUE);


# The template subject whose surfaces are used as the context mesh. The surfaces are part of
# FreeSurfer and can be downloaded with 'fsbrain::download_fsaverage()'.
template_subject = "fsaverage";

# The threshold and the number of nested shells per cluster to use in the demo.
cluster_threshold = 3.0;
num_shells = 4L;

# The size of the conformed volume used in this demo (256^3 with 1 mm voxels).
volume_dim = 256L;


# Compute a synthetic statistical map: add a Gaussian blob with the given peak value around the given
# surface RAS position. Returns the modified volume.
add.blob <- function(volume, ras, sigma_mm = 8.0, peak = 6.0) {
    crs0 = (ras2vox_tkr() %*% c(ras, 1.0))[1:3];          # 0-based voxel index of the blob center
    axis_coords = lapply(dim(volume), function(n) { seq_len(n) - 1.0; });
    radius = ceiling(3.0 * sigma_mm);
    for(x in which(abs(axis_coords[[1L]] - crs0[1L]) <= radius)) {
        for(y in which(abs(axis_coords[[2L]] - crs0[2L]) <= radius)) {
            for(z in which(abs(axis_coords[[3L]] - crs0[3L]) <= radius)) {
                dist_sq = (x - crs0[1L])^2 + (y - crs0[2L])^2 + (z - crs0[3L])^2;
                volume[x, y, z] = volume[x, y, z] + peak * exp(-dist_sq / (2.0 * sigma_mm^2));
            }
        }
    }
    return(volume);
}


# Compute the synthetic map: one large positive blob deep in the left hemisphere, one smaller
# positive blob near the right frontal surface, and one negative blob in the left hemisphere.
get.synthetic.map <- function(dim = volume_dim) {
    stat = array(0.0, dim = c(dim, dim, dim));
    stat = add.blob(stat, c(-30.0, -25.0, 10.0), sigma_mm = 9.0, peak = 6.5);
    stat = add.blob(stat, c(35.0, 20.0, 30.0), sigma_mm = 7.0, peak = 4.5);
    stat = add.blob(stat, c(-25.0, 15.0, 40.0), sigma_mm = 6.0, peak = -5.5);
    return(stat);
}


# Visualize the clusters in the 4 standard views, with and without a context mesh, and once with a
# single shell per cluster. Returns the list of the written image files.
vis.volume.clusters.example <- function(subjects_dir, output_dir = ".", downsample = 2L, silent = FALSE) {

    stat = get.synthetic.map();
    views = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh");
    written = character(0);

    # 1) The default style: one set of nested shells per cluster, inside a semi-transparent cortex.
    #    The shells are colored with the 'cluster' colormap, which encodes the iso-level, i.e., the
    #    colors match the colorbar.
    output_img = file.path(output_dir, "volume_clusters_views.png");
    cm = fsbrain::vis.volume.clusters(subjects_dir, template_subject, stat,
        threshold = cluster_threshold, num_levels = num_shells, downsample = downsample,
        rglactions = list("no_vis" = TRUE), silent = silent);
    fsbrain::export(cm, view_angles = views, colorbar_legend = "statistic",
        output_img = output_img, silent = silent);
    written = c(written, output_img);
    if(! silent) {
        cat(sprintf("Wrote image '%s'.\n", output_img));
    }

    # 2) A single, opaque shell per cluster: only the cluster boundaries are rendered, which is the
    #    cleanest option if the extent of the clusters is all that matters.
    output_img_single = file.path(output_dir, "volume_clusters_single_shell.png");
    cm_single = fsbrain::vis.volume.clusters(subjects_dir, template_subject, stat,
        threshold = cluster_threshold, num_levels = 1L, downsample = downsample,
        rgloptions = rglo(), rglactions = list("no_vis" = TRUE), silent = silent);
    fsbrain::export(cm_single, view_angles = views, colorbar_legend = "statistic",
        output_img = output_img_single, silent = silent);
    written = c(written, output_img_single);
    if(! silent) {
        cat(sprintf("Wrote image '%s'.\n", output_img_single));
    }

    # 3) The same clusters without any context mesh. Note that the colorbar is drawn for the cluster
    #    values, the context mesh does not contribute to it.
    output_img_nocontext = file.path(output_dir, "volume_clusters_no_context.png");
    cm_nocontext = fsbrain::vis.volume.clusters(subjects_dir, template_subject, stat,
        threshold = cluster_threshold, num_levels = num_shells, downsample = downsample,
        context = NULL, rglactions = list("no_vis" = TRUE), silent = silent);
    fsbrain::export(cm_nocontext, view_angles = views, colorbar_legend = "statistic",
        output_img = output_img_nocontext, silent = silent);
    written = c(written, output_img_nocontext);
    if(! silent) {
        cat(sprintf("Wrote image '%s'.\n", output_img_nocontext));
    }

    return(invisible(written));
}


# --- Command line handling ----------------------------------------------------

parse_args <- function(args) {
    settings = list("subjects_dir" = NULL, "output_dir" = ".", "renderer" = "rgl", "downsample" = 2L);
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
        } else if(arg == "--downsample") {
            if(idx + 1 > length(args)) {
                stop("Option '--downsample' requires an argument (a positive integer).");
            }
            settings$downsample = as.integer(args[idx + 1]);
            if(is.na(settings$downsample) || settings$downsample < 1L) {
                stop("Option '--downsample' requires a positive integer.");
            }
            idx = idx + 2;
        } else {
            positional = c(positional, arg);
            idx = idx + 1;
        }
    }

    if(length(positional) < 1L || length(positional) > 2L) {
        stop("USAGE: ./volume_clusters.R <subjects_dir> [<output_dir>] [--renderer <rgl|scimesh>] [--downsample <n>]");
    }
    settings$subjects_dir = positional[1L];
    if(length(positional) == 2L) {
        settings$output_dir = positional[2L];
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
    if(!requireNamespace("Rvcg", quietly = TRUE)) {
        stop("This example requires the 'Rvcg' package to extract the cluster iso-surfaces.");
    }

    if(! dir.exists(settings$output_dir)) {
        dir.create(settings$output_dir, recursive = TRUE);
    }

    vis.volume.clusters.example(settings$subjects_dir, settings$output_dir, downsample = settings$downsample);
}


main(args);
