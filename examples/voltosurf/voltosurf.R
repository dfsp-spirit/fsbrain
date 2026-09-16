#!/usr/bin/env Rscript
#
# voltosurf.R -- Project volume data onto a cortical surface (volume to surface projection).
#
# This is an example script that comes with 'fsbrain': https://github.com/dfsp-spirit/fsbrain
#
# The script demonstrates the two projection functions of fsbrain:
#
#   1) 'subject.vol2surf()' projects a volume onto the surface of a *subject*, using the
#      native space of that subject. It is the R equivalent of the FreeSurfer command
#      'mri_vol2surf --projfrac 0' (and of 'project_vol2surf()' from the Python package
#      'yabplot'): the position of every surface vertex is read from the surface file, and the
#      volume is sampled at that position (trilinear interpolation by default). Volume and
#      surface must be defined in the same coordinate space, which is the case for the volumes
#      in the 'mri' directory (e.g. 'brain.mgz') and the surfaces in the 'surf' directory
#      (e.g. 'lh.white') of the same subject. No registration is involved.
#
#   2) 'template.vol2surf()' projects a volume onto a *template* surface, e.g. the HCP-style
#      fs_LR 32k template or the FreeSurfer 'fsaverage' template. This is the variant you want
#      for group-level statistical maps: the map is in a standard space (e.g. MNI152 for the
#      fs_LR 32k template, MNI305 for fsaverage) and the template surfaces are defined in the
#      same space. As an example, the script creates a synthetic group-level "stat map" in
#      MNI152 space (a few blobs, written as an MGZ file -- a NIfTI file from FSL, SPM or AFNI
#      works exactly the same way, only the affine matrix from the file header is used) and
#      projects it onto the fs_LR 32k midthickness surface.
#
# Note that the medial wall is masked by default in part 2 ('cortex_only = TRUE'): the medial
# wall vertices are not part of the cortex and would otherwise show signal from the other
# hemisphere or from subcortical structures.
#
# Dependencies:
#   Requires the 'fsbrain' package to be installed, and the fs_LR 32k template files
#   (downloaded into the package cache by this script, if needed).
#
# USAGE: ./voltosurf.R <subjects_dir> [<output_dir>] [--renderer <rgl|scimesh>] [--threshold <value>]
#
# OPTIONS:
#   --renderer <backend> : the renderer backend to use for image export, either 'rgl'
#                          (default, needs a working display) or 'scimesh' (headless
#                          software renderer that writes PNG images; requires the
#                          scimesh package).
#   --threshold <value>  : the threshold for the synthetic stat map in part 2 (default: 4.0).
#                          Values below the threshold are not shown, which is what one does
#                          with real statistical maps as well.
#
# Written by Tim Schaefer

library("fsbrain");
args = commandArgs(trailingOnly = TRUE);


# The MNI152 template space: the 2 mm grid and the affine matrix that maps 0-based voxel
# indices (column, row, slice) to world coordinates (in mm), as used by FSL's MNI152 templates.
mni152.dims = c(91L, 109L, 91L);
mni152.affine = matrix(c(-2.0, 0.0, 0.0, 0.0,
                          0.0, 2.0, 0.0, 0.0,
                          0.0, 0.0, 2.0, 0.0,
                          90.0, -126.0, -72.0, 1.0), nrow = 4L);

# The blobs of the synthetic stat map: the MNI152 coordinates of the blob centers, the peak
# value and the width (standard deviation in mm) of each blob. These are just some plausible
# cortical positions, the values are arbitrary.
demo.stat.map.blobs = list(
    list("center" = c(-38.0, -14.0, 58.0), "peak" = 8.0, "sd" = 9.0),
    list("center" = c(-46.0, 32.0, 8.0), "peak" = 6.0, "sd" = 9.0),
    list("center" = c(-20.0, -92.0, 8.0), "peak" = 7.0, "sd" = 9.0),
    list("center" = c(54.0, -34.0, 8.0), "peak" = -6.5, "sd" = 9.0)
);


# Create a synthetic "stat map" in MNI152 space and write it as an MGZ file into the given
# directory. Returns the filepath of the written volume.
create.demo.stat.map.mni152 <- function(output_dir) {
    # The world (MNI152) coordinates of all voxel centers along each axis.
    world_x = mni152.affine[1, 1] * 0:(mni152.dims[1] - 1L) + mni152.affine[1, 4];
    world_y = mni152.affine[2, 2] * 0:(mni152.dims[2] - 1L) + mni152.affine[2, 4];
    world_z = mni152.affine[3, 3] * 0:(mni152.dims[3] - 1L) + mni152.affine[3, 4];

    volume = array(0.0, dim = mni152.dims);
    for(blob in demo.stat.map.blobs) {
        # Squared distance from the blob center, as a 3D array (vectorized, no loops over voxels).
        sq_x = (world_x - blob$center[1])^2;
        sq_y = (world_y - blob$center[2])^2;
        sq_z = (world_z - blob$center[3])^2;
        sq_xy = outer(sq_x, sq_y, "+");
        sq_dist = array(rep(sq_xy, mni152.dims[3]), dim = mni152.dims) +
                  array(rep(sq_z, each = mni152.dims[1] * mni152.dims[2]), dim = mni152.dims);
        volume = volume + blob$peak * exp(-sq_dist / (2.0 * blob$sd^2));
    }
    filepath = file.path(output_dir, "demo_stat_map_mni152.mgz");
    freesurferformats::write.fs.mgh(filepath, volume, vox2ras_matrix = mni152.affine);
    return(filepath);
}


# Part 1: project the brain volume of a subject onto its own surfaces (native space).
project.subject.example <- function(subjects_dir, subject_id, output_dir = ".",
    views = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"), silent = FALSE) {

    if(! silent) {
        cat(sprintf("Projecting the 'brain' volume of subject '%s' onto its own 'white' surface.\n", subject_id));
    }

    # The white surface of the subject. The volume ('mri/brain.mgz') and this surface are both
    # in the native space of the subject, so the volume values can be sampled at the vertex
    # positions directly. The transformation from voxel indices to the surface space is derived
    # from the volume (see the 'vox2ras' parameter): for MGH/MGZ volumes the FreeSurfer
    # 'tkregister' convention is used, which is the space in which FreeSurfer defines its
    # surfaces. This matters for volumes whose header has a non-zero center-of-RAS offset, as
    # the one used here (check with 'mri_info --cras <volume>', which reports a non-zero offset
    # for this subject). The projection matches the FreeSurfer command
    # 'mri_vol2surf --projfrac 0 --interp nearest' exactly (see the fsbrain unit tests).
    morph_white = fsbrain::subject.vol2surf(subjects_dir, subject_id, volume = "brain",
        surface = "white", hemi = "both", cortex_only = FALSE);

    # The mid-cortical surface: instead of the white surface, the volume is sampled at the
    # positions halfway between the white and the pial surface (this is what the FreeSurfer
    # option '--surf-frac 0.5' does, and it is the surface that 'midthickness' meshes contain).
    morph_midcortex = fsbrain::subject.vol2surf(subjects_dir, subject_id, volume = "brain",
        surface = "white", frac_surface = "pial", surface_frac = 0.5, hemi = "both");

    # For the visualization, the (hidden) medial wall is masked, and the data are rendered on
    # the inflated surface so that the sulci are visible. The colorbar range is derived from
    # the data, as the intensities of the volume are not known in advance.
    output_images = character(0);
    for(spec in list(list("data" = morph_white, "img" = "voltosurf_subject_white.png", "legend" = "brain volume at white surface"),
                     list("data" = morph_midcortex, "img" = "voltosurf_subject_midcortex.png", "legend" = "brain volume at mid-cortical surface (white-pial frac 0.5)"))) {
        masked = fsbrain::apply.label.to.morphdata(spec$data$lh, subjects_dir, subject_id, "lh", label = "cortex.label");
        masked_rh = fsbrain::apply.label.to.morphdata(spec$data$rh, subjects_dir, subject_id, "rh", label = "cortex.label");
        cm = fsbrain::vis.data.on.subject(subjects_dir, subject_id,
            morph_data_lh = masked, morph_data_rh = masked_rh, surface = "inflated",
            makecmap_options = fsbrain::mkco.seq(),
            rglactions = list('no_vis' = TRUE));
        output_img = file.path(output_dir, spec$img);
        fsbrain::export(cm, view_angles = views, colorbar_legend = spec$legend,
            output_img = output_img, silent = silent);
        if(! silent) {
            cat(sprintf("Wrote image '%s'.\n", output_img));
        }
        output_images = c(output_images, output_img);
    }

    return(invisible(output_images));
}


# Part 2: project a group-level (MNI152) stat map onto the HCP-style fs_LR 32k template.
project.fslr32.example <- function(subjects_dir, output_dir = ".", threshold = 4.0,
    views = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"), silent = FALSE) {

    # Make sure the fs_LR 32k template files are available (these calls do nothing if the files
    # are already in the package cache).
    fsbrain::download_fs_LR_32_meshes();
    fsbrain::download_fs_LR_32_labels();

    stat_map_file = create.demo.stat.map.mni152(output_dir);
    if(! silent) {
        cat(sprintf("Created the synthetic MNI152 stat map '%s' (4 blobs).\n", stat_map_file));
        cat(sprintf("Projecting it onto the fs_LR_32 midthickness surface (threshold: %.1f, medial wall masked).\n", threshold));
    }

    # The projection. 'cortex_only = TRUE' sets all vertices which are not part of the cortex
    # (as defined by 'label/?h.cortex.label' of the template) to NA, which removes the medial
    # wall. Vertices outside the volume would be set to NA as well (and a warning would be
    # printed), which does not happen here because the stat map covers the whole brain.
    stat = fsbrain::template.vol2surf(stat_map_file, template = "fs_LR_32", surface = "midthickness",
        hemi = "both", subjects_dir = subjects_dir, cortex_only = TRUE);

    # Statistical maps are typically thresholded for display. We use the absolute value of the
    # map here, so that the negative blob in the right hemisphere is shown as well, and render
    # the result with a diverging colormap. Values below the threshold are set to NA, which
    # makes them invisible (the value NA is not rendered at all).
    stat_thresholded = stat;
    stat_thresholded$lh[abs(stat$lh) < threshold] = NA_real_;
    stat_thresholded$rh[abs(stat$rh) < threshold] = NA_real_;
    if(! silent) {
        cat(sprintf("Vertices above threshold: lh=%d, rh=%d. Medial wall vertices: lh=%d, rh=%d.\n",
            sum(!is.na(stat_thresholded$lh)), sum(!is.na(stat_thresholded$rh)),
            sum(is.na(stat$lh)), sum(is.na(stat$rh))));
    }

    cm = fsbrain::vis.data.on.subject(subjects_dir, "fs_LR_32",
        morph_data_lh = stat_thresholded$lh, morph_data_rh = stat_thresholded$rh, surface = "midthickness",
        makecmap_options = fsbrain::mkco.div(),
        rglactions = list('no_vis' = TRUE));
    output_img = file.path(output_dir, "voltosurf_fsLR32_statmap.png");
    fsbrain::export(cm, view_angles = views, colorbar_legend = "t value",
        output_img = output_img, silent = silent);
    if(! silent) {
        cat(sprintf("Wrote image '%s'.\n", output_img));
    }

    return(invisible(list("image" = output_img, "morph_data" = stat, "stat_map_file" = stat_map_file)));
}


# --- Command line handling ----------------------------------------------------

parse_args <- function(args) {
    settings = list("subjects_dir" = NULL, "output_dir" = ".", "renderer" = "rgl", "threshold" = 4.0);
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
        } else if(arg == "--threshold") {
            if(idx + 1 > length(args)) {
                stop("Option '--threshold' requires an argument.");
            }
            settings$threshold = as.numeric(args[idx + 1]);
            idx = idx + 2;
        } else {
            positional = c(positional, arg);
            idx = idx + 1;
        }
    }

    if(length(positional) < 1 || length(positional) > 2) {
        stop("USAGE: ./voltosurf.R <subjects_dir> [<output_dir>] [--renderer <rgl|scimesh>] [--threshold <value>]");
    }
    settings$subjects_dir = positional[1];
    if(length(positional) == 2) {
        settings$output_dir = positional[2];
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

    # Part 1: a subject, in its own native space (uses the 'brain' volume and the white/pial
    # surfaces of the subject).
    project.subject.example(settings$subjects_dir, "subject1", settings$output_dir);

    # Part 2: a group-level map in MNI152 space, projected onto the fs_LR 32k template. Note
    # that the fs_LR 32k template files are downloaded into the package cache here, and that
    # the synthetic stat map is written into the output directory.
    project.fslr32.example(settings$subjects_dir, settings$output_dir, threshold = settings$threshold);

    cat("Done.\n");
}


main(args);
