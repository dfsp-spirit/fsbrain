#!/usr/bin/env Rscript
#
# facecheck.R -- Create images of MRI volumes to see whether defacing (anonymization) worked.
# This is an example script that comes with 'fsbrain': https://github.com/dfsp-spirit/fsbrain
#
# Dependencies:
#   Requires the 'fsbrain' package to be installed.
#
#   NOTE: Currently, you need the latest development version of 'fsbrain' for this to work. To install it, run in your R session:
#
#            install.packages(c("devtools", "knitr", "rmarkdown", "testthat"));
#            devtools::install_github("dfsp-spirit/fsbrain", build_vignettes=TRUE);
#
# USAGE: ./facecheck.R <subjects_dir> <subject_id>
#
# Written by Tim Schaefer


library("fsbrain");
library("misc3d");
library("magick");

args = commandArgs(trailingOnly=TRUE);


generate_facecheck_image <- function(subjects_dir, subject_id, output_img=NULL, silent=FALSE) {
    volumes_rel = c("mri/orig.mgz", "mri/orig_nu.mgz", "mri/T1.mgz", "mri/rawavg.mgz", "mri/orig/001.mgz");
    volumes = paste(file.path(subjects_dir, subject_id), .Platform$file.sep, volumes_rel, sep="");
    output_single_images = c();
    for(vol_file in volumes) {
        if(!silent) {
            cat(sprintf("Handling volume file '%s'.\n", vol_file));
        }
        if(file.exists(vol_file)) {
            vol_data = freesurferformats::read.fs.mgh(vol_file, drop=TRUE);
            rglactions = list("snapshot_png"=tempfile(fileext=".png"));
            surface_tris = fsbrain::volvis.countour(vol_data, rglactions=rglactions);
            brainviews("t9", list(surface_tris));
            #fsbrain::vislayout.from.coloredmeshes(list(surface_tris), view_angles = "si");
            # todo: support rendering  of Triangles3D (and fs.coloredvoxels) instances by adapting vis.rotated.coloredmeshes and sort.coloredmeshes.by.hemi
        }

    }
    if(is.null(output_img)) {
        output_img = sprintf("facecheck_subject_%s.png", subject_id);
    }
    fsbrain::arrange.brainview.images(output_single_images, output_img, silent=silent, grid_like = FALSE);
}


if (length(args) != 2) {
    stop("USAGE: ./facecheck.R <subjects_dir> <subject_id>");
} else {
    generate_facecheck_image(args[1], args[2]);
}

