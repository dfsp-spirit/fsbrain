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

quietly = TRUE;
library("fsbrain", quietly = quietly);
library("misc3d", quietly = quietly);
library("magick", quietly = quietly);

args = commandArgs(trailingOnly=TRUE);


generate_facecheck_image <- function(subjects_dir, subject_id, output_img=NULL, silent=TRUE, delete_tmp_images=TRUE) {
    volumes_rel = c("mri/orig.mgz", "mri/orig_nu.mgz", "mri/T1.mgz", "mri/rawavg.mgz", "mri/orig/001.mgz");
    volumes = paste(file.path(subjects_dir, subject_id), .Platform$file.sep, volumes_rel, sep="");

    output_single_images = c();
    sub_img_idx = 1L;

    for(vol_file in volumes) {

        img_name = sprintf("facecheck_tmp_%d_subject_%s.png", sub_img_idx, subject_id);
        if(!silent) {
            cat(sprintf("Handling volume file '%s'.\n", vol_file));
        }

        if(file.exists(vol_file)) {
            vol_data = freesurferformats::read.fs.mgh(vol_file);
            surface_tris = fsbrain::volvis.contour(vol_data, level=80, show=FALSE);
            fsbrain::vislayout.from.coloredmeshes(surface_tris, view_angles = "sd_caudal", output_img = img_name, silent = silent);
            output_single_images = c(output_single_images, img_name);
        }
        sub_img_idx = sub_img_idx + 1L;
    }

    if(is.null(output_img)) {
        output_img = sprintf("facecheck_subject_%s.png", subject_id);
    }

    if(length(output_single_images) >= 1) {
        if(!silent) {
            cat(sprintf("Combined views of %d volumes into image '%s'.\n", length(output_single_images), output_img));
        }
        fsbrain::arrange.brainview.images(output_single_images, output_img, silent=silent, grid_like = FALSE);
        if(delete_tmp_images) {
            del_result = file.remove(output_single_images);
        }
    } else {
        warning(sprintf("No volume files found, nothing to visualize.\n"));
    }


}


if (length(args) != 2) {
    stop("USAGE: ./facecheck.R <subjects_dir> <subject_id>");
} else {
    generate_facecheck_image(args[1], args[2]);
}

