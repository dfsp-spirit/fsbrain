#!/usr/bin/env Rscript
#
# facecheck.R -- Create images of MRI volumes to see whether defacing (anonymization) worked.
# This is an example script that comes with 'fsbrain': https://github.com/dfsp-spirit/fsbrain
#
# Dependencies:
#   Requires the 'fsbrain' package to be installed.
#
#
# USAGE: ./facecheck.R <subjects_dir> <subject_id>
#
# EXAMPLE if you have FreeSurfer installed:
#   ./facecheck.R $SUBJECTS_DIR bert
#
# Written by Tim Schaefer

quietly = TRUE;
library("fsbrain", quietly = quietly);
library("misc3d", quietly = quietly);
library("magick", quietly = quietly);

# Allow forcing the renderer backend via the environment (used by the example
# runner to select the headless scimesh backend, see examples/run_all_examples.sh).
backend_env = Sys.getenv("FSBRAIN_RENDERER_BACKEND", unset = NA);
if(!is.na(backend_env)) {
    options(fsbrain.renderer_backend = backend_env);
}

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
            mgh = freesurferformats::read.fs.mgh(vol_file, with_header = TRUE);
            surface_tris = fsbrain::volvis.contour(mgh, level=90, show=FALSE);
            surface_tris = fsbrain::apply.transform(surface_tris, freesurferformats::mghheader.vox2ras(mgh)); # re-orient to standard rendering orientation
            rglactions = list("text"=list("x"=-160, y=-130, "texts"=basename(vol_file))); # Draw volume label onto the 3D image.
            fsbrain::vislayout.from.coloredmeshes(surface_tris, view_angles = "sd_caudal", output_img = img_name, silent = silent, rglactions = rglactions);
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


if (length(args) == 2) {
    generate_facecheck_image(args[1], args[2]);
} else if (length(args) == 3) {
    generate_facecheck_image(args[1], args[2], args[3]);
} else {
    stop("USAGE: ./facecheck.R <subjects_dir> <subject_id> [<output_img>]");
}
