#!/usr/bin/env Rscript
#
# medial_mask.R -- Generate and save medial mask for a single FreeSurfer subject.
# This is an example script that comes with 'fsbrain': https://github.com/dfsp-spirit/fsbrain
#
# Dependencies:
#   Requires the 'fsbrain' package to be installed.
#
#
# USAGE: ./medial_mask.R <subjects_dir> <subject>
# EXAMPLE if you have FreeSurfer installed:
#   ./medial_mask.R $SUBJECTS_DIR bert
# Written by Tim Schaefer


library("fsbrain");
args = commandArgs(trailingOnly=TRUE);


save_medial_mask <- function(subjects_dir, subject_id, outfile_lh="lh_mask.mgz", outfile_rh="rh_mask.mgz", silent=FALSE) {
  mask = fsbrain::subject.mask(subjects_dir, subject_id);
  freesurferformats::write.fs.mgh(outfile_lh, as.integer(mask$lh));
  freesurferformats::write.fs.mgh(outfile_rh, as.integer(mask$rh));
  if(!silent) {
    cat(sprintf("lh: %d verts total, %d in cortex, %d medial wall.\n", length(mask$lh), sum(mask$lh), (length(mask$lh)- sum(mask$lh))));
    cat(sprintf("rh: %d verts total, %d in cortex, %d medial wall.\n", length(mask$rh), sum(mask$rh), (length(mask$rh)- sum(mask$rh))))
    cat(sprintf("Mask files written to '%s' and '%s' in MGZ format.\n", outfile_lh, outfile_rh));
  }
}


if (length(args) == 2) {
  save_medial_mask(args[1], args[2]);
} else if(length(args) == 4) {
  save_medial_mask(args[1], args[2], args[3], args[4]);
} else {
  stop("USAGE: ./medial_mask.R <subjects_dir> <subject> [<lh_mask_output_file> <rh_mask_output_file>]");
}
