#!/usr/bin/env Rscript
#
# Extract the given subjects from a QDEC longtitudinal table and write tghe subset to a new file.
#


library("fsbrain");
args = commandArgs(trailingOnly=TRUE);


save_medial_mask <- function(subjects_dir, subject_id, outfile_lh="lh_mask.mgz", outfile_rh="rh_mask.mgz", silent=FALSE) {
    mask = fsbrain::subject.mask(subjects_dir, subject_id);
    freesurferformats::write.fs.mgh(outfile_lh, as.integer(mask$lh));
    freesurferformats::write.fs.mgh(outfile_rh, as.integer(mask$rh));
    if(!silent) {
        cat(sprintf("lh: %d verts total, %d in cortex, %d medial wall.\n", length(mask$lh), sum(mask$lh), (length(mask$lh)- sum(mask$lh))));
        cat(sprintf("rh: %d verts total, %d in cortex, %d medial wall.\n", length(mask$rh), sum(mask$rh), (length(mask$rh)- sum(mask$rh))))
        cat(sprintf("Mask files written to '%s' and '%s'.\n", outfile_lh, outfile_rh));
    }
}


if (length(args) == 3) {
    fsbrain:::qdec.table.filter(args[1], fsbrain::read.md.subjects(args[2], FALSE), args[3]);
} else {
    stop("USAGE: ./qdeclong_subset <input_qdec_file> <subject_file> <output_qdec_file>\n <input_qdec_file>  : Path to a qdec.table.dat file in longitudinal format.\n <subject_file>    : Path to subjects file containing 1 subject ID per line. Subjects must occur in <input_qdec_file>.\n <output_qdec_file> : Path for the output qdec long table containing only subjects from <subjects_file>. Will be created.");

}

