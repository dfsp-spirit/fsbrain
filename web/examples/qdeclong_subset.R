#!/usr/bin/env Rscript
#
# Extract the given subjects from a QDEC longtitudinal table and write tghe subset to a new file.
#
# This script uses an internal fsbrain function (using the fsbrain:::<function> notation), which you shoud NOT do. Internal functions are not part
#  of the official API and may change or disappear between versions without notice. You can consider it an anti-example. To be honest it's here
#  because I needed it and did not know where else to put it. It will disappear one day.
#

library("fsbrain");
args = commandArgs(trailingOnly=TRUE);


if (length(args) == 3) {
    subjects_list = fsbrain::read.md.subjects(args[2], FALSE);
    fsbrain:::qdec.table.filter(args[1], subjects_list, args[3]);
} else {
    stop("USAGE: ./qdeclong_subset.R <input_qdec_file> <subject_file> <output_qdec_file>\n <input_qdec_file>  : Path to a qdec.table.dat file in longitudinal format.\n <subject_file>     : Path to subjects file containing 1 subject ID per line. Subjects must occur in <input_qdec_file>.\n <output_qdec_file> : Path for the output qdec long table containing only subjects from <subjects_file>. Will be created.");

}

