# Functions for accessing FreeSurfer group data, with knowledge on the directory structure.
# This is an abstraction layer over the freesurferforamts pacakge on group level.


#' @title Retrieve native space morphometry data for a group of subjects.
#'
#' @description Load native space morphometry data (like 'surf/lh.area') for a group of subjects from disk. Uses knowledge about the FreeSurfer directory structure to load the correct file.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subjects_list, vector of strings. The subject identifiers.
#'
#' @param measure, string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi, string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param format, string. One of 'mgh', 'mgz', 'curv'. Defaults to 'curv'.
#'
#' @return named list with native space morph data, the names are the subject identifiers from the subjects_list, and the values are morphometry data vectors (of different length, as each subject has a different vertex count in native space).
#'
#' @export
group.morph.native <- function(subjects_dir, subjects_list, measure, hemi, format='curv') {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    data_all_subjects = list();
    for(subject_id in subjects_list) {
        data_all_subjects[[subject_id]] = subject.morph.native(subjects_dir, subject_id, measure, hemi, format=format);
    }

   return(data_all_subjects);
}

