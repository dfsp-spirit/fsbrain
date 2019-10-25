# Functions for accessing FreeSurfer group data, with knowledge on the directory structure.
# This is an abstraction layer over the freesurferformats package on group level.


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
#' @family morphometry data functions
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    subjects_list = c("subject1", "subject2");
#'    data = group.morph.native(subjects_dir, subjects_list, "thickness", "lh");
#' }
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


#' @title Retrieve standard space morphometry data for a group of subjects.
#'
#' @description Load standard space morphometry data (like 'surf/lh.area') for a group of subjects from disk. Uses knowledge about the FreeSurfer directory structure to load the correct file.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subjects_list, vector of strings. The subject identifiers.
#'
#' @param measure, string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi, string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param fwhm, string. Smoothing as string, e.g. '10' or '25'.
#'
#' @param template_subject, string. Template subject name, defaults to 'fsaverage'.
#'
#' @param format, string. One of 'mgh', 'mgz', 'curv'. Defaults to 'mgh'.
#'
#' @return named list with standard space morph data, the names are the subject identifiers from the subjects_list, and the values are morphometry data vectors (all with identical length, the data is mapped to a template subject).
#'
#' @family morphometry data functions
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    subjects_list = c("subject1", "subject2");
#'    data = group.morph.standard(subjects_dir, subjects_list, "thickness", "lh", fwhm='10');
#' }
#'
#' @export
group.morph.standard <- function(subjects_dir, subjects_list, measure, hemi, fwhm='10', template_subject='fsaverage', format='mgh') {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    data_all_subjects = list();
    for(subject_id in subjects_list) {
        data_all_subjects[[subject_id]] = subject.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm=fwhm, template_subject=template_subject, format=format);
    }

    return(data_all_subjects);
}


#' @title Retrieve label data for a group of subjects.
#'
#' @description Load a label (like 'label/lh.cortex.label') for a group of subjects from disk. Uses knowledge about the FreeSurfer directory structure to load the correct file.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subjects_list, vector of strings. The subject identifiers.
#'
#' @param label, string. Name of the label file, without the hemi part (if any), but including the '.label' suffix. E.g., 'cortex.label' for '?h.cortex.label'
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param return_one_based_indices, logical. Whether the indices should be 1-based. Indices are stored zero-based in the file, but R uses 1-based indices. Defaults to TRUE, which means that 1 will be added to all indices read from the file before returning them.
#'
#' @return named list of integer vectors with label data: Each name is a subject identifier from subjects_list, and the values are lists of the vertex indices in the respective label. See 'return_one_based_indices' for important information.
#'
#' @family label data functions
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    subjects_list = c("subject1", "subject2");
#'    labels = group.label(subjects_dir, subjects_list, 'cortex.label', "lh");
#' }
#'
#' @export
group.label <- function(subjects_dir, subjects_list, label, hemi, return_one_based_indices=TRUE) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    all_labels = list();
    for(subject_id in subjects_list) {
        all_labels[[subject_id]] = subject.label(subjects_dir, subject_id, label, hemi, return_one_based_indices=return_one_based_indices);
    }

    return(all_labels);
}



#'@title Load annotations for a group of subjects.
#'
#' @description Load a brain surface annotation, i.e., a cortical parcellation based on an atlas, for a group of subjects.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subjects_list, vector of strings. The subject identifiers.
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param atlas, string. The atlas name. E.g., "aparc", "aparc.2009s", or "aparc.DKTatlas". Used to construct the name of the annotation file to be loaded.
#'
#' @return list of annotations, as returned by freesurferformats::read.fs.annot(). If hemi is 'both', the annotations are the results of merging over the hemispheres for each subject.
#'
#' @family atlas functions
#'
#'@examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    subjects_list = c("subject1", "subject2");
#'    annotations = group.annot(subjects_dir, subjects_list, "lh", "aparc");
#' }
#'
#' @export
group.annot <- function(subjects_dir, subjects_list, hemi, atlas) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    all_annots = list();
    for(subject_id in subjects_list) {
        all_annots[[subject_id]] = subject.annot(subjects_dir, subject_id, hemi, atlas);
    }

    return(all_annots);
}

