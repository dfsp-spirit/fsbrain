# Functions for accessing FreeSurfer subject data, with knowledge on the directory structure.
# This is an abstraction layer over the freesurferforamts pacakge on subject level.


#' @title Retrieve native space morphometry data for a single subject.
#'
#' @description Load native space morphometry data (like 'surf/lh.area') for a subject from disk. Uses knowledge about the FreeSurfer directory structure to load the correct file.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier
#'
#' @param measure, string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi, string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param format, string. One of 'mgh', 'mgz', 'curv'. Defaults to 'mgh'.
#'
#' @return vector with native space morph data
#'
#' @export
subject.morph.native <- function(subjects_dir, subject_id, measure, hemi, format='curv') {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(hemi == "both") {
        lh_curvfile = subject.filepath.morph.native(subjects_dir, subject_id, measure, "lh", warn_if_nonexistent=TRUE);
        lh_morph_data = freesurferformats::read.fs.morph(lh_curvfile);

        rh_curvfile = lh_curvfile = subject.filepath.morph.native(subjects_dir, subject_id, measure, "rh", warn_if_nonexistent=TRUE);
        rh_morph_data = freesurferformats::read.fs.morph(rh_curvfile);

        merged_morph_data = c(lh_morph_data, rh_morph_data);
        return(merged_morph_data);

    } else {
        curvfile = lh_curvfile = subject.filepath.morph.native(subjects_dir, subject_id, measure, hemi, warn_if_nonexistent=TRUE);
        return(freesurferformats::read.fs.morph(curvfile));
    }
}


#' @title Retrieve standard space morphometry data for a single subject.
#'
#' @description Load standard space morphometry data (like 'surf/lh.area.fwhm10.fsaverage.mgh') for a subject from disk. Uses knowledge about the FreeSurfer directory structure to load the correct file.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier
#'
#' @param measure, string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi, string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param fwhm, string. Smoothing as string, e.g. '10' or '25'.
#'
#' @param template_subject, string. Template subject name, defaults to 'fsaverage'.
#'
#' @param format, string. One of 'mgh', 'mgz', 'curv'. Defaults to 'mgh'.
#'
#' @return vector with standard space morph data
#'
#' @export
subject.morph.standard <- function(subjects_dir, subject_id, measure, hemi, fwhm='10', template_subject='fsaverage', format='mgh') {
    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(hemi == "both") {
        lh_curvfile = subject.filepath.morph.standard(subjects_dir, subject_id, measure, "lh", fwhm, warn_if_nonexistent=TRUE);
        lh_morph_data = freesurferformats::read.fs.morph(lh_curvfile);

        rh_curvfile = subject.filepath.morph.standard(subjects_dir, subject_id, measure, "rh", fwhm, warn_if_nonexistent=TRUE);
        rh_morph_data = freesurferformats::read.fs.morph(rh_curvfile);

        merged_morph_data = c(lh_morph_data, rh_morph_data);
        return(merged_morph_data);

    } else {
        curvfile = subject.filepath.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm, warn_if_nonexistent=TRUE);
        return(freesurferformats::read.fs.morph(curvfile));
    }
}


#' @title Construct filepath of standard space morphometry data file.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier
#'
#' @param measure, string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name.
#'
#' @param fwhm, string. Smoothing as string, e.g. '10' or '25'. Defaults to '10'.
#'
#' @param template_subject, string. Template subject name, defaults to 'fsaverage'.
#'
#' @param format, string. One of 'mgh', 'mgz', 'curv'. Defaults to 'mgh'.
#'
#' @param warn_if_nonexistent, logical. Whether to print a warning if the file does not exist or cannot be accessed. Defaults to FALSE.
#'
#' @return string, the file path.
#'
#' @export
subject.filepath.morph.standard <- function(subjects_dir, subject_id, measure, hemi, fwhm='10', template_subject='fsaverage', format='mgh', warn_if_nonexistent=FALSE) {
    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    if(nchar(fwhm) > 0) {
        fwhm_tag = sprintf(".fwhm%s", fwhm)
    } else {
        fwhm_tag = "" # Support opening files without any FWHM part
    }

    curvfile = file.path(subjects_dir, subject_id, "surf", sprintf("%s.%s%s.%s%s", hemi, measure, fwhm_tag, template_subject, freesurferformats::fs.get.morph.file.ext.for.format(format)));

    if((!file.exists(curvfile)) && (warn_if_nonexistent) ) {
        warning(sprintf("Standard space morphometry file '%s' for subject '%s' measure '%s' hemi '%s' fwhm '%s' cannot be accessed.\n", curvfile, subject_id, measure, hemi, fwhm));
    }

    return(curvfile);
}


#' @title Construct filepath of native space morphometry data file.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier
#'
#' @param measure, string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name.
#'
#' @param format, string. One of 'mgh', 'mgz', 'curv'. Defaults to 'curv'.
#'
#' @param warn_if_nonexistent, logical. Whether to print a warning if the file does not exist or cannot be accessed. Defaults to FALSE.
#'
#' @return string, the file path.
#'
#' @export
subject.filepath.morph.native <- function(subjects_dir, subject_id, measure, hemi, format='curv', warn_if_nonexistent=FALSE) {
    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    curvfile = file.path(subjects_dir, subject_id, "surf", sprintf("%s.%s%s", hemi, measure, freesurferformats::fs.get.morph.file.ext.for.format(format)));

    if((!file.exists(curvfile)) && (warn_if_nonexistent) ) {
        warning(sprintf("Native space morphometry file '%s' for subject '%s' measure '%s' hemi '%s' cannot be accessed.\n", curvfile, subject_id, measure, hemi));
    }

    return(curvfile);
}







