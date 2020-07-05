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
#' @param cortex_only logical, whether to mask the medial wall, i.e., whether the morphometry data for all vertices which are *not* part of the cortex (as defined by the label file `label/?h.cortex.label`) should be replaced with NA values. In other words, setting this to TRUE will ignore the values of the medial wall between the two hemispheres. If set to true, the mentioned label file needs to exist for the subjects. Defaults to FALSE.
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
group.morph.native <- function(subjects_dir, subjects_list, measure, hemi, format='curv', cortex_only=FALSE) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    check.subjectslist(subjects_list, subjects_dir=subjects_dir);

    data_all_subjects = list();
    for(subject_id in subjects_list) {
        data_all_subjects[[subject_id]] = subject.morph.native(subjects_dir, subject_id, measure, hemi, format=format, cortex_only=cortex_only);
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
#' @param cortex_only logical, whether to mask the medial wall, i.e., whether the morphometry data for all vertices which are *not* part of the cortex (as defined by the label file `label/?h.cortex.label`) should be replaced with NA values. In other words, setting this to TRUE will ignore the values of the medial wall between the two hemispheres. If set to true, the mentioned label file needs to exist for the template subject. Defaults to FALSE.
#'
#' @param df logical, whether to return a dataframe instead of the named list. The dataframe will have one subject per column, and *n* rows, where *n* is the number of vertices of the template subject surface.
#'
#' @return named list with standard space morph data, the names are the subject identifiers from the subjects_list, and the values are morphometry data vectors (all with identical length, the data is mapped to a template subject).
#'
#' @family morphometry data functions
#'
#' @examples
#' \dontrun{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    subjects_list = c("subject1", "subject2");
#'    fulldata = group.morph.standard(subjects_dir, subjects_list, "thickness", "lh", fwhm='10');
#'    mean(fulldata$subject1);
#'
#'    cortexdata = group.morph.standard(subjects_dir, subjects_list, "thickness",
#'     "lh", fwhm='10', cortex_only=FALSE);
#'    mean(cortexdata$subject1, na.rm=TRUE);
#' }
#'
#' @export
group.morph.standard <- function(subjects_dir, subjects_list, measure, hemi='both', fwhm='10', template_subject='fsaverage', format='mgh', cortex_only=FALSE, df=FALSE) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    check.subjectslist(subjects_list, subjects_dir=subjects_dir);

    data_all_subjects = list();
    for(subject_id in subjects_list) {
        data_all_subjects[[subject_id]] = subject.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm=fwhm, template_subject=template_subject, format=format, cortex_only=cortex_only);
    }

    if(df) {
        return(as.data.frame(data_all_subjects));
    }
    return(data_all_subjects);
}


#' @title Read combined data for a group from a single file.
#'
#' @description Read morphometry data for a group from a matrix in a single MGH or MGZ file.
#'
#' @param filepath character string, path to a file in MGH or MGZ format
#'
#' @param df logical, whether to return a data.frame, like \code{group.morph.standard}. If FALSE, the raw 4d matrix is returned.
#'
#' @return dataframe or 4d matrix, the morph data. See parameter 'df' for details.
#'
#' @note The file has typically been generated by running \code{mris_preproc} and/or \code{mri_surf2surf} on the command line, or written from R using \code{\link{write.group.morph.standard.sf}}. The file contains no information on the subject identifiers, you need to know the subjects and their order in the file. Same goes for the hemisphere.
#'
#' @seealso If you have created the input data file in FreeSurfer based on an FSGD file, you can read the subject identifiers from that FSGD file using \code{\link{read.md.subjects.from.fsgd}}.
#'
#' @export
group.morph.standard.sf <- function(filepath, df=TRUE) {
    group_morph_data = freesurferformats::read.fs.volume(filepath);
    ddim = dim(group_morph_data);
    if(length(ddim) != 4L) {
        stop(sprintf("Expected 4-dimensional volume, but file has %d dimensions. Not a group data file?\n", length(ddim)));
    }
    if(ddim[2] != 1L | ddim[3] != 1L) {
        stop(sprintf("Expected data dimension (n, 1, 1, m) with n vertices and m subjects, but found (%d, %d, %d, %d). Not a group data file?\n", ddim[1], ddim[2], ddim[3], ddim[4]));
    }
    if(df) {
        return(unname(as.data.frame(drop(group_morph_data))));
    } else {
        return(group_morph_data);
    }
}


#' @title Write combined data for a group to a single MGH file.
#'
#' @description Write morphometry data for a group into a single MGH or MGZ file.
#'
#' @param filepath character string, path to the target file, should end with '.mgh' or '.mgz'.
#'
#' @param data numerical 4d matrix, with first dimension holding 1D data for all vertices of a subject, 2nd and 3rd dimension are of length 1, and 4th dimenions is for each subject.
#'
#' @note The file will contain no information on the subject identifiers. The data can be for one or both hemispheres. See \code{\link{group.morph.standard.sf}} to read the data back into R.
#'
#' @export
write.group.morph.standard.sf <- function(filepath, data) {
    data = group.data.to.array(data);
    freesurferformats::write.fs.mgh(filepath, data);
}


#' @title Convert group 1D data to array format.
#'
#' @description In general, 1D morphometry data for a group can be stored in a dataframe, a named list, or already a 4D array. This function will convert the given format to matrix format.
#'
#' @param data 4D array, named list, or data.frame of group data. The data is expected to be a vector (1D) per subject, as suitable for surface based (vertex-wise) measures.
#'
#' @return the array form of the group data. No values are changed, this is only a different data type.
#'
#' @keywords internal
group.data.to.array <- function(data) {
    if(is.list(data)) {
        data = as.data.frame(data);
    }
    if(is.data.frame(data)) {
        data = data.matrix(data);
        ddim = dim(data);
        if(length(ddim) != 2L) {
            stop("Invalid dataframe format for 1D group morphometry data.");
        }
        dim(data) = c(ddim[1], 1, 1, ddim[2]);
    }
    if(! is.array(data)) {
        stop("Parameter 'data' must be a matrix, named list, or data.frame.");
    }
    if(length(dim(data)) != 4L) {
        stop("Data must have 4 dimensions.");
    }
    return(data);
}


#' @title Check whether the subjects_list looks good, warn if not.
#'
#' @param subjects_list vector of character strings, the subject IDs.
#'
#' @param subjects_dir optional, character string. The full path to the subjects directory containing all subjects. If given, extra checks will be performed, e.g., whether the subjects exist in this directory.
#'
#' @param report_name character string, the variable name that should be used for the list in the messages.
#'
#' @keywords internal
check.subjectslist <- function(subjects_list, subjects_dir=NULL, report_name='subjects_list') {
    if(is.null(subjects_list)) {
        warning(sprintf("The '%s' must not be NULL.\n", report_name));
    } else {
        if(length(subjects_list) < 1L) {
            warning(sprintf("The '%s' is empty.\n", report_name));
        }
        if(length(subjects_list) != length(unique(subjects_list))) {
            warning(sprintf("The '%s' contains %d duplicate entries.\n", report_name, (length(subjects_list) - length(unique(subjects_list)))));
        }
        if(! is.null(subjects_dir)) {
            if(! dir.exists(subjects_dir)) {
                stop(sprintf("The subjects_dir at '%s' does not exist.\n", subjects_dir));
            } else {
                subjects_missing_dir = c();
                for(subject in subjects_list) {
                    sjd = file.path(subjects_dir, subject);
                    if(! dir.exists(sjd)) {
                        subjects_missing_dir = c(subjects_missing_dir, subject);
                    }
                }
                if(length(subjects_missing_dir) > 0L) {
                    stop(sprintf("Directories for the following %d subjects missing in subjects directory '%s': %s\n", length(subjects_missing_dir), subjects_dir, paste(subjects_missing_dir, collapse=', ')));
                }
            }
        }
    }
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

    check.subjectslist(subjects_list, subjects_dir=subjects_dir);

    all_labels = list();
    for(subject_id in subjects_list) {
        all_labels[[subject_id]] = subject.label(subjects_dir, subject_id, label, hemi, return_one_based_indices=return_one_based_indices);
    }

    return(all_labels);
}


#' @title Retrieve surface mesh data for a group of subjects.
#'
#' @inheritParams group.label
#'
#' @param hemi string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to construct the names of the mesh files to be loaded.
#'
#' @param surface character string, the surface to load. Something like 'white' or 'pial'.
#'
#' @param force_hemilist logical, whether to force the individual values inside the named return value list to be hemilists (even if the 'hemi' parameter is not set to 'both'). If this is FALSE, the inner values will contain the respective (lh or rh) surface only.
#'
#' @return named list of surfaces: Each name is a subject identifier from subjects_list, and the values are hemilists of `fs.surface` instances.
#'
#' @family mesh data functions
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    subjects_list = c("subject1", "subject2");
#'    surfaces = group.surface(subjects_dir, subjects_list, 'white', "both");
#' }
#'
#' @export
group.surface <- function(subjects_dir, subjects_list, surface, hemi='both', force_hemilist = TRUE) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    check.subjectslist(subjects_list, subjects_dir=subjects_dir);

    all_surfaces = list();
    for(subject_id in subjects_list) {
        all_surfaces[[subject_id]] = subject.surface(subjects_dir, subject_id, surface, hemi, force_hemilist = force_hemilist);
    }
    return(all_surfaces);
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

    check.subjectslist(subjects_list, subjects_dir=subjects_dir);

    all_annots = list();
    for(subject_id in subjects_list) {
        all_annots[[subject_id]] = subject.annot(subjects_dir, subject_id, hemi, atlas);
    }

    return(all_annots);
}

