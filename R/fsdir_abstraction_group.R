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
#' \dontrun{
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
#' @param df_t logical, whether to return a transposed dataframe. Only one of df or df_t must be TRUE.
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
group.morph.standard <- function(subjects_dir, subjects_list, measure, hemi='both', fwhm='10', template_subject='fsaverage', format='mgh', cortex_only=FALSE, df=FALSE, df_t=FALSE) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    check.subjectslist(subjects_list, subjects_dir=subjects_dir);

    if(df_t) {
        if(df) {
            stop("Only one of parameters 'df' and 'df_t' can be TRUE.");
        }
        num_subjects = length(subjects_list);
        num_verts = length(subject.morph.standard(subjects_dir, subjects_list[1], measure, hemi, fwhm=fwhm, template_subject=template_subject, format=format, cortex_only=cortex_only));
        data_mtx = matrix(rep(0.0, num_subjects * num_verts), ncol = num_verts, nrow = num_subjects);

        subject_index = 1L;
        for(subject_id in subjects_list) {
            data_mtx[subject_index, ] = subject.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm=fwhm, template_subject=template_subject, format=format, cortex_only=cortex_only);
            subject_index = subject_index + 1L;
        }
        df = as.data.frame(data_mtx);
        rownames(df) = subjects_list;
        return(df);
    }

    data_all_subjects = list();
    for(subject_id in subjects_list) {
        data_all_subjects[[subject_id]] = subject.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm=fwhm, template_subject=template_subject, format=format, cortex_only=cortex_only);
    }

    if(df) {
        return(as.data.frame(data_all_subjects));
    }
    return(data_all_subjects);
}


#' @title Split a per-vertex group data matrix for both hemispheres into a hemilist at given index.
#'
#' @param data numerical matrix or dataframe of per-vertex data, with subjects in columns
#'
#' @param numverts_lh scalar positive integer, the number of vertices in the left hemisphere mesh (defining the index where to split).
#'
#' @return \code{\link[fsbrain]{hemilist}} of the data, split at the index.
#'
#' @examples
#' \dontrun{
#'    fsbrain::download_optional_data();
#'    fsbrain::download_fsaverage(TRUE);
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    subjects_list = c("subject1", "subject2");
#'    data = group.morph.standard(subjects_dir, subjects_list, "thickness", "lh", fwhm='10');
#'    numverts_lh = subject.num.verts(subjects_dir, "fsaverage", hemi="lh");
#'    data_hemilist = groupmorph.split.hemilist(data, numverts_lh);
#' }
#'
#' @export
groupmorph.split.hemilist <- function(data, numverts_lh) {
    return(list('lh'=data[1:numverts_lh, ], 'rh'=data[(numverts_lh+1L):nrow(data), ]));
}


#' @title Write standard space group data to a standard FreeSurfer directory stucture.
#'
#' @inheritParams group.morph.standard
#'
#' @inheritParams write.group.morph.standard.mf
#'
#' @param data the data matrix
#'
#' @param measure_name character string, the data part of the generated file names, e.g., 'thickness' or 'area'.
#'
#' @seealso \code{\link[fsbrain]{write.group.morph.standard.sf}} and \code{\link[fsbrain]{write.group.morph.standard.mf}}
#'
#' @param template_lh_numverts positive integer, the vertex count of the left hemi of the template subject, only used if 'hemi' is 'both'. If hemi is both and this is unspecified (left at the default value \code{NULL}), the template subject needs to exist in the 'subjects_dir' to determine the vertex count of the left hemisphere, so that the data can be split into the \code{lh} and \code{rh} files at the correct index.
#'
#' @examples
#' \dontrun{
#' dm = matrix(rnorm(325684 * 6, 5.0, 1.2), ncol = 6);
#' subjects = paste("subject", seq(6), sep="");
#' write.group.morph.standard("/tmp/groupdata", subjects, dm,
#'   "rand", template_lh_numverts = 325684 / 2);
#' }
#'
#' @export
write.group.morph.standard <- function(subjects_dir, subjects_list, data, measure_name, hemi='both', fwhm='10', template_subject='fsaverage', format='mgh', create_dirs = TRUE, template_lh_numverts = NULL) {
    if(! is.data.frame(data)) {
        data = as.data.frame(data);
    }

    if(ncol(data) != length(subjects_list)) {
        stop(sprintf("Received %d subjects in 'subjects_list' but 'data' is for %d subjects, counts must match.\n", length(subjects_list), ncol(data)));
    }

    if(hemi == "both") {
        if(is.null(template_lh_numverts)) {
            template_lh_numverts = subject.num.verts(subjects_dir, template_subject, surface = "white", hemi = "lh");
        }
        lh_filenames = subject.filepath.morph.standard(subjects_dir, subjects_list, measure = measure_name, hemi = "lh", fwhm = fwhm, template_subject = template_subject, format = format);
        rh_filenames = subject.filepath.morph.standard(subjects_dir, subjects_list, measure = measure_name, hemi = "rh", fwhm = fwhm, template_subject = template_subject, format = format);
        filepaths_hl = hemilist(lh_filenames, rh_filenames);
        data_hl = groupmorph.split.hemilist(data, template_lh_numverts);
    } else {
        filepaths_hl = hemilist.wrap(subject.filepath.morph.standard(subjects_dir, subjects_list, measure = measure_name, hemi = hemi, fwhm = fwhm, template_subject = template_subject, format = format), hemi);
        data_hl = hemilist.wrap(data, hemi);
    }
    write.group.morph.standard.mf(filepaths_hl, data_hl, format = format, create_dirs = create_dirs);
}


#' @title Write per-vertex standard space data for a group of subjects to given file names.
#'
#' @param filepaths_hl \code{\link[fsbrain]{hemilist}} of vectors of character strings, the full paths to the output files, including file names and extension.
#'
#' @param data_hl \code{\link[fsbrain]{hemilist}} of numerical matrix or data.frame, the morph data for the hemispheres of all subjects. See \code{groupmorph.split.hemilist} to get this format if you have a full matrix or dataframe for both hemispheres.
#'
#' @param format character string, a valid format spec for \code{freesurferformats::write.fs.morph}, e.g., "auto" to derive from filename, "mgh", "mgz", "curv" or others.
#'
#' @param create_dirs logical, whether to create missing (sub) directories which occur in the 'filepaths'.
#'
#' @seealso \code{\link[fsbrain]{write.group.morph.standard.sf}} to write the data to a single stacked file instead.
#'
#' @export
write.group.morph.standard.mf <- function(filepaths_hl, data_hl, format = "auto", create_dirs = TRUE) {
    if(! is.hemilist(data_hl)) {
        stop("Parameter 'data_hl' must be a hemilist");
    }
    if(! is.hemilist(filepaths_hl)) {
        stop("Parameter 'filepaths_hl' must be a hemilist");
    }

    if(! is.null(filepaths_hl$lh)) {
        write.group.morph.standard.singlehemi(filepaths_hl$lh, data_hl$lh, format = format, create_dirs = create_dirs);
    }
    if(! is.null(filepaths_hl$rh)) {
        write.group.morph.standard.singlehemi(filepaths_hl$rh, data_hl$rh, format = format, create_dirs = create_dirs);
    }
}


#' @title Write single hemi per-vertex data for a group of subjects to given file names.
#'
#' @param filepaths vector of character strings, the full paths to the output files, including file names and extension.
#'
#' @param data numerical matrix or data.frame, the morph data for a single hemi (as returned by \code{group.morph.standard}). Number of subjects (columns) must match the length of the 'filepaths'.
#'
#' @param format character string, a valid format spec for \code{freesurferformats::write.fs.morph}, e.g., "auto" to derive from filename, "mgh", "mgz", "curv" or others.
#'
#' @param create_dirs logical, whether to create missing (sub) directories which occur in the 'filepaths'.
#'
#' @keywords internal
write.group.morph.standard.singlehemi <- function(filepaths, data, format = "auto", create_dirs = TRUE) {
    if(! is.data.frame(data)) {
        data = as.data.frame(data);
    }

    if(ncol(data) != length(filepaths)) {
        stop(sprintf("Received %d filepaths but data is for %d subjects, counts must match.\n", length(filepaths), ncol(data)));
    }

    subject_idx = 1L;
    for (subject_name in colnames(data)) {
        cfp = filepaths[subject_idx];
        if(create_dirs & (! dir.exists(dirname(cfp)))) {
            dir.create(dirname(cfp), recursive = TRUE);
        }
        freesurferformats::write.fs.morph(cfp, data[[subject_name]], format = format);
        subject_idx = subject_idx + 1L;
    }
    return(invisible(NULL));
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
#' @seealso \code{\link[fsbrain]{write.group.morph.standard.mf}} to write the data to one file per hemi per subject instead. If you have created the input data file in FreeSurfer based on an FSGD file, you can read the subject identifiers from that FSGD file using \code{\link{read.md.subjects.from.fsgd}}.
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


#' @title Reshape and write combined per-vertex data for a group to a single MGH file.
#'
#' @description Write morphometry data for a group into a single MGH or MGZ file. In neuroimaging, the first 3 dimensions in the resulting 4D volume file are space, and the 4th is the time/subject dimension.
#'
#' @param filepath character string, path to the target file, should end with '.mgh' or '.mgz'.
#'
#' @param data numerical 2D matrix, with the rows identifying the subjects and the columns identifying the vertices.
#'
#' @note The file will contain no information on the subject identifiers. The data can be for one or both hemispheres. See \code{\link{group.morph.standard.sf}} to read the data back into R.
#'
#' @examples
#' \dontrun{
#' # create per-vertex data for 5 subjects.
#' mat = matrix(rnorm(5 * 163842, 3.0, 0.5), nrow=5, ncol = 163842);
#' fsbrain::write.group.morph.standard.sf("~/group_pvd.mgz", mat);
#' }
#'
#' @export
write.group.morph.standard.sf <- function(filepath, data) {
    data = group.data.to.array(data);
    freesurferformats::write.fs.mgh(filepath, data);
}


#' @title Convert group 2D data (1 vector per subject) to 4D array format.
#'
#' @description In general, 1D morphometry data for a group can be stored in a dataframe, a named list, or already a 4D array. This function will convert the given format to 4D array format.
#'
#' @param data 2D matrix, named list, or data.frame of group data. The data is expected to be a vector (1D) per subject, as suitable for surface based (vertex-wise) measures. Subjects in rows, per-vertex data in columns.
#'
#' @return the 4D array form of the group data. No values are changed, this is only a different data layout. In neuroimaging, the first 3 dimensions are space, and the 4th is the time/subject dimension.
#'
#' @examples
#'     # create per-vertex data for 255 subjects.
#'     mat = matrix(rnorm(255 * 163842, 3.0, 0.5), nrow=255, ncol = 163842);
#'     fsbrain:::group.data.to.array(mat);
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
    if(is.matrix(data)) {
        ddim = dim(data);
        data = array(data=data, dim = c(ddim[1], 1, 1, ddim[2]));
    }

    # Check output format.
    if(! is.array(data)) {
        stop("Output data must be an array, conversion failed.");
    }
    if(length(dim(data)) != 4L) {
        stop(sprintf("Output data must have 4 dimensions but has %d.\n", length(dim(data))));
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
#' @note This function will stop if subjects dirs are missing.
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
            duplicates = c();
            for(s in subjects_list) {
                if(length(which(subjects_list == s)) > 1L) {
                    duplicates = c(duplicates, s);
                }
            }
            warning(sprintf(" Duplicates: %s.\n", paste(unique(duplicates), collapse = ", ")));
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


#' @title Report subjects missing files
#'
#' @inheritParams group.morph.native
#'
#' @param sfiles vector of character strings, the file names, relative to the data dir of the subject.
#'
#' @return vector of character strings, the subjects missing any of the files.
#'
#' @keywords internal
check.subjects.files <- function(subjects_dir, subjects_list, sfiles=c("surf/lh.thickness", "surf/rh.thickness", "surf/lh.white", "surf/rh.white", "label/lh.aparc.annot", "label/rh.aparc.annot")) {
    subjects_missing_files = c();
    if(length(subjects_list) < 1L) {
        stop(sprintf("The subjects_list must not be empty.\n"));
    }
    if(length(subjects_dir) > 1L) {
        stop("The 'subjects_dir' must be a single directory.");
    }
    if(! dir.exists(subjects_dir)) {
        stop("Directory subjects_dir does not exist.");
    }
    for(subject in subjects_list) {
        sjd = file.path(subjects_dir, subject);
        if(! dir.exists(sjd)) {
            subjects_missing_files = c(subjects_missing_files, subject);
            next;
        }
        for(sfile in sfiles) {
            ffile = file.path(sjd, sfile);
            if(! file.exists(ffile)) {
                subjects_missing_files = c(subjects_missing_files, subject);
                break;
            }
        }
    }
    return(subjects_missing_files);
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
#' \dontrun{
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
#' \dontrun{
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
#' \dontrun{
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

