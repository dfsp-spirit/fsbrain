# Functions for volume loading.


#' @title Read a brain volume.
#'
#' @description Load a brain volume (like `mri/brain.mgz`) for a subject from disk. Uses knowledge about the FreeSurfer directory structure to load the correct file.
#'
#' @param subjects_dir character string, the FreeSurfer `SUBJECTS_DIR`, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id character string, the subject identifier.
#'
#' @param volume character string, name of the volume file without file extension. Examples: `brain` or `aseg`.
#'
#' @param format string. One of 'mgh', 'mgz', 'AUTO'. If left at the default value 'AUTO', the function will look for files with extensions 'mgh' and 'mgz' (in that order) and use the first one that exists.
#'
#' @param drop_empty_dims logical, whether to drop empty dimensions of the returned data. Passed to \code{\link[freesurferformats]{read.fs.mgh}}.
#'
#' @param with_header logical. Whether to return the header as well. If TRUE, return a named list with entries "data" and "header". The latter is another named list which contains the header data. These header entries exist: "dtype": int, one of: 0=MRI_UCHAR; 1=MRI_INT; 3=MRI_FLOAT; 4=MRI_SHORT. "voldim": integer vector. The volume (=data) dimensions. E.g., c(256, 256, 256, 1). These header entries may exist: "vox2ras_matrix" (exists if "ras_good_flag" is 1), "mr_params" (exists if "has_mr_params" is 1). Passed to \code{\link[freesurferformats]{read.fs.mgh}}.
#'
#' @return numerical array, the voxel data. If `with_header`, the full volume datastructure (see above).
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    brain = subject.volume(subjects_dir, 'subject1', 'brain', with_header = TRUE);
#'    # Use the vox2ras matrix from the header to compute RAS coordinates at volume center:
#'    brain$header$vox2ras_matrix %*% c(0,0,0,1);
#' }
#'
#' @export
subject.volume <- function(subjects_dir, subject_id, volume, format='AUTO', drop_empty_dims=TRUE, with_header=FALSE) {
    if(!(format %in% c('AUTO', 'mgh', 'mgz'))) {
        stop(sprintf("The volume format must be one of ('AUTO', 'mgh', 'mgz') but is '%s'.\n", format));
    }

    volume_filepath_noext = file.path(subjects_dir, subject_id, 'mri', volume);

    if(format=="AUTO") {
        volume_file = readable.volume(volume_filepath_noext);
    } else {
        volume_file = paste(volume_filepath_noext, ".", format, sep='');
    }

    return(freesurferformats::read.fs.mgh(volume_file, drop_empty_dims=drop_empty_dims, with_header=with_header));
}


#' @title Find files with the given base name that exist.
#'
#' @param filepath character string, path to a file without extension
#'
#' @param precedence vector of character strings, the file extensions to check.
#'
#' @param error_if_none logical, whether to raise an error if none of the files exist
#'
#' @return character string, the path to the first existing file (or NULL if none of them exists).
#'
#' @keywords internal
readable.volume <- function(filepath, precedence=c('.mgh', '.mgz'), error_if_none=TRUE) {
    candidate_files = paste(filepath, precedence, sep='');
    for(cfile in candidate_files) {
        if(file.exists(cfile)) {
            return(cfile);
        }
    }
    if(error_if_none) {
        stop(sprintf("At location '%s' exists no file with any of the %d extensions '%s'.\n", filepath, length(precedence), paste(precedence, collapse=' ')));
    } else {
        return(NULL);
    }
}
