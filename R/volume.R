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
#' @return numerical array, the voxel data.
#'
#' @export
subject.volume <- function(subjects_dir, subject_id, volume, format='AUTO') {
    if(!(format %in% c('AUTO', 'mgh', 'mgz'))) {
        stop(sprintf("The volume format must be one of ('AUTO', 'mgh', 'mgz') but is '%s'.\n", format));
    }

    volume_filepath_noext = file.path(subjects_dir, subject_id, 'mri', volume);

    if(format=="AUTO") {
        volume_file = readable.volume(volume_filepath_noext);
    } else {
        volume_file = paste(volume_filepath_noext, ".", format, sep='');
    }
    return(drop(freesurferformats::read.fs.mgh(volume_file)));
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
