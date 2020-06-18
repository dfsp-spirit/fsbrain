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
#' @param mri_subdir character string or NULL, the subdir to use within the `mri` directory. Defaults to `NULL`, which means to read directly from the `mri` dir. You could use this to read volumes from the `mri/orig/` directory by setting it to `orig`.
#'
#' @return numerical array, the voxel data. If `with_header`, the full volume datastructure (see above).
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    brain = subject.volume(subjects_dir, 'subject1', 'brain', with_header = TRUE);
#'    # Use the vox2ras matrix from the header to compute RAS coordinates at CRS voxel (0, 0, 0):
#'    brain$header$vox2ras_matrix %*% c(0,0,0,1);
#' }
#'
#' @export
subject.volume <- function(subjects_dir, subject_id, volume, format='auto', drop_empty_dims=TRUE, with_header=FALSE, mri_subdir=NULL) {
    formats =tolower(format);
    if(!(format %in% c('auto', 'mgh', 'mgz'))) {
        stop(sprintf("The volume format must be one of ('auto', 'mgh', 'mgz') but is '%s'.\n", format));
    }

    if(is.null(mri_subdir)) {
        volume_filepath_noext = file.path(subjects_dir, subject_id, 'mri', volume);
    } else {
        volume_filepath_noext = file.path(subjects_dir, subject_id, 'mri', mri_subdir, volume);
    }

    if(format=="auto") {
        volume_file = freesurferformats::readable.files(volume_filepath_noext, precedence = c(".mgh", ".mgz"));
    } else {
        volume_file = paste(volume_filepath_noext, ".", format, sep='');
    }

    return(freesurferformats::read.fs.mgh(volume_file, drop_empty_dims=drop_empty_dims, with_header=with_header));
}


#' @title Compute R voxel index for FreeSurfer CRS voxel index.
#'
#' @description Performs a vox2vos transform from FreeSurfer to R indices.
#'
#' @param fs_crs integer vector of length 3, Freesurfer indices for column, row, and slice (CRS).
#'
#' @param add_affine logical, whether to add 1 to the output vector as the 4th value
#'
#' @return the R indices into the volume data for the given FreeSurfer CRS indices
#'
#' @examples
#'    # Get voxel intensity data on the command line, based
#'    #  on the FreeSUrfer (zero-based) CRS voxel indices:
#'    #  `mri_info --voxel 127 100 100 ~/data/tim_only/tim/mri/brain.mgz`
#'    # (the result is: 106.0)
#'    #
#'    # That should be identical to:
#'    # our_crs = vol.vox.from.crs(c(127,100,100), add_affine = FALSE);
#'    # brain$data[our_crs[1], our_crs[2], our_crs[3]];   # gives 106
#' @export
vol.vox.from.crs <- function(fs_crs, add_affine=FALSE) {

    if(! is.numeric(fs_crs)) {
        stop("Parameter 'fs_crs' must be numeric.");
    }

    # Transform from FreeSurfer CRS to the R indices. This is all we need to do, as the orientation of the volume
    # is already correct: this has been taken care of by freesurferformats::read.fs.mgh().
    our_crs = fs_crs + 1;

    if(add_affine) {
        if(is.vector(our_crs)) {
            if(length(our_crs) != 3) {
                stop("Parameter 'fs_crs' must have length 3 if it is a vector.");
            }
            return(c(our_crs, 1));
        } else if(is.matrix(our_crs)) {
            if(ncol(our_crs) != 3) {
                stop("Parameter 'fs_crs' must have 3 columns if it is a matrix");
            }
            return(cbind(our_crs, 1));
        } else {
            stop("Parameter 'fs_crs' must be a vector or matrix.");
        }

    } else {
        return(our_crs);
    }
}


#' @title The FreeSurfer default vox2ras_tkr matrix.
#'
#' @description Applying this matrix to a FreeSurfer CRS index of a conformed volume will give you the RAS coordinates of the voxel in surface coordinates, i.e., in the coordinates used in surface file like `lh.white`. The central voxel is 127,127,127 when using zero-based indices (or 128,128,128 when using one-based indices), meaning its surface RAS coordinates are 0.0, 0.0, 0.0. The returned matrix is the inverse of the `ras2vox_tkr` matrix.
#'
#' @return numeric 4x4 matrix, the FreeSurfer vox2ras_tkr matrix.
#'
#' @examples
#'    # Compute surface RAS coordinate of voxel with CRS (0L, 0L, 0L):
#'    vox2ras_tkr() %*% c(0, 0, 0, 1);
#'    # Show that voxel with CRS (128,128,128) is at the
#'    #  origin (0.0, 0.0, 0.0) of the surface RAS coordinate system:
#'    (vox2ras_tkr() %*% c(128, 128, 128, 1))[1:3];
#'
#' @family surface and volume coordinates
#'
#' @export
vox2ras_tkr <- function() {
    cat(sprintf("################### RUNNING vox2ras_tkr ###############\n"));
    return(matrix(c(-1,0,0,0, 0,0,-1,0, 0,1,0,0, 128,-128,128,1), nrow = 4));
}


#' @title The FreeSurfer default ras2vox_tkr matrix.
#'
#' @description Applying this matrix to a FreeSurfer surface RAS coordinate (from a surface file like `lh.white`) will give you the voxel index (CRS) in a conformed FreeSurfer volume.  The returned matrix is the inverse of the `vox2ras_tkr` matrix.
#'
#' @return numeric 4x4 matrix, the FreeSurfer ras2vox_tkr matrix.
#'
#' @examples
#'    # Compute the FreeSurfer CRS voxel index of surface RAS coordinate (0.0, 0.0, 0.0):
#'    ras2vox_tkr() %*% c(0, 0, 0, 1);
#'    # Show that the voxel at surface RAS corrds (0.0, 0.0, 0.0) is the one with CRS (128, 128, 128):
#'    ras2vox_tkr() %*% c(0.0, 0.0, 0.0, 1);
#'
#' @family surface and volume coordinates
#'
#' @export
ras2vox_tkr <- function() {
    return(matrix(c(-1,-0,-0,-0, -0,-0,+1,-0, -0,-1,-0,-0, 128,128,128,1), nrow = 4));
}


