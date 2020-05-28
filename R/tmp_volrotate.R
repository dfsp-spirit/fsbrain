# These functions have been moved to freesurferformats, but are not exported from it in v0.1.10.
# They will be exported in the next freesurferformats release, and then this file will be deleted.

#' @title Rotate a 2D matrix in 90 degree steps.
#'
#' @param slice a 2D matrix
#'
#' @param degrees integer, must be a (positive or negative) multiple of 90
#'
#' @return 2D matrix, the rotated matrix
#'
#' @export
rotate2D <- function(slice, degrees=90) {
    if(length(dim(slice)) != 2L) {
        stop("Slice must be a 2D matrix.");
    }
    degrees = as.integer(degrees %% 360L);
    if(!degrees %in% as.integer(c(0, 90, 180, 270))) {
        stop("Parameter 'degrees' must be a multiple of 90 (it can be negative).");
    }
    if(degrees == 0L) {
        return(slice);
    } else if(degrees == 270) {
        return(rotate90(slice, times=1L, clockwise=FALSE));
    } else if(degrees == 180) {
        return(rotate90(slice, times=2L));
    } else {  # 90
        return(rotate90(slice));
    }
}


#' @title Rotate 2D matrix clockwise in 90 degree steps.
#'
#' @param mtx a 2D matrix
#'
#' @param times integer, how often to rotate in 90 degree steps. Example: pass `3L` to rotate `270` degrees.
#'
#' @param clockwise logical, whether to rotate clockwise.
#'
#' @keywords internal
rotate90 <- function(mtx, times=1L, clockwise=TRUE) {
    for(i in seq_len(times)) {
        if(clockwise) {
            mtx = t(apply(mtx, 2, rev));
        } else {
            mtx = apply(t(mtx), 2, rev);
        }
    }
    return(mtx);
}

#' @title Flip a 2D matrix.
#'
#' @param slice a 2D matrix
#'
#' @param how character string, one of 'vertically' or 'horizontally'. Note that flipping *horizontally* means that the image will be mirrored along the central *vertical* axis. If `NULL` is passed, the passed value is returned unaltered.
#'
#' @return 2D matrix, the flipped matrix.
#'
#' @export
flip2D <- function(slice, how='horizontally') {
    if(is.null(how)) {
        return(slice);
    }

    if(how == 'vertically') {
        axis = 2L;
    } else if(how == 'horizontally') {
        axis = 1L;
    } else {
        stop("How must be one of 'vertically' or 'horizontally' (or NULL for noop).");
    }

    axis = as.integer(axis);
    if(axis < 1L | axis > 2L) {
        stop(sprintf("Axis must be integer with value 1 or 2 but is %d.\n", axis));
    }

    if(length(dim(slice)) != 2L) {
        stop("Slice must be a 2D matrix.");
    }
    if(axis == 1L) {
        return(slice[nrow(slice):1,]);
    } else {
        return(slice[, ncol(slice):1]);
    }
}


#' @title Find files with the given base name and extensions that exist.
#'
#' @description Note that in the current implementation, the case of the filepath and the extension must match.
#'
#' @param filepath character string, path to a file without extension
#'
#' @param precedence vector of character strings, the file extensions to check. Must include the dot (if you expect one).
#'
#' @param error_if_none logical, whether to raise an error if none of the files exist
#'
#' @param return_all logical, whether to return all readable files instead of just the first one
#'
#' @return character string, the path to the first existing file (or NULL if none of them exists).
#'
#' @keywords internal
readable.files <- function(filepath, precedence=c('.mgh', '.mgz'), error_if_none=TRUE, return_all=FALSE) {
    candidate_files = paste(filepath, precedence, sep='');
    readable_files = c();
    for(cfile in candidate_files) {
        if(file.exists(cfile)) {
            if(return_all) {
                readable_files = c(readable_files, cfile);
            } else {
                return(cfile);
            }
        }
    }

    if(length(readable_files) == 0 & error_if_none) {
        stop(sprintf("At location '%s' exists no file with any of the %d extensions '%s'.\n", filepath, length(precedence), paste(precedence, collapse=' ')));
    } else {
        return(readable_files);
    }
}


