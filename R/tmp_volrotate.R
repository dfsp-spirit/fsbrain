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

