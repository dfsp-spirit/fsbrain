#' @keywords internal
rad2deg <- function(rad) {(rad * 180) / (pi)}

#' @keywords internal
deg2rad <- function(deg) {(deg * pi) / (180)}


#' @title Get rotation matrix for a 3D rotation around an axis.
#'
#' @param angle_rad doule, the angle in radians
#'
#' @param x rotation axis
#'
#' @param y rotation axis
#'
#' @param z rotation axis
#'
#' @param with_trans logical, whether to extend the 3x3 rotation matrix to a 4x4 rotation and tranlsation matrix.
#'
#' @return a 3x3 or 4x4 double matrix
#'
#' @keywords internal
rotation.matrix.for.axis.rot <- function(angle_rad, x, y, z, with_trans=TRUE) {
    x = as.integer(x);
    y = as.integer(y);
    z = as.integer(z);
    if(sum(abs(c(x,y,z))) != 1L) {
        stop("Parameters x,y,z as abs must sum to 1 (multi-axis rotations not supported).");
    }
    a = angle_rad;
    by_row = FALSE;
    if(abs(x) == 1L) {
        m = matrix(c(1, 0, 0, 0, cos(a), -sin(a), 0, sin(a), cos(a)), ncol = 3, byrow = by_row);
    } else if(abs(y) == 1) {
        m = matrix(c(cos(a), 0, sin(a), 0, 1, 0, -sin(a), 0, cos(a)), ncol = 3, byrow = by_row);
    } else {
        m = matrix(c(cos(a), -sin(a), 0, sin(a), cos(a), 0, 0, 0, 1), ncol = 3, byrow = by_row);
    }
    if(with_trans) {
        m2 = matrix(rep(0, 16), ncol = 4);
        m2[4,4] = 1;
        m2[1:3,1:3] = m;
        return(m2);
    } else {
        return(m);
    }
}
