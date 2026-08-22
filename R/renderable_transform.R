# Plain-R geometry transforms shared by the rgl and scimesh renderer backends.
# Keeping these in fsbrain (instead of calling rgl::rotate3d or
# scimesh::translate_mesh) means both backends transform geometry identically.
# See TODO_FSBRAIN_RGL_CAM.md.

#' @title Build a 4x4 rotation matrix (Rodrigues formula).
#'
#' @param angle_rad numeric scalar, the rotation angle in radians.
#'
#' @param x numeric, x component of the rotation axis.
#'
#' @param y numeric, y component of the rotation axis.
#'
#' @param z numeric, z component of the rotation axis.
#'
#' @return 4x4 numeric rotation matrix in homogeneous coordinates. Matches the
#'   convention of \code{\link[rgl]{rotate3d}} / \code{\link[rgl]{rotationMatrix}}.
#'
#' @keywords internal
rotation.matrix <- function(angle_rad, x, y, z) {
    axis <- c(x, y, z);
    n <- sqrt(sum(axis^2));
    if(n == 0) {
        stop("Rotation axis must be non-zero.");
    }
    axis <- axis / n;
    c0 <- cos(angle_rad);
    s0 <- sin(angle_rad);
    C1 <- 1 - c0;
    ux <- axis[1]; uy <- axis[2]; uz <- axis[3];

    R <- matrix(0, nrow = 4L, ncol = 4L);
    R[1:3, 1:3] <- rbind(
        c(c0 + ux*ux*C1,      ux*uy*C1 - uz*s0,  ux*uz*C1 + uy*s0),
        c(uy*ux*C1 + uz*s0,   c0 + uy*uy*C1,     uy*uz*C1 - ux*s0),
        c(uz*ux*C1 - uy*s0,   uz*uy*C1 + ux*s0,  c0 + uz*uz*C1)
    );
    R[4L, 4L] <- 1;
    return(R);
}


#' @title Apply a 4x4 homogeneous transform to a matrix of 3D points.
#'
#' @param coords Nx3 numeric matrix of coordinates.
#'
#' @param matrix 4x4 numeric transform matrix.
#'
#' @return Nx3 numeric matrix of transformed coordinates.
#'
#' @noRd
transform_coords <- function(coords, matrix) {
    if(is.null(coords)) {
        return(coords);
    }
    if(! is.matrix(coords)) {
        coords <- matrix(coords, ncol = 3L);
    }
    if(ncol(coords) != 3L) {
        stop("coords must be an Nx3 matrix.");
    }
    R <- matrix[1:3, 1:3, drop = FALSE];
    tvec <- matrix[1:3, 4L];
    n <- nrow(coords);
    result <- coords %*% R;
    if(n > 0L) {
        result <- result + matrix(tvec, nrow = n, ncol = 3L, byrow = TRUE);
    }
    return(result);
}


#' @title Apply a 4x4 transform to a renderable (shared rgl/scimesh geometry).
#'
#' @description Rotates/translates the geometry of a renderable in plain R,
#' rotating vertex normals as well (rigid rotation), matching
#' \code{\link[rgl]{rotate3d}}.
#'
#' @param x a renderable: \code{fs.coloredmesh}, \code{fs.coloredvoxels},
#'   \code{Triangles3D}, a \code{mesh3d}/\code{tmesh3d}, or an Nx3 matrix of
#'   coordinates.
#'
#' @param matrix 4x4 numeric transform matrix.
#'
#' @param ... ignored.
#'
#' @return a transformed copy of \code{x}.
#'
#' @noRd
transform_renderable <- function(x, matrix, ...) {
    if(is.fs.coloredmesh(x)) {
        x$mesh <- transform_renderable(x$mesh, matrix);
        return(x);
    }
    if(is.fs.coloredvoxels(x)) {
        x$voxeltris <- transform_renderable(x$voxeltris, matrix);
        return(x);
    }
    if(is.Triangles3D(x)) {
        x$v1 <- transform_coords(x$v1, matrix);
        x$v2 <- transform_coords(x$v2, matrix);
        x$v3 <- transform_coords(x$v3, matrix);
        return(x);
    }
    if(is.matrix(x) || is.numeric(x)) {
        return(transform_coords(x, matrix));
    }
    if(inherits(x, "mesh3d") || (is.list(x) && ! is.null(x$vb))) {
        R <- matrix[1:3, 1:3, drop = FALSE];
        tR <- t(R);
        tvec <- matrix[1:3, 4L];
        if(! is.null(x$vb)) {
            x$vb[1:3, ] <- tR %*% x$vb[1:3, , drop = FALSE];
            if(any(tvec != 0)) {
                x$vb[1:3, ] <- x$vb[1:3, ] + tvec;
            }
        }
        if(! is.null(x$normals)) {
            nrm <- x$normals;
            if(nrow(nrm) >= 3L) {
                nrm[1:3, ] <- tR %*% nrm[1:3, , drop = FALSE];
                x$normals <- nrm;
            }
        }
        return(x);
    }
    stop(sprintf("transform_renderable not supported for object of class '%s'.", paste(class(x), collapse = " ")));
}
