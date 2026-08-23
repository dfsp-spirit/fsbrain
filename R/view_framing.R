# Shared plain-R camera/framing math for the rgl and scimesh renderer
# backends. Keeping this in fsbrain (instead of inside either renderer)
# means both backends frame and orient a view identically.
# See dev_tools/TODO_FSBRAIN_RGL_CAM.md (Step 2) and scimesh_bridge.R.


#' @title Compute the bounding sphere of a set of 3D vertices.
#'
#' @description Computes the sphere that encloses the axis-aligned bounding box
#'   (AABB) of the given vertices, using the same convention as rgl's
#'   \code{Sphere(AABB)} auto-framing (see rgl src/geom.cpp) and scimesh's
#'   \code{camera_auto(rgl_compat=TRUE)}: the center is the AABB center and the
#'   radius is half the length of the AABB diagonal.
#'
#' @param vertices an Nx3 numeric matrix of vertex coordinates, or a
#'   \code{mesh3d}/\code{tmesh3d} object, or an \code{fs.surface}, or a list of
#'   any of these (all vertices are pooled).
#'
#' @return a list with entries \code{center} (numeric vector of length 3) and
#'   \code{radius} (numeric scalar). For an empty/zero-extent input, \code{radius}
#'   is 0 and the center is the single vertex (or \code{NA} if no vertices).
#'
#' @keywords internal
bounding_sphere <- function(vertices) {
    # Convert a single vertex source to an Nx3 matrix.
    to_mat <- function(v) {
        if (is.matrix(v)) {
            if (ncol(v) != 3L) {
                stop("Vertex matrix must have 3 columns (x, y, z).")
            }
            return(v)
        }
        if (inherits(v, "mesh3d")) {
            return(t(v$vb[1:3, , drop = FALSE]))
        }
        if (is.list(v) && !is.null(v$vertices)) {
            # fs.surface or mesh descriptor.
            return(as.matrix(v$vertices))
        }
        if (is.list(v) && !is.null(v$mesh) && inherits(v$mesh, "mesh3d")) {
            # fs.coloredmesh.
            return(t(v$mesh$vb[1:3, , drop = FALSE]))
        }
        stop("Unsupported vertex source for bounding_sphere(): must be an Nx3 matrix, mesh3d, fs.surface, or a list of these.")
    }

    is_pool <- is.list(vertices) && !inherits(vertices, "mesh3d") &&
        (is.null(vertices$vertices) || !is.matrix(vertices$vertices))

    if (is_pool) {
        # List of vertex sources -> pool all vertices.
        vertices <- do.call(rbind, lapply(vertices, to_mat))
    } else {
        vertices <- to_mat(vertices)
    }

    if (nrow(vertices) == 0L) {
        return(list(center = c(NA_real_, NA_real_, NA_real_), radius = 0.0))
    }

    vmin <- apply(vertices, 2L, min)
    vmax <- apply(vertices, 2L, max)
    center <- unname((vmin + vmax) / 2.0)
    radius <- sqrt(sum((vmax - vmin)^2)) / 2.0
    return(list(center = center, radius = radius))
}
