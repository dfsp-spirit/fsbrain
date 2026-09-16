# Renderable class for line segments, e.g. the edges of a connectome.
#
# This is the counterpart of 'fs.coloredmesh' for geometry that has no surface:
# a coloredmesh needs triangles to exist, so an edge (a single line segment) has
# no representation as one. Like 'fs.coloredmesh', an 'fs.coloredpaths'
# instance is understood by both renderer backends: the rgl backend draws the
# segments with rgl::segments3d() (hardware lines), the headless scimesh
# backend passes them to the scimesh line layer (see scimesh::line_layer()),
# which rasterizes them without creating any geometry. Both backends can
# therefore render thousands of edges cheaply.
#
# Instances are usually created by higher-level functions like
# vis.connectome(), but they can be built manually with fs.coloredpaths().


#' @title Create fs.coloredpaths instance from 2 point matrices.
#'
#' @description Create a renderable that draws the line segments from `from` to `to`, with one color and (optionally) one width per segment. This is the line equivalent of a coloredmesh, see the details.
#'
#' @details Both renderer backends support this class. The rgl backend renders the segments as hardware lines (rgl::segments3d), the headless scimesh backend uses a scimesh line layer (see scimesh::line_layer), which rasterizes the lines directly instead of creating tube geometry. Note that the width of lines is measured in pixels and is a screen-space property, so lines do not get thinner when the camera moves away (this is what hardware line rendering does, and it also means that a line is always visible, even when it is very thin).
#'
#' The `metadata` field works exactly like the one of a coloredmesh: if it contains the entries 'src_data' and 'makecmap_options', a colorbar can be plotted for this renderable, see \code{\link[fsbrain]{coloredmesh.plot.colorbar.separate}}.
#'
#' @param from numeric matrix of size (n, 3), the start points of the n segments.
#'
#' @param to numeric matrix of size (n, 3), the end points of the n segments.
#'
#' @param col vector of hex color strings, either a single one or one per segment.
#'
#' @param width vector of positive numbers or a single one, the line width(s) in pixels.
#'
#' @param depth_test logical, whether the lines should be hidden by geometry which is closer to the camera. Defaults to TRUE. Set to FALSE to draw the lines on top of everything, which is useful for annotations.
#'
#' @param lit logical, whether to apply lighting to the lines. Defaults to FALSE, which means the color is used as-is (like hardware-rendered lines, and like \code{\link[rgl]{segments3d}} without a lit material).
#'
#' @param hemi character string or NULL, the hemisphere this renderable belongs to. Defaults to NULL, which means that it is not hemisphere-specific and is rendered in the views of both hemispheres.
#'
#' @param metadata named list, metadata for this renderable. See the details.
#'
#' @param render logical, whether to render this instance. Meshes with `render=FALSE` are skipped by all rendering functions, see the parameter 'skip_all_na' of \code{\link[fsbrain]{vis.coloredmeshes}}.
#'
#' @param style `NULL` or a rendering style for this renderable, see \code{\link[fsbrain]{get.rglstyle}}.
#'
#' @return fs.coloredpaths instance. A named list with entries: "from" (the start points), "to" (the end points), "col" (the colors), "width" (the line widths), "depth_test", "lit", "hemi", "render", and "metadata".
#'
#' @family coloredpaths functions
#'
#' @examples
#' # A single white line segment from the origin to (10, 0, 0):
#' p = fs.coloredpaths(matrix(c(0, 0, 0), ncol = 3), matrix(c(10, 0, 0), ncol = 3), col = "#FFFFFF");
#' p;
#'
#' # Two segments in different colors, both of them with a width of 2 pixels:
#' from = matrix(c(0, 0, 0, 0, 0, 10), ncol = 3, byrow = TRUE);
#' to = matrix(c(10, 0, 0, 0, 10, 10), ncol = 3, byrow = TRUE);
#' p2 = fs.coloredpaths(from, to, col = c("#FF0000", "#00FF00"), width = 2);
#'
#' @export
fs.coloredpaths <- function(from, to, col = "#FF0000", width = 1.0, depth_test = TRUE, lit = FALSE, hemi = NULL, metadata = list(), render = TRUE, style = NULL) {

    from = check.segment.points(from, 'from');
    to = check.segment.points(to, 'to');

    if(nrow(from) != nrow(to)) {
        stop(sprintf("Parameters 'from' and 'to' must have the same number of rows, but they have %d and %d.\n", nrow(from), nrow(to)));
    }
    num_segments = nrow(from);

    col = recycle(col, num_segments);
    if(! is.character(col)) {
        stop("Parameter 'col' must be a character vector of hex color strings, a single one or one per segment.\n");
    }
    if(length(col) != num_segments) {
        stop(sprintf("Parameter 'col' must have length 1 or %d (one color per segment), but it has length %d.\n", num_segments, length(col)));
    }

    if(! is.numeric(width) || length(width) < 1L || any(! is.finite(width)) || any(width <= 0.0)) {
        stop("Parameter 'width' must be a positive number, or a vector of positive numbers (one per segment).\n");
    }
    if(length(width) == 1L) {
        width = rep(as.double(width), num_segments);
    }
    if(length(width) != num_segments) {
        stop(sprintf("Parameter 'width' must have length 1 or %d (one width per segment), but it has length %d.\n", num_segments, length(width)));
    }

    if(! (is.logical(depth_test) && length(depth_test) == 1L)) {
        stop("Parameter 'depth_test' must be a single logical value.\n");
    }
    if(! (is.logical(lit) && length(lit) == 1L)) {
        stop("Parameter 'lit' must be a single logical value.\n");
    }

    if(! is.null(hemi) && ! (is.character(hemi) && length(hemi) == 1L && hemi %in% c("lh", "rh", "both"))) {
        stop("Parameter 'hemi' must be NULL, or one of 'lh', 'rh' or 'both'.\n");
    }

    cp = list("from"=from, "to"=to, "col"=col, "width"=width, "depth_test"=depth_test, "lit"=lit, "hemi"=hemi, "metadata"=metadata, "render"=render);
    if(! is.null(style)) {
        cp$style = style;
    }
    class(cp) = c("fs.coloredpaths", class(cp));
    return(cp);
}


#' @title Check whether a matrix is a valid point matrix for coloredpaths.
#'
#' @param x the object to check.
#'
#' @param arg_name character string, the name of the parameter, used in error messages.
#'
#' @return an (n, 3) numeric matrix.
#'
#' @keywords internal
check.segment.points <- function(x, arg_name) {
    if(is.null(x)) {
        stop(sprintf("Parameter '%s' must be an (n, 3) numeric matrix of point coordinates, but it is NULL.\n", arg_name));
    }
    if(is.vector(x) && is.numeric(x)) {
        if(length(x) == 3L) {
            return(matrix(x, ncol = 3L));
        }
        stop(sprintf("Parameter '%s' must be an (n, 3) numeric matrix of point coordinates, or a vector of length 3 for a single point.\n", arg_name));
    }
    if(! is.matrix(x) || ! is.numeric(x) || ncol(x) != 3L) {
        stop(sprintf("Parameter '%s' must be an (n, 3) numeric matrix of point coordinates.\n", arg_name));
    }
    return(x);
}


#' @title Check whether object is an fs.coloredpaths instance (S3)
#'
#' @param x any `R` object
#'
#' @return TRUE if its argument is an fs.coloredpaths instance (that is, has "fs.coloredpaths" among its classes) and FALSE otherwise.
#'
#' @export
is.fs.coloredpaths <- function(x) inherits(x, "fs.coloredpaths")


#' @title Print description of an fs.coloredpaths instance (S3).
#'
#' @param x an fs.coloredpaths instance.
#'
#' @param ... further arguments passed to or from other methods.
#'
#' @export
print.fs.coloredpaths <- function(x, ...) {
    cat(sprintf("Brain coloredpaths with %d segment(s).\n", nrow(x$from)));
    cat(sprintf("  widths: %s\n", paste(unique(x$width), collapse = ", ")));
    cat(sprintf("  colors: %s\n", paste(unique(x$col), collapse = ", ")));
    cat(sprintf("  depth test: %s, lit: %s, render: %s\n", as.character(x$depth_test), as.character(x$lit), as.character(x$render)));
    if(length(x$metadata) > 0L) {
        cat(sprintf("  metadata keys: %s\n", paste(names(x$metadata), collapse = ", ")));
    }
    return(invisible(x));
}


#' @title Get the number of segments of an fs.coloredpaths instance.
#'
#' @param x an fs.coloredpaths instance.
#'
#' @return positive integer, the number of segments.
#'
#' @keywords internal
coloredpaths.length <- function(x) {
    if(! is.fs.coloredpaths(x)) {
        stop("Parameter 'x' must be an fs.coloredpaths instance.\n");
    }
    return(nrow(x$from));
}
