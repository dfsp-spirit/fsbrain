# Functions for computing approx geodesic distance on brain meshes using Rvcg.
# IMPORTANT: This requires Rvcg from Github, as the CRAN version does not include the
# required functions.


#' @title Compute all vertices within given geodesic distance on the mesh.
#'
#' @param mesh an instance of \code{rgl::tmesh3d} or \code{freesurferformats::fs.surface}.
#'
#' @param vertex positive integer, the index of the source vertex in the mesh.
#'
#' @param max_distance double, the neighborhood size. All mesh vertices in geodesic distance smaller than / up to this distance will be returned.
#'
#' @param include_max logical, whether the max_distance value is inclusive.
#'
#' @return integer vector, the indices of all vertices in the neigborhood.
#'
#' @note This function uses the pseudo-geodesic distance along the mesh edges.
#'
#' @export
geod.vert.neighborhood <- function(mesh, vertex, max_distance=5.0, include_max = TRUE) {
    mesh = ensure.tmesh3d(mesh);
    if(requireNamespace("Rvcg", quietly = TRUE)) {
        if(! exists('vcgDijkstra', where=asNamespace('Rvcg'), mode='function')) {
            stop("Your Rvcg version does not export the vcgDijkstra function, which means it is too old. You need to install Rvcg from GitHub for this this functionality to be available. Try 'devtools::install_github('zarquon42b/Rvcg')'.");
        }
        geodesic_dists_to_vertex = Rvcg::vcgDijkstra(lh_tmesh3d, vertex);
        if(include_max) {
            return(which(geodesic_dists_to_vertex <= max_distance));
        } else {
            return(which(geodesic_dists_to_vertex < max_distance));
        }
    } else {
        stop("The 'Rvcg' package must be installed to use this functionality.");
    }
}


#' @title Ensure the mesh is a tmesh3d instance. Will convert fs.surfaces to one automatically.
#'
#' @param mesh whatever, but hopefully an \code{rgl::tmesh3d} or \code{freesurferformats::fs.surface} instance.
#'
#' @return tmesh3d instance, the input or converted from the input.
#'
#' @note This function will stop if the mesh cannot be converted to tmesh3d.
#'
#' @keywords internal
ensure.tmesh3d <- function(mesh) {
    if(freesurferformats::is.fs.surface(mesh)) {
        return(fs.surface.to.tmesh3d(mesh));
    } else if ("mesh3d" %in% class(mesh)) {
        return(mesh);
    } else {
        stop("Cannot convert value in parameter 'mesh' to tmesh3d instance, invalid mesh.");
    }
}
