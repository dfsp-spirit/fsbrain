# Functions for computing approx geodesic distance on brain meshes using Rvcg.
# IMPORTANT: This requires Rvcg from Github, as the CRAN version does not include the
# required functions.


#' @title Compute all vertices within given geodesic distance on the mesh.
#'
#' @param mesh an instance of \code{rgl::tmesh3d} or \code{freesurferformats::fs.surface}.
#'
#' @param vertex positive integer (or vector of the latter), the index of the source vertex in the mesh. If a vector, the neighborhoods for all vertices will be computed separately.
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
        neighborhood = vertex;
        for (v in vertex) {
            geodesic_dists_to_vertex = geodesic.dists.to.vertex(mesh, v);
            if(include_max) {
                neighborhood = c(neighborhood, which(geodesic_dists_to_vertex <= max_distance));
            } else {
                neighborhood = c(neighborhood, which(geodesic_dists_to_vertex < max_distance));
            }
        }
        return(unique(neighborhood));
    } else {
        stop("The 'Rvcg' package must be installed to use this functionality.");
    }
}


#' @title Generate color overlay from geodesic patches around several vertices.
#'
#' @description Works across hemispheres (for a whole brain) if you pass a hemilist of meshes as parameter 'mesh', see below.
#'
#' @inheritParams geod.vert.neighborhood
#'
#' @param mesh a single \code{fs.surface} instance, or a hemilist of two such meshes. If a hemilist, the vertex indices can go from 1 to the sum of vertices in both meshes, and the proper hemisphere will be used automatically.
#'
#' @param color single color string like \code{'#FF0000'} or vector of such strings. If a vector, the length should match the number of vertices in parameter 'vertex'.
#'
#' @param ... extra arguments passed to \code{geod.vert.neighborhood}.
#'
#' @return vector of color strings (or a hemilist of 2 such vectors if 'mesh' is a hemilist), an overlay suitable for visualization using \code{vis.color.on.subject}.
#'
#' @export
geod.patches.color.overlay <- function(mesh, vertex, color = "#FF0000", bg_color = "#FEFEFE", ...) {
    color = recycle(color, length(vertex));
    if(is.hemilist(mesh)) {
        if(! is.fs.surface(mesh$lh)) {
            stop("Paramter 'mesh$lh' must be an fs.surface instance if a hemilist is passed.");
        }
        if(! is.fs.surface(mesh$rh)) {
            stop("Paramter 'mesh$rh' must be an fs.surface instance if a hemilist is passed.");
        }
        lh_nv = nrow(mesh$lh$vertices);
        rh_nv = nrow(mesh$rh$vertices);
        lh_vertex = vertex[which(vertex <= lh_nv)];
        lh_color = color[which(vertex <= lh_nv)];
        rh_vertex = vertex[which(vertex > lh_nv)];
        rh_vertex = rh_vertex - lh_nv;
        rh_color = color[which(vertex > lh_nv)];

        lh_overlay = geod.patches.color.overlay.singlehemi(mesh$lh, lh_vertex, lh_color, bg_color = bg_color, ...);
        rh_overlay = geod.patches.color.overlay.singlehemi(mesh$rh, rh_vertex, rh_color, bg_color = bg_color, ...);
        res = list('lh'=lh_overlay, 'rh'=rh_overlay);
        return(res);
    } else {
        return(geod.patches.color.overlay.singlehemi(mesh, vertex, color, bg_color = bg_color, ...));
    }
}


#' @keywords internal
geod.patches.color.overlay.singlehemi <- function(mesh, vertex, color = "#FF0000", bg_color = "#FEFEFE", ...) {
    if(! is.fs.surface(mesh)) {
        stop("Paramter 'mesh' must be an fs.surface instance (or hemilist of such).");
    }
    color = recycle(color, length(vertex));
    nv = nrow(mesh$vertices);
    color_overlay = rep(bg_color, nv);

    query_v_idx = 1L;
    for (v in vertex) {
        neighborhood = geod.vert.neighborhood(mesh, vertex, ...);
        color_overlay[neighborhood] = color[query_v_idx];
        query_v_idx = query_v_idx + 1L;
    }
    return(color_overlay);
}


#' @title Simple internal wrapper around \code{Rvcg::vcgDijkstra} with function check.
#'
#' @note This can be remove once the required Rvcg version is on CRAN and properly listed in the DESCRIPTION file.
#'
#' @keywords internal
geodesic.dists.to.vertex <- function(mesh, v) {
    if(! exists('vcgDijkstra', where=asNamespace('Rvcg'), mode='function')) {
        stop("Your Rvcg version does not export the vcgDijkstra function, which means it is too old. You need to install Rvcg from GitHub for this this functionality to be available. Try 'devtools::install_github('zarquon42b/Rvcg')'.");
    } else {
        return(Rvcg::vcgDijkstra(mesh, v));
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
