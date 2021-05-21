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
#' @param return_distances logical, whether to compute the 'distances' entry in the returned list. Doing so is a little bit slower, so it can be turned off if not needed.
#'
#' @return named list with the following entries: 'vertices': integer vector, the indices of all vertices in the neigborhood. 'distances': double vector, the distances to the respective vertices (unless 'return_distances' is FALSE).
#'
#' @note This function uses the pseudo-geodesic distance along the mesh edges.
#'
#' @export
geod.vert.neighborhood <- function(mesh, vertex, max_distance=5.0, include_max = TRUE, return_distances = TRUE) {
    mesh = ensure.tmesh3d(mesh);
    if(requireNamespace("Rvcg", quietly = TRUE)) {
        neighborhood = vertex;
        neighborhood_distances = rep(0.0, length(vertex));
        for (v in vertex) {
            geodesic_dists_to_vertex = geodesic.dists.to.vertex(mesh, v);
            if(include_max) {
                this_neighborhood_indices = which(geodesic_dists_to_vertex <= max_distance);
            } else {
                this_neighborhood_indices = which(geodesic_dists_to_vertex < max_distance);
            }
            neighborhood = c(neighborhood, this_neighborhood_indices);
            neighborhood_distances = c(neighborhood_distances, geodesic_dists_to_vertex[this_neighborhood_indices]);
        }

        if(! return_distances) {
            return(list("vertices" = unique(neighborhood)));
        } else {
            # Make neighborhood unique, and also remove the corresponding duplicated distances (so we cannot simply use base::unique).
            verts_unique = c();
            dists_unique = c();
            for(neigh_idx in seq_len(length(neighborhood))) {
                vert_idx = neighborhood[neigh_idx];
                if(! (vert_idx %in% verts_unique)) {
                    verts_unique = c(verts_unique, vert_idx);
                    dists_unique = c(dists_unique, neighborhood_distances[neigh_idx]);
                }
            }
            return(list('vertices' = verts_unique, 'distances' = dists_unique));
        }
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

#' @title Generate color overlay from geodesic patches around several vertices for a single hemi.
#'
#' @inheritParams geod.patches.color.overlay
#'
#' @param mesh a single \code{fs.surface} instance.
#'
#' @keywords internal
geod.patches.color.overlay.singlehemi <- function(mesh, vertex, color = "#FF0000", bg_color = "#FEFEFE", ...) {
    if(! is.fs.surface(mesh)) {
        stop("Parameter 'mesh' must be an fs.surface instance.");
    }
    color = recycle(color, length(vertex));
    nv = nrow(mesh$vertices);
    color_overlay = rep(bg_color, nv);

    query_v_idx = 1L;
    for (v in vertex) {
        neighborhood = geod.vert.neighborhood(mesh, v, ...)$vertices;
        color_overlay[neighborhood] = color[query_v_idx];
        query_v_idx = query_v_idx + 1L;
    }
    return(color_overlay);
}

#' @title Generate per-vertex distance data from geodesic patches around several vertices.
#'
#' @description Works across hemispheres (for a whole brain) if you pass a hemilist of meshes as parameter 'mesh', see below.
#'
#' @inheritParams geod.patches.color.overlay
#'
#' @return vector of doubles (or a hemilist of 2 such vectors if 'mesh' is a hemilist), the per-vertex distance data. Data for vertices outside neighborhoods will be NA.
#'
#' @export
geod.patches.pervertexdata <- function(mesh, vertex, ...) {
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
        rh_vertex = vertex[which(vertex > lh_nv)];
        rh_vertex = rh_vertex - lh_nv;

        lh_overlay = geod.patches.pervertexdata.singlehemi(mesh$lh, lh_vertex, ...);
        rh_overlay = geod.patches.pervertexdata.singlehemi(mesh$rh, rh_vertex, ...);
        res = list('lh'=lh_overlay, 'rh'=rh_overlay);
        return(res);
    } else {
        return(geod.patches.pervertexdata.singlehemi(mesh, vertex, ...));
    }
}


#' @title Get vertex data for a single fs.surface or a hemilist of surfaces.
#' @keywords internal
constant.pervertexdata <- function(surfaces, value = NA) {
    mesh = surfaces;
    if(is.hemilist(mesh)) {
        if(! is.fs.surface(mesh$lh)) {
            stop("Paramter 'mesh$lh' must be an fs.surface instance if a hemilist is passed.");
        }
        if(! is.fs.surface(mesh$rh)) {
            stop("Paramter 'mesh$rh' must be an fs.surface instance if a hemilist is passed.");
        }
        lh_nv = nrow(mesh$lh$vertices);
        rh_nv = nrow(mesh$rh$vertices);
        return(list('lh'=rep(value, lh_nv), 'rh'=rep(value, rh_nv)));
    } else {
        if(! is.fs.surface(mesh)) {
            stop("Paramter 'mesh' must be an fs.surface instance (unless a hemilist is passed).");
        } else {
            return(rep(value, nrow(mesh$vertices)));
        }
    }
}



#' @title Generate per-vertex distance data from geodesic patches around several vertices for a single hemi.
#'
#' @inheritParams geod.patches.pervertexdata
#'
#' @param mesh a single \code{fs.surface} instance.
#'
#' @seealso geod.patches.pervertexdata
#'
#' @keywords internal
geod.patches.pervertexdata.singlehemi <- function(mesh, vertex, ...) {
    if(! is.fs.surface(mesh)) {
        stop("Paramter 'mesh' must be an fs.surface instance.");
    }
    nv = nrow(mesh$vertices);
    data_overlay = rep(NA, nv);

    query_v_idx = 1L;
    for (v in vertex) {
        neighborhood = geod.vert.neighborhood(mesh, v, ...);
        data_overlay[neighborhood$vertices] = neighborhood$distances;
        query_v_idx = query_v_idx + 1L;
    }
    return(data_overlay);
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
