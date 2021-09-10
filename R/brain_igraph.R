

#' @title Create igraph undirected graph from a brain surface mesh.
#'
#' @param surface an fs.surface instance as returned by \code{subject.surface}, an existing igraph (which will be returned as-is) or a string which is interpreted as a path to a surface file.
#'
#' @return igraph::graph instance
#'
#' @examples
#' \dontrun{
#'   # Find the one-ring neighbors of vertex 15 on the fsaverage left hemi:
#'   sf = subject.surface(fsaverage.path(T), "fsaverage", "white", "lh");
#'   g = fs.surface.to.igraph(sf);
#'   igraph::neighborhood(g, order = 1, nodes = 15);
#' }
#'
#' @export
fs.surface.to.igraph <- function(surface) {
    if(requireNamespace("igraph", quietly = TRUE)) {
        if(igraph::is.igraph(surface)) {
            return(surface);
        }
        surface = ensure.fs.surface(surface);
        el = rbind(surface$faces[,1:2], surface$faces[,2:3])
        return(igraph::graph_from_edgelist(el, directed = FALSE));
    } else {
        stop("This functionality requires the 'igraph' package to be installed.");
    }
}

#' @title Compute vertex neighborhoods or the full adjacency list for a mesh using the Rvcg or igraph library.
#'
#' @inheritParams fs.surface.to.igraph
#'
#' @description This is a faster replacement for \code{mesh.vertex.neighbors} that requires the optional dependency package 'igraph' or 'Rvcg'.
#'
#' @param nodes the source vertex. Passed on to \code{igraph::neighborhood}. Can be a vector, in which case the neighborhoods for all these vertices are computed separately. If NULL, all graph vertices are used.
#'
#' @param order integer, the max graph distance of vertices to consider neighbors (number of neighborhood rings). Passed on to \code{igraph::neighborhood}
#'
#' @param simplify logical, whether to return only an integer vector if the 'nodes' parameter has length 1 (instead of a list where the first element is such a vector).
#'
#' @param include_self logical, whether to include vertices in their own neighborhood
#'
#' @note If you intend to call several functions on the igraph, it is faster to construct it with \code{fs.surface.to.igraph} and keep it.
#'
#' @seealso The \code{fs.surface.as.adjacencylist} function computes the 1-ring neighborhood for the whole graph.
#'
#' @return named list of integer vectors (see \code{igraph::neighborhood}), unless 'simplify' is TRUE, see there for details.
#'
#' @export
fs.surface.vertex.neighbors <- function(surface, nodes = NULL, order = 1L, simplify = TRUE, include_self = FALSE) {

    surface = ensure.fs.surface(surface);
    if(is.null(nodes)) {
        nodes = seq(nrow(surface$vertices));
    }

    if(requireNamespace("Rvcg", quietly = TRUE)) {
        if(exists('vcgVertexNeighbors', where=asNamespace('Rvcg'), mode='function')) {
            tmesh = ensure.tmesh3d(surface);
            neigh = Rvcg::vcgVertexNeighbors(tmesh, vi = nodes, numstep = order, include_self = include_self);
            if(simplify && length(nodes) == 1L) {
                return(unlist(neigh));
            } else {
                return(neigh);
            }
        }
    }
    if(requireNamespace("igraph", quietly = TRUE)) {
        g = fs.surface.to.igraph(surface);
        res = igraph::neighborhood(g, order = order, nodes = nodes, mode = "all");
        if(simplify && length(nodes) == 1L) {
            res = as.integer(unlist(res));
            if(include_self) {
                res = c(res, nodes);
            }
            return(res);
        }
        res = lapply(res, as.integer);
        if(include_self) {
            for(nodeidx in seq_along(nodes)) {
                res[[nodeidx]] = c(res[[nodeidx]], nodes[nodeidx]);
            }
        }
        return(res);
    } else {
        stop("This functionality requires the 'igraph' package to be installed.");
    }
}


#' @title Turn surface mesh into a igraph and return its adjacency list representation.
#'
#' @inheritParams fs.surface.to.igraph
#'
#' @return list of integer vectors, the adjacency list.
#'
#' @export
fs.surface.as.adjacencylist <- function(surface) {
    if(requireNamespace("Rvcg", quietly = TRUE)) {
        if(exists('vcgVertexNeighbors', where=asNamespace('Rvcg'), mode='function')) {
            tmesh = ensure.tmesh3d(surface);
            return(Rvcg::vcgVertexNeighbors(tmesh));
        }
    }
    if(requireNamespace("igraph", quietly = TRUE)) {
        g = fs.surface.to.igraph(surface);
        return(lapply(igraph::as_adj_list(g), as.integer));
    } else {
        stop("This functionality requires the 'Rvcg' or 'igraph' package to be installed.");
    }
}


#' @title Check whether parameter is an fs.surface instance.
#'
#' @param surface an fs.surface instance which will be returned as-is, a tmesh3d which will be converted to a surface using \code{\link[fsbrain]{tmesh3d.to.fs.surface}}, or a character string which will be interpreted as a file system path and loaded with \code{freesurferformats::read.fs.surface}. Anything else will stop with an error.
#'
#' @return an fs.surface instance, unless an error occurs.
#'
#' @keywords internal
ensure.fs.surface <- function(surface) {
    if(freesurferformats::is.fs.surface(surface)) {
        return(surface);
    } else if(is.character(surface)) {
        return(freesurferformats::read.fs.surface(surface));
    } else if('mesh3d' %in% class(surface)) {
        return(tmesh3d.to.fs.surface(surface));
    } else {
        stop("Parameter 'surface' must be an fs.surface instance or a character string.");
    }
}

