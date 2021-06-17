

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

#' @title Compute vertex neighborhood for a mesh using the igraph library.
#'
#' @inheritParams fs.surface.to.igraph
#'
#' @description This is a faster replacement for \code{mesh.vertex.neighbors} that requires the optional dependency package 'igraph'.
#'
#' @param nodes the source vertex. Passed on to \code{igraph::neighborhood}
#'
#' @param order integer, the max graph distance of vertices to consider neighbors (number of neighborhood rings). Passed on to \code{igraph::neighborhood}
#'
#' @param simplify logical, whether to return only an integer vector if the 'nodes' parameter has length 1 (instead of a list where the first element is such a vector).
#'
#' @note If you intend to call several functions on the igraph, it is faster to construct it with \code{fs.surface.to.igraph} and keep it.
#'
#' @seealso The \code{fs.surface.as.adjacencylist} function computes the 1-ring neighborhood for the whole graph.
#'
#' @return named list of integer vectors (see \code{igraph::neighborhood}), unless 'simplify' is TRUE, see there for details.
#'
#' @export
fs.surface.vertex.neighbors <- function(surface, nodes, order = 1L, simplify = TRUE) {
    if(requireNamespace("igraph", quietly = TRUE)) {
        g = fs.surface.to.igraph(surface);
        res = igraph::neighborhood(g, order = order, nodes = nodes, mode = "all");
        if(simplify && length(nodes) == 1L) {
            return(as.integer(unlist(res)));
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
    if(requireNamespace("igraph", quietly = TRUE)) {
        g = fs.surface.to.igraph(surface);
        return(igraph::as_adj_list(g));
    } else {
        stop("This functionality requires the 'igraph' package to be installed.");
    }
}


#' @title Check whether parameter is an fs.surface instance.
#'
#' @param surface an fs.surface instance which will be returned as-is, or a character string which will be interpreted as a file system path and loaded with \code{freesurferformats::read.fs.surface}. Anything else will stop with an error.
#'
#' @return an fs.surface instance, unless an error occurs.
#'
#' @keywords internal
ensure.fs.surface <- function(surface) {
    if(freesurferformats::is.fs.surface(surface)) {
        return(surface);
    } else if(is.character(surface)) {
        return(freesurferformats::read.fs.surface(surface));
    } else {
        stop("Parameter 'surface' must be an fs.surface instance or a character string.");
    }
}

