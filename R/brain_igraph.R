

#' @title Create igraph undirected graph from a brain surface mesh.
#'
#' @param surface an fs.surface instance, as returned by \code{subject.surface}.
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
    surface = ensure.fs.surface(surface);
    if(requireNamespace("igraph", quietly = TRUE)) {
        el = rbind(surface$faces[,1:2], surface$faces[,2:3])
        return(igraph::graph_from_edgelist(el, directed = FALSE));
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

