# Functions for visualizing DTI tracks.
# This is pretty experimental.


#' @title Visualize DTI tracks from Diffusion Toolkit/TrackVis TRK format file.
#'
#' @param trk character string, the path to a TRK file that should be loaded. Alternatively, a loaded \code{trk} instance as returned by \code{freesurferformats::read.dti.trk}.
#'
#' @return The (loaded or received) \code{trk} instance. Note that this function is typically called for the side effect of visualization.
#'
#' @importMethodsFrom freesurferformats read.dti.trk
#' @export
vis.dti.trk <- function(trk) {
    if(is.character(trk)) {
        trk = freesurferformats::read.dti.trk(trk);
    }
    if(trk$header$n_count > 0L) {
        for(track_idx in 1L:trk$header$n_count) {
            vis.path.along.verts(trk$tracks[[track_idx]]$coords);
        }
    } else {
        stop("No tracks to visualize");
    }
    return(trk);
}
