# Functions for visualizing DTI tracks.
# This is pretty experimental.


#' @title Visualize DTI tracks from Diffusion Toolkit/TrackVis TRK format file.
#'
#' @param trk character string, the path to a TRK file that should be loaded. Alternatively, a loaded \code{trk} instance as returned by \code{freesurferformats::read.dti.trk}.
#'
#' @param filter_tracks optional, named list of filters. Can contain fields \code{min_length} and \code{min_segment_count}. Set the whole thing to \code{NULL} or an entry to 0 for no filtering.
#'
#' @return The (loaded or received) \code{trk} instance. Note that this function is typically called for the side effect of visualization.
#'
#' @note The current simple implementation is very slow if the number of tracks becomes large (several thousand tracks).
#'
#' @importMethodsFrom freesurferformats read.dti.trk
#' @export
vis.dti.trk <- function(trk, filter_tracks = list('min_length' = 20, 'min_segment_count' = 8)) {
    if(is.character(trk)) {
        trk = freesurferformats::read.dti.trk(trk);
    }

    if(trk$header$n_count <= 0L) {
        stop("Empty track file.");
    }

    tracks = trk$tracks; # the potentially filtered tracks.


    if(is.list(filter_tracks)) {
        # Determine track lengths for filtering.
        do_filter_by_track_length = filter_tracks$min_length > 0L;
        if(do_filter_by_track_length) {
            tl = lapply(tracks, function(x) {track.length(x$coords)});
            tracks = tracks[which(tl >= filter_tracks$min_length)];
        }

        # Determine number of segments per track for filtering.
        do_filter_by_num_segments = filter_tracks$min_segment_count > 0L;
        if(do_filter_by_num_segments) {
            tns = lapply(tracks, function(x) {nrow(x$coords)});
            tracks = tracks[which(tns >= filter_tracks$min_segment_count)];
        }
    }

    for(track_idx in 1L:length(tracks)) {
        vis.path.along.verts(tracks[[track_idx]]$coords);
    }
    cat(sprintf("Rendered %d of %d tracks.\n", length(tracks), trk$header$n_count));
    return(invisible(trk));
}


#' @title Compute the total length of a path given by the coordinates of its points.
#'
#' @param point_coords n x 3 numerical matrix of 3D point coordinates, in the order of traversal.
#'
#' @return float, the sum of the length of all segments of the path.
#'
#' @keywords internal
track.length <- function(point_coords) {
    if(nrow(point_coords) < 2L) {
        return(0.0);
    }
    pair_end_indices = seq(2L, nrow(point_coords));
    segment_lengths = sapply(pair_end_indices, function(pair_end_index){
        sqrt(sum((point_coords[(pair_end_index-1L),] - point_coords[pair_end_index, ])^2));
    });
    return(sum(as.double(segment_lengths)));
}
