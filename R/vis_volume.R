# Functions for volume rendering. Totally WIP.

#' @keywords internal
volvis <- function(volume, slice_index=100) {
    slice = volume[slice_index,,];
    image(slice);
}
