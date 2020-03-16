# Functions that enable visualization of several stacked color layers (e.g., a background and foreground) on a surface
# mesh. The upper layers must have at least some (partly) transparent colors for this to make any sense.
# The background can be a single color or a FreeSurfer-style gray/dark-gray binarized mean curvature pattern that
# gives the viewer a rough orientation with respect to gyri and sulci.


#' @title Compute mean curv surface color layer.
#'
#' @param subjects_dir character string, the FreeSurfer SUBJECTS_DIR.
#'
#' @param subject_id character string, the subject identifier.
#'
#' @param hemi character string, one of 'lh', 'rh', or 'both'. The latter will merge the data for both hemis into a single vector.
#'
#' @param surface string. The display surface. E.g., "white", "pial", or "inflated". Defaults to "white".
#'
#' @param bin_colors vector of two character strings, the two colors to use.
#'
#' @param bin_thresholds vector of 2 double values, the curvature threshold values used to separate gyri from sulci.
#'
#' @return vector of color strings, one color per surface vertex. The coloring separates gyri from sulci.
#'
#' @seealso You can plot the return value using \code{\link[fsbrain]{vis.color.on.subject}}.
#'
#' @export
background.mean.curvature <- function(subjects_dir, subject_id, hemi="both", cortex_only=FALSE, bin_colors=c('#898989', '#5e5e5e'), bin_thresholds=c(-0.1, 0.1)) {
    mc = subject.morph.native(subjects_dir, subject_id, 'curv', hemi=hemi, cortex_only=cortex_only);
    color_layer = rep(bin_colors[1], length(mc));
    gyri_vertices = which(mc > bin_thresholds[1] & mc < bin_thresholds[2]);
    color_layer[gyri_vertices] = bin_colors[2];
    return(color_layer);
}

