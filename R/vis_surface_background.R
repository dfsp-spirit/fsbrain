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
#' @family surface color layer
#' @export
background.mean.curvature <- function(subjects_dir, subject_id, hemi="both", cortex_only=FALSE, bin_colors=c('#898989', '#5e5e5e'), bin_thresholds=c(-0.1, 0.1)) {
    mc = subject.morph.native(subjects_dir, subject_id, 'curv', hemi=hemi, cortex_only=cortex_only);
    color_layer = rep(bin_colors[1], length(mc));
    gyri_vertices = which(mc > bin_thresholds[1] & mc < bin_thresholds[2]);
    color_layer[gyri_vertices] = bin_colors[2];
    return(color_layer);
}


#' @title Merge two or more color layers based on their transparency values.
#'
#' @description Merge several color layers into one based on their transparency and alpha blending. In the final result, the lower layers are visible through the transparent or `NA` parts (if any) of the upper layers.
#'
#' @param collayers named list, the values must be vectors, matrices or arrays of color strings (as produced by \code{\link[grDevices]{rgb}}. The names are freeform and do not really matter. All values must have the same length.
#'
#' @param opaque_background a single color string or `NULL`. If a color string, this color will be used as a final opaque background layer to ensure that the returned colors are all opaque. Pass `NULL` to skip this, which may result in a return value that contains non-opaque color values.
#'
#' @return a vector, matrix or array of color strings
#'
#' @family surface color layer
#'
#' @importFrom grDevices col2rgb
#' @export
collayers.merge <- function(collayers, opaque_background="#ffffff") {
    if(! is.list(collayers)) {
        stop("Parameter 'collayers' must be a named list.");
    }

    if(length(collayers) < 1L) {
        stop("List passed as parameter 'collayers' must not be empty.");
    }

    if(! is.null(opaque_background)) {
        bg_alpha = grDevices::col2rgb(opaque_background, alpha = TRUE)[4];
        if(bg_alpha != 255L) {
            warning(sprintf("Color passed as parameter 'opaque_background' is not opaque: alpha channel has value %d (expected 255).", bg_alpha));
        }
        # Add a new opaque layer at the end
        new_layer_index = length(collayers) + 1L;
        collayers[[new_layer_index]] = rep(opaque_background, length(collayers[[1]]));  # We use the length of the first layer here, but we could use any.
        names(layers)[[new_layer_index]] = 'opaque_background';
    }

    merged = collayers[1];
    for (layer_idx in seq.int(2L, length(collayers))) {
        clayer = collayers[layer_idx];
        merged = alphablend(merged, clayer);
    }
    return(merged);
}


#' @title Perform alpha blending for pairs of RGBA colors.
#'
#' @description Implements the *over* alpha blending operation.
#'
#' @param front_color rgba color strings, the upper color layer
#'
#' @param back_color rgba color strings, the lower color layer
#'
#' @return rgba color strings, the alpha-blended colors
#'
#' @references see the *Alpha blending* section on https://en.wikipedia.org/wiki/Alpha_compositing
#'
#' @importFrom grDevices rgb col2rgb
#' @export
alphablend <- function(front_color, back_color) {

    if(length(front_color) != length(back_color)) {
        stop("The parameters 'front_color' and 'back_color' must be vectors with identical length.");
    }

    front_color_rgba_matrix = grDevices::col2rgb(front_color, alpha = TRUE)/255.;
    back_color_rgba_matrix = grDevices::col2rgb(back_color, alpha = TRUE)/255.;

    src_alpha = front_color_rgba_matrix[4,];
    src_rgb = front_color_rgba_matrix[1:3,];

    dst_alpha = back_color_rgba_matrix[4,];
    dst_rgb = back_color_rgba_matrix[1:3,];

    out_alpha = src_alpha + dst_alpha * (1.0 - src_alpha);

    # TODO: Handle possible division by zero
    out_rgb = (src_rgb * src_alpha + dst_rgb * dst_alpha * (1.0 - src_alpha)) / out_alpha;

    return(grDevices::rgb(t(rbind(out_rgb, out_alpha)), alpha = TRUE));
}
