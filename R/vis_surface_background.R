# Functions that enable visualization of several stacked color layers (e.g., a background and foreground) on a surface
# mesh. The upper layers must have at least some (partly) transparent colors for this to make any sense.
# The background can be a single color or a FreeSurfer-style gray/dark-gray binarized mean curvature pattern that
# gives the viewer a rough orientation with respect to gyri and sulci.


#' @title Compute binarized mean curvature surface color layer.
#'
#' @description Compute a binarized mean curvature surface color layer, this is intended as a background color layer. You can merge it with your data layer using \code{\link[fsbrain]{collayers.merge}}.
#'
#' @inheritParams collayer.bg.meancurv
#'
#' @param bg character string, a background name. One of 'curv', 'sulc', or 'aparc'.  If this is already a colorlayer in a hemilist, it will be returned as-is.
#'
#' @return a color layer, i.e., vector of color strings in a hemilist
#'
#' @seealso You can plot the return value using \code{\link[fsbrain]{vis.color.on.subject}}.
#'
#' @family surface color layer
#' @export
collayer.bg <- function(subjects_dir, subject_id, bg, hemi="both") {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(is.hemilist(bg)) {
        return(bg);
    } else if(is.character(bg)) {
        if(bg == "curv") {
            return(collayer.bg.meancurv(subjects_dir, subject_id, hemi=hemi));
        } else if(bg == "sulc") {
            return(collayer.bg.sulc(subjects_dir, subject_id, hemi=hemi));
        } else if(bg == "aparc") {
            return(collayer.bg.atlas(subjects_dir, subject_id, hemi=hemi, atlas='aparc'));
        } else {
            stop("Parameter 'bg' has unsupported character string value.");
        }
    } else {
        stop("Parameter 'bg' must be a collayer in a hemilist of one of the fixed character strings listed in the help.");
    }
}


#' @title Compute binarized mean curvature surface color layer.
#'
#' @description Compute a binarized mean curvature surface color layer, this is intended as a background color layer. You can merge it with your data layer using \code{\link[fsbrain]{collayers.merge}}.
#'
#' @param subjects_dir character string, the FreeSurfer SUBJECTS_DIR.
#'
#' @param subject_id character string, the subject identifier.
#'
#' @param hemi character string, one of 'lh', 'rh', or 'both'. The latter will merge the data for both hemis into a single vector.
#'
#' @param cortex_only logical, whether to restrict pattern computation to the cortex.
#'
#' @param bin_colors vector of two character strings, the two colors to use.
#'
#' @param bin_thresholds vector of 1 or 2 double values, the curvature threshold values used to separate gyri from sulci.
#'
#' @return a color layer, i.e., vector of color strings in a hemilist
#'
#' @seealso You can plot the return value using \code{\link[fsbrain]{vis.color.on.subject}}.
#'
#' @family surface color layer
#' @export
collayer.bg.meancurv <- function(subjects_dir, subject_id, hemi="both", cortex_only=FALSE, bin_colors=c('#898989', '#5e5e5e'), bin_thresholds=c(0.0)) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    color_layer = list();
    if(hemi %in% c("lh", "both")) {
        mc = subject.morph.native(subjects_dir, subject_id, 'curv', hemi='lh', cortex_only=cortex_only);
        cl = rep(bin_colors[1], length(mc));
        if(length(bin_thresholds) == 1L) {
            gyri_vertices = which(mc > bin_thresholds[1]);
        } else {
            gyri_vertices = which(mc > bin_thresholds[1] & mc < bin_thresholds[2]);
        }
        cl[gyri_vertices] = bin_colors[2];
        color_layer$lh = cl;
    }
    if(hemi %in% c("rh", "both")) {
        mc = subject.morph.native(subjects_dir, subject_id, 'curv', hemi='rh', cortex_only=cortex_only);
        cl = rep(bin_colors[1], length(mc));
        if(length(bin_thresholds) == 1L) {
            gyri_vertices = which(mc > bin_thresholds[1]);
        } else {
            gyri_vertices = which(mc > bin_thresholds[1] & mc < bin_thresholds[2]);
        }
        cl[gyri_vertices] = bin_colors[2];
        color_layer$rh = cl;
    }
    return(color_layer);
}


#' @title Compute binarized sulcal depth surface color layer.
#'
#' @description Compute a binarized sulcal depth surface color layer, this is intended as a background color layer. You can merge it with your data layer using \code{\link[fsbrain]{collayers.merge}}.
#'
#' @inheritParams collayer.bg.meancurv
#'
#' @return a color layer, i.e., vector of color strings in a hemilist
#'
#' @seealso You can plot the return value using \code{\link[fsbrain]{vis.color.on.subject}}.
#'
#' @family surface color layer
#' @export
collayer.bg.sulc <- function(subjects_dir, subject_id, hemi="both", cortex_only=FALSE, bin_colors=c('#898989', '#5e5e5e'), bin_thresholds=c(0.0)) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    color_layer = list();
    if(hemi %in% c("lh", "both")) {
        mc = subject.morph.native(subjects_dir, subject_id, 'sulc', hemi='lh', cortex_only=cortex_only);
        cl = rep(bin_colors[1], length(mc));
        if(length(bin_thresholds) == 1L) {
            gyri_vertices = which(mc > bin_thresholds[1]);
        } else {
            gyri_vertices = which(mc > bin_thresholds[1] & mc < bin_thresholds[2]);
        }
        cl[gyri_vertices] = bin_colors[2];
        color_layer$lh = cl;
    }
    if(hemi %in% c("rh", "both")) {
        mc = subject.morph.native(subjects_dir, subject_id, 'sulc', hemi='rh', cortex_only=cortex_only);
        cl = rep(bin_colors[1], length(mc));
        if(length(bin_thresholds) == 1L) {
            gyri_vertices = which(mc > bin_thresholds[1]);
        } else {
            gyri_vertices = which(mc > bin_thresholds[1] & mc < bin_thresholds[2]);
        }
        cl[gyri_vertices] = bin_colors[2];
        color_layer$rh = cl;
    }
    return(color_layer);
}


#' @title Compute atlas or annotation surface color layer.
#'
#' @inheritParams collayer.bg.meancurv
#'
#' @param atlas character string, the atlas name. E.g., "aparc", "aparc.2009s", or "aparc.DKTatlas". Used to construct the name of the annotation file to be loaded.
#'
#' @param grayscale logical, whether to convert the atlas colors to grayscale
#'
#' @param outline logical, whether to draw an outline only instead of filling the regions. Defaults to `FALSE`. Instead of passing `TRUE`, one can also pass a list of extra parameters to pass to \code{\link[fsbrain]{annot.outline}}, e.g., \code{outline=list('outline_color'='#000000')}.
#'
#' @param outline_surface character string, the surface to load. Only relevant when 'outline' is used. (In that case the surface mesh is needed to compute the vertices forming the region borders.)
#'
#' @return a color layer, i.e., vector of color strings in a hemilist
#'
#' @seealso You can plot the return value using \code{\link[fsbrain]{vis.color.on.subject}}.
#'
#' @note Using 'outline' mode is quite slow, and increasing the border thickness makes it even slower.
#'
#' @family surface color layer
#' @importFrom utils modifyList
#' @export
collayer.bg.atlas <- function(subjects_dir, subject_id, hemi="both", atlas="aparc", grayscale=FALSE, outline=FALSE, outline_surface = "white") {
    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(is.list(outline)) {
        annot_outline_extra_options = outline;
        #annot_outline_full_options = utils::modifyList(list(annot, surface_mesh), annot_outline_extra_options);
        #col = do.call(annot.outline, annot_outline_full_options);
    } else if(outline == TRUE) {
        annot_outline_extra_options = list('background' = '#FFFFFFFF');
    } else if(is.integer(outline)) {
        annot_outline_extra_options = list('background' = '#FFFFFFFF', 'expand_inwards' = outline - 1L);
    } else {
        annot_outline_extra_options = NULL; # do not use outline mode
    }

    use_outline_mode = ! is.null(annot_outline_extra_options);

    if(use_outline_mode) {
        annot_layer = list();
        if(hemi %in% c("lh", "both")) {
            annot = subject.annot(subjects_dir, subject_id, 'lh', atlas);
            surface_mesh = subject.surface(subjects_dir, subject_id, outline_surface, 'lh');
            annot_outline_full_options = utils::modifyList(list(annot, surface_mesh), annot_outline_extra_options);
            annot_layer$lh = do.call(annot.outline, annot_outline_full_options);
        }
        if(hemi %in% c("rh", "both")) {
            annot = subject.annot(subjects_dir, subject_id, 'rh', atlas);
            surface_mesh = subject.surface(subjects_dir, subject_id, outline_surface, 'rh');
            annot_outline_full_options = utils::modifyList(list(annot, surface_mesh), annot_outline_extra_options);
            annot_layer$rh = do.call(annot.outline, annot_outline_full_options);
            #annot_layer$rh = annot.outline(subject.annot(subjects_dir, subject_id, 'rh', atlas), subject.surface(subjects_dir, subject_id, outline_surface, 'rh'), background = '#FFFFFFFF', expand_inwards = outline - 1L);
        }
    } else {
        annot_layer = collayer.from.annot(subjects_dir, subject_id, hemi, atlas);
    }

    if(grayscale) {
        annot_layer = lapply(annot_layer, desaturate);
    }
    return(annot_layer);
}


#' @title Compute surface color layer from morph-like data.
#'
#' @param lh_morph_data numerical vector, can be NULL
#'
#' @param rh_morph_data numerical vector, can be NULL
#'
#' @param makecmap_options named list of parameters to pass to \code{\link{makecmap}}. Must not include the unnamed first parameter, which is derived from 'measure'.
#'
#' @param return_metadata logical, whether to return additional metadata as entry 'metadata' in the returned list
#'
#' @return named hemi list, each entry is a vector of color strings, one color per surface vertex. The coloring represents the morph data.
#'
#' @seealso You can plot the return value using \code{\link[fsbrain]{vis.color.on.subject}}.
#'
#' @family surface color layer
#' @importFrom utils modifyList
#' @importFrom squash cmap makecmap jet
#' @export
collayer.from.morphlike.data <- function(lh_morph_data=NULL, rh_morph_data=NULL, makecmap_options=list('colFn'=squash::jet), return_metadata=FALSE) {
    if(is.null(lh_morph_data) | is.null(rh_morph_data)) {

        if(is.null(lh_morph_data) & is.null(rh_morph_data)) {
            warning("Both 'lh_morph_data' and 'rh_morph_data' are NULL, return a single white color value for each hemi.");
            return(list("lh"="#FFFFFF", "rh"="#FFFFFF"));
        }

        if(is.null(lh_morph_data)) {
            hemi = "rh";
            morph_data = rh_morph_data;
        } else {
            hemi = "lh";
            morph_data = lh_morph_data;
        }

        if(! 'colFn' %in% names(makecmap_options)) {
            warning("No colFn given");
        }

        map = do.call(squash::makecmap, utils::modifyList(list(morph_data), makecmap_options));
        single_hemi_color_layer = squash::cmap(morph_data, map = map);
        collayer = hemilist.wrap(single_hemi_color_layer, hemi);
        if(return_metadata) {
            map_sorted = do.call(squash::makecmap, utils::modifyList(list(sort(morph_data)), makecmap_options));
            col_sorted = squash::cmap(sort(morph_data), map = map_sorted);
            collayer$metadata = list('map'=map, 'map_sorted'=map_sorted, 'col_sorted'=col_sorted);
        }
        return(collayer);
    } else {
        merged_morph_data = c(lh_morph_data, rh_morph_data);

        common_cmap = do.call(squash::makecmap, utils::modifyList(list(merged_morph_data), makecmap_options));
        if(is.numeric(lh_morph_data)) {
            lh_layer = squash::cmap(lh_morph_data, map = common_cmap);
        } else {
            lh_layer = NULL;
        }
        if(is.numeric(rh_morph_data)) {
            rh_layer = squash::cmap(rh_morph_data, map = common_cmap);
        } else {
            rh_layer = NULL;
        }
        collayer = list("lh"=lh_layer, "rh"=rh_layer);
        if(return_metadata) {
            common_cmap_sorted = do.call(squash::makecmap, utils::modifyList(list(sort(merged_morph_data)), makecmap_options));
            col_sorted = squash::cmap(sort(merged_morph_data), map = common_cmap_sorted);
            collayer$metadata = list('map'=common_cmap, 'map_sorted'=common_cmap_sorted, 'col_sorted'=col_sorted);
        }
        return(collayer);
    }
}


#' @title Compute surface color layer from morph-like data.
#'
#' @param lh_data integer vector, can be NULL
#'
#' @param rh_data numerical vector, can be NULL
#'
#' @param makecmap_options named list of parameters to pass to \code{\link{makecmap}}. Must not include the unnamed first parameter, which is derived from 'measure'.
#'
#' @return named hemi list, each entry is a vector of color strings, one color per surface vertex. The coloring represents the label data.
#'
#' @seealso You can plot the return value using \code{\link[fsbrain]{vis.color.on.subject}}.
#'
#' @family surface color layer
#' @importFrom utils modifyList
#' @importFrom squash cmap makecmap rainbow2
#' @export
collayer.from.mask.data <- function(lh_data=NULL, rh_data=NULL, makecmap_options=list('colFn'=label.colFn)) {
    if(is.null(lh_data) | is.null(rh_data)) {

        if(is.null(lh_data) & is.null(rh_data)) {
            warning("Both 'lh_data' and 'rh_data' are NULL, return a single white color value for each hemi.");
            return(list("lh"="#FFFFFF", "rh"="#FFFFFF"));
        }

        if(is.null(lh_data)) {
            hemi = "rh";
            label_data = rh_data;
        } else {
            hemi = "lh";
            label_data = lh_data;
        }

        if(is.logical(label_data)) {
            label_data = as.integer(label_data);
        }

        color_layer = squash::cmap(label_data, map = do.call(squash::makecmap, utils::modifyList(list(label_data), makecmap_options)));
        return(hemilist.wrap(color_layer, hemi));
    } else {
        if(is.logical(lh_data)) {
            lh_data = as.integer(lh_data);
        }
        if(is.logical(rh_data)) {
            rh_data = as.integer(rh_data);
        }
        merged_label_data = c(lh_data, rh_data);
        common_cmap = do.call(squash::makecmap, utils::modifyList(list(merged_label_data), makecmap_options));
        if(is.numeric(lh_data)) {
            lh_layer = squash::cmap(lh_data, map = common_cmap);
        } else {
            lh_layer = NULL;
        }
        if(is.numeric(rh_data)) {
            rh_layer = squash::cmap(rh_data, map = common_cmap);
        } else {
            rh_layer = NULL;
        }
        return(list("lh"=lh_layer, "rh"=rh_layer));
    }
}


#' @title A simple colormap function for binary colors.
#'
#' @description Useful for plotting labels.
#'
#' @param n positive integer, the number of colors. Must be 1 or 2 for this function.
#'
#' @param col_a color string, the foreground color
#'
#' @param col_b color string, the background color
#'
#' @return vector of 'n' RGB colorstrings
#'
#' @export
label.colFn <- function(n=2L, col_a='#228B22', col_b="#FFFFFF") {
    n = as.integer(n);
    if(n < 1) {
        stop(sprintf("Parameter 'n' must be >= 1L but is '%d'.\n", n));
    }
    if(n == 1L) {
        return();
    } else if(n==2L) {
        return(c(col_b, col_a));
    } else {
        n_half = as.integer(ceiling(n/2.0));
        col = rep(col_a, n);
        col[1:n_half] = col_b;
        return(col);
    }
}


#' @title A simple colormap function for binary colors.
#'
#' @description Useful for plotting labels.
#'
#' @param n positive integer, the number of colors. Must be 1 or 2 for this function.
#'
#' @param col_a color string, the foreground color
#'
#' @param col_b color string, the background color
#'
#' @return vector of 'n' RGB colorstrings
#'
#' @export
label.colFn.inv <- function(n=2L, col_a='#228B22', col_b="#FFFFFF") {
    return(label.colFn(n=n, col_a=col_b, col_b=col_a));
}


#' @title Compute surface color layer from annotation or atlas data.
#'
#' @param lh_annotdata loaded annotation data for left hemi, as returned by \code{\link[fsbrain]{subject.annot}}
#'
#' @param rh_annotdata loaded annotation data for right hemi
#'
#' @return named hemi list, each entry is a vector of color strings, one color per surface vertex. The coloring represents the atlas data.
#'
#' @seealso You can plot the return value using \code{\link[fsbrain]{vis.color.on.subject}}.
#' @family surface color layer
#' @export
collayer.from.annotdata <- function(lh_annotdata=NULL, rh_annotdata=NULL) {
    if(is.null(lh_annotdata) | is.null(rh_annotdata)) {

        if(is.null(lh_annotdata) & is.null(rh_annotdata)) {
            warning("Both 'lh_annotdata' and 'rh_annotdata' are NULL, return a single white color value for each hemi.");
            return(list("lh"="#FFFFFF", "rh"="#FFFFFF"));
        }

        if(is.null(lh_annotdata)) {
            hemi = "rh";
            annot_data = rh_annotdata;
        } else {
            hemi = "lh";
            annot_data = lh_annotdata;
        }

        color_layer = annot_data$hex_colors_rgb;
        return(hemilist.wrap(color_layer, hemi));
    } else {
        lh_layer = lh_annotdata$hex_colors_rgb;
        rh_layer = rh_annotdata$hex_colors_rgb;
        return(list("lh"=lh_layer, "rh"=rh_layer));
    }
}


#' @title Compute surface color layer from annotation or atlas data.
#'
#' @param subjects_dir character string, the FreeSurfer SUBJECTS_DIR.
#'
#' @param subject_id character string, the subject identifier.
#'
#' @param hemi character string, one of 'lh', 'rh', or 'both'.
#'
#' @param atlas character string, the atlas name. E.g., "aparc", "aparc.2009s", or "aparc.DKTatlas". Used to construct the name of the annotation file to be loaded.
#'
#' @return named hemi list, each entry is a vector of color strings, one color per surface vertex. The coloring represents the atlas data.
#'
#' @seealso You can plot the return value using \code{\link[fsbrain]{vis.color.on.subject}}.
#'
#' @family surface color layer
#' @export
collayer.from.annot <- function(subjects_dir, subject_id, hemi, atlas) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(hemi == "both") {
        lh_annotdata = subject.annot(subjects_dir, subject_id, 'lh', atlas);
        rh_annotdata = subject.annot(subjects_dir, subject_id, 'rh', atlas);
        return(collayer.from.annotdata(lh_annotdata, rh_annotdata));
    } else {
        hemi_annotdata = subject.annot(subjects_dir, subject_id, hemi, atlas);
        if(hemi == "lh") {
            return(collayer.from.annotdata(hemi_annotdata, NULL));
        } else {
            return(collayer.from.annotdata(NULL, hemi_annotdata));
        }
    }
}


#' @title Merge two or more color layers based on their transparency values.
#'
#' @description Merge several color layers into one based on their transparency and alpha blending. In the final result, the lower layers are visible through the transparent or `NA` parts (if any) of the upper layers.
#'
#' @param collayers named list, the values must be vectors, matrices or arrays of color strings (as produced by \code{\link[grDevices]{rgb}}. The names are freeform and do not really matter. All values must have the same length.
#'
#' @param opaque_background a single color string or `NULL`. If a color string, this color will be used as a final opaque background layer to ensure that the returned colors are all opaque. Pass `NULL` to skip this, which may result in a return value that contains non-opaque color values.
#'
#' @return a color layer, i.e., vector of color strings in a hemilist
#'
#' @family surface color layer
#'
#' @importFrom grDevices col2rgb
#' @export
collayers.merge <- function(collayers, opaque_background="#FFFFFF") {
    if(! is.list(collayers)) {
        stop("Parameter 'collayers' must be a named list.");
    }

    if(length(collayers) < 1L) {
        stop("List passed as parameter 'collayers' must not be empty.");
    }

    if(! is.null(opaque_background)) {
        bg_alpha = grDevices::col2rgb(opaque_background, alpha = TRUE)[4];
        if(bg_alpha != 255L) {
            warning(sprintf("Color passed as parameter 'opaque_background' is not opaque: alpha channel has value %d (expected 255).\n", bg_alpha));
        }
        # Add a new opaque layer at the end
        new_layer_index = length(collayers) + 1L;

        first_layer = collayers[[1]];  # We use the the first layer here, but we could use any: they must have same dimensions
        if(is.hemilist(first_layer)) {
            new_layer = list();
            if(!is.null(first_layer$lh)) {
                new_layer$lh = rep(opaque_background, length(first_layer$lh));
            }
            if(!is.null(first_layer$rh)) {
                new_layer$rh = rep(opaque_background, length(first_layer$rh));
            }
        } else {
            new_layer = rep(opaque_background, length(first_layer));
        }

        collayers[[new_layer_index]] = new_layer;
        names(collayers)[[new_layer_index]] = 'opaque_background';
    }

    merged = collayers[[1]];
    for (layer_idx in seq.int(2L, length(collayers))) {
        layer_name = names(collayers)[[layer_idx]];
        clayer = collayers[[layer_idx]];
        merged = alphablend(merged, clayer, silent=(layer_name == 'opaque_background'));
    }
    return(merged);
}


#' @title Perform alpha blending for pairs of RGBA colors.
#'
#' @description Implements the *over* alpha blending operation.
#'
#' @param front_color rgba color strings, the upper color layer or foreground
#'
#' @param back_color rgba color strings, the lower color layer or background
#'
#' @param silent logical, whether to suppress messages
#'
#' @return rgba color strings, the alpha-blended colors
#'
#' @references see the *Alpha blending* section on https://en.wikipedia.org/wiki/Alpha_compositing
#'
#' @family color functions
#'
#' @importFrom grDevices rgb col2rgb
#' @export
alphablend <- function(front_color, back_color, silent=TRUE) {

    if(is.hemilist(front_color)) {
        if(is.hemilist(back_color)) {
            ret_list = list();
            if(!is.null(front_color$lh)) {
                ret_list$lh = alphablend(front_color$lh, back_color$lh);
            }
            if(!is.null(front_color$rh)) {
                ret_list$rh = alphablend(front_color$rh, back_color$rh);
            }
            return(ret_list);
        } else {
            stop("The parameters 'front_color' and 'back_color' must have the same type (currently 'front_color' is a hemilist, 'back_color' is not).");
        }
    }

    if(length(front_color) != length(back_color)) {
        stop(sprintf("The parameters 'front_color' (length %d) and 'back_color' (length %d) must be vectors with identical length.\n", length(front_color), length(back_color)));
    }

    # Treat NA values as fully transparent color.
    front_color[which(is.na(front_color))] = '#00000000';
    back_color[which(is.na(back_color))] = '#00000000';

    front_color_rgba_matrix = grDevices::col2rgb(front_color, alpha = TRUE)/255.;
    back_color_rgba_matrix = grDevices::col2rgb(back_color, alpha = TRUE)/255.;

    src_alpha = front_color_rgba_matrix[4,];

    if(!any(src_alpha < 1)) {
        if(!silent) {
            message("Background will not be visible, foreground is fully opaque. Set foreground colors to NA or use the alpha channel to see the background.");
        }
    }

    src_rgb = front_color_rgba_matrix[1:3,];

    dst_alpha = back_color_rgba_matrix[4,];
    dst_rgb = back_color_rgba_matrix[1:3,];

    out_alpha = src_alpha + dst_alpha * (1.0 - src_alpha);

    out_rgb = (t(src_rgb) * src_alpha + t(dst_rgb) * dst_alpha * (1.0 - src_alpha)) / out_alpha;

    # Handle possible division by zero NaNs from last division. Happens when both foreground and background
    # color are fully transparent. The output alpha will be 0, and we set the rgb values to all zeroes as well.
    out_rgb[is.nan(out_rgb)] = 0.;

    return(grDevices::rgb(cbind(out_rgb, out_alpha), alpha = TRUE));
}


#' @title Perform simple desaturation or grayscale conversion of RGBA colors.
#'
#' @param color rgba color strings
#'
#' @param gamma_correct logical, whether to apply non-linear gamma correction. First performs gamma expansion, then applies the gray-scale channel weigths, then gamma compression.
#'
#' @return rgba color strings, the grayscale colors. The information from one of the three rgb channels would be enough. The alpha value is not touched.
#'
#' @references see https://en.wikipedia.org/wiki/Grayscale#Converting_color_to_grayscale
#'
#' @note Assumes sRGB color space.
#'
#' @family color functions
#'
#' @importFrom grDevices rgb col2rgb
#' @export
desaturate <- function(color, gamma_correct=FALSE) {
    color_rgba_matrix = grDevices::col2rgb(color, alpha = TRUE)/255.;

    src_alpha = color_rgba_matrix[4,];
    src_rgb = color_rgba_matrix[1:3,];

    if(gamma_correct) {
        # perform gamma expansion
        src_rgb = ifelse(src_rgb <= 0.04045, src_rgb / 12.92, ((src_rgb + 0.055)/1.055)**2.4);
    }

    channel_weights = c(0.2126, 0.7152, 0.0722);

    out_rgb_per_channel = t(src_rgb) %*% c(channel_weights);   # divide by number of channels

    if(gamma_correct) {
        # perform gamma compression
        out_rgb_per_channel = ifelse(out_rgb_per_channel <= 0.0031308, 12.92 * out_rgb_per_channel, 1.055 * out_rgb_per_channel**(1/2.4) - 0.055);
    }

    out_rgb = cbind(out_rgb_per_channel, out_rgb_per_channel, out_rgb_per_channel);

    return(grDevices::rgb(cbind(out_rgb, src_alpha), alpha = TRUE));
}

