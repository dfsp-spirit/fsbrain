# functions for rglactions

#' @title Clip data at quantiles to remove outliers.
#'
#' @description Set all data values outside the given quantile range to the border values. This is useful to properly visualize morphometry data that includes outliers. These outliers negatively affect the colormap, as all the non-outlier values become hard to distinguish. This function can be used to filter the data before plotting it.
#'
#' @param data, numeric vector. The input data. Can also be a \code{\link{hemilist}}.
#'
#' @param lower, numeric. The probability for the lower quantile, defaults to `0.05`.
#'
#' @param upper, numeric. The probability for the upper quantile, defaults to `0.95`.
#'
#' @return numeric vector. The output data.
#'
#' @seealso The \code{\link{clip_fun}} function is more convenient when used in \code{\link{rglactions}}, as it allows specification of custom quantiles.
#'
#' @examples
#'    full_data = rnorm(50, 3, 1);
#'    clipped = clip.data(full_data);
#'
#' @importFrom stats quantile
#' @export
clip.data <- function(data, lower=0.05, upper=0.95){

    if(is.hemilist(data)) { # treat as a hemi list
        return(lapply(data, clip.data, lower, upper));
    } else {
        quantiles = stats::quantile(data, c(lower, upper), na.rm = TRUE, names = FALSE);
        data[ data < quantiles[1] ] = quantiles[1];
        data[ data > quantiles[2] ] = quantiles[2];
    }
    return(data);
}


#' @title Get data clipping function.
#'
#' @description Get data clipping function to use in \code{\link{rglactions}} as 'trans_fun' to transform data. This is typically used to limit the colorbar in a plot to a certain range. This uses percentiles to clip. Clipping means that values more extreme than the gíven quantiles will be set to the quantile values.
#'
#' @inheritParams clip.data
#'
#' @return a function that takes as argument the data, and clips it to the requested range. I.e., values outside the range will be set to the closest border value. Designed to be used as \code{rglactions$trans_fun} in vis functions, to limit the colorbar and data range.
#'
#' @seealso \code{\link{rglactions}}
#'
#' @examples
#'    rglactions = list("trans_fun"=clip_fun(0.10, 0.90));
#'    rglactions = list("trans_fun"=clip_fun());
#'    f = clip_fun();
#'    f(rnorm(100));
#'
#' @importFrom stats quantile
#' @export
clip_fun <- function(lower=0.05, upper=0.95) {
    res_fun <- function(data) {
        quantiles = stats::quantile(data, c(lower, upper), na.rm = TRUE, names = FALSE);
        data[ data < quantiles[1] ] = quantiles[1];
        data[ data > quantiles[2] ] = quantiles[2];
        return(data);
    };
    return(res_fun);
}


#' @title Shift hemis apart if indicated in rglactions
#'
#' @param coloredmeshes hemilist of coloredmeshes
#'
#' @param rglactions the \code{\link{rglactions}}, a named list as passed to functions like vis.subject.morph.native.
#'
#' @return hemilist of coloredmeshes, the coordinates may or may not have been shifted, depending on the \code{\link{rglactions}}.
#'
#' @keywords internal
shift.hemis.rglactions <- function(coloredmeshes, rglactions) {

    if(rglactions.has.key(rglactions, 'shift_hemis_apart')) {
        #logger::log_info("Function shift.hemis.rglactions called, key 'shift_hemis_apart' is present.");
        shift_hemis = rglactions$shift_hemis_apart;
        if(is.logical(shift_hemis)) {
            #logger::log_info("Function shift.hemis.rglactions called with logical.");
            if(shift_hemis) {
                return(shift.hemis.apart(coloredmeshes, hemi_order_on_axis='lr'));
            }
        } else if(is.list(shift_hemis)) {
            #logger::log_info("Function shift.hemis.rglactions called with list.");
            # interpret the list as extra parameters to pass to shift.hemis.apart
            return(do.call(shift.hemis.apart, utils::modifyList(list(coloredmeshes), shift_hemis)));
        } else if(is.character(shift_hemis)) {
            #logger::log_info("Function shift.hemis.rglactions called with char.");
            if(shift_hemis == 'lr' | shift_hemis == 'lhrh') {
                return(shift.hemis.apart(coloredmeshes, hemi_order_on_axis='lr'));
            } else if(shift_hemis == 'rl' | shift_hemis == 'rhlh') {
                return(shift.hemis.apart(coloredmeshes, hemi_order_on_axis='rl'));
            } else if(shift_hemis == 'auto' | shift_hemis == 'auto_flipped') {
                return(shift.hemis.apart(coloredmeshes, hemi_order_on_axis=shift_hemis));
            } else {
                warning("Value in rglactions$shift_hemis_apart is not supported, ignored. Not shifting hemis.");
            }
        } else {
            warning("Value in rglactions$shift_hemis_apart is not supported, ignored. Not shifting hemis.");
        }
    }
    return(coloredmeshes);
}


#' @title Apply data transformation rglactions.
#'
#' @param measure_data numeric vector, or hemilist of numeric vectors. The data, usually vertex-wise morphometry data.
#'
#' @param rglactions named list, passed as parameter '\code{\link{rglactions}}' to functions like \code{\link[fsbrain]{vis.subject.morph.native}}.
#'
#' @return the transformed data
#'
#' @keywords internal
rglactions.transform <- function(measure_data, rglactions) {
    if(is.null(rglactions)) {
        return(measure_data);
    }
    if(is.hemilist(measure_data)) {
        return(lapply(measure_data, rglactions.transform, rglactions=rglactions));
    }
    if(hasIn(rglactions, list('clip_data'))) {
        clip_range = rglactions$clip_data;
        measure_data = clip.data(measure_data, lower=clip_range[1], upper=clip_range[2]);
    }
    if(hasIn(rglactions, list('trans_fun'))) {
        trans_fun = rglactions$trans_fun;
        if(! is.function(trans_fun)) {
            stop("The value of rglactions entry 'trans_fun' must be a function.");
        }
        measure_data = trans_fun(measure_data);
    }
    return(measure_data);
}


#' @title Get data limiting function.
#'
#' @description Get data limiting function to use in rglactions as 'trans_fun' to transform data. This is typically used to limit the colorbar in a plot to a certain range. This is similar to \code{\link{clip.data}} or \code{\link{clip_fun}}, but uses absolute values instead of percentiles to clip.
#'
#' @param vmin numerical scalar, the lower border. Data values below this will be set to vmin in the return value.
#'
#' @param vmax numerical scalar, the upper border. Data values above this will be set to vmax in the return value.
#'
#' @return a function that takes as argument the data, and clips it to the requested range. I.e., values outside the range will be set to the closest border value ('vmin' or 'vmax'). Designed to be used as \code{rglactions$trans_fun} in vis functions, to limit the colorbar and data range.
#'
#' @seealso \code{\link{rglactions}}
#'
#' @examples
#'    rglactions = list("trans_fun"=limit_fun(2,3));
#'
#' @export
limit_fun <- function(vmin, vmax) {
    limit_fun <- function(data) {
        data[data < vmin] = vmin;
        data[data > vmax] = vmax;
        return(data);
    };
    return(limit_fun);
}


#' @title Get data limiting function to NA.
#'
#' @description Get data limiting function to use in \code{\link{rglactions}} as 'trans_fun' to transform data. This is typically used to limit the colorbar in a plot to a certain range. This is similar to \code{\link{clip.data}}, but uses absolute values instead of percentiles to clip.
#'
#' @param vmin numerical scalar, the lower border. Data values below this will be set to `NA` in the return value.
#'
#' @param vmax numerical scalar, the upper border. Data values above this will be set to `NA` in the return value.
#'
#' @return a function that takes as argument the data, and clips it to the requested range. I.e., values outside the range will be set to `NA`. Designed to be used as \code{rglactions$trans_fun} in vis functions, to limit the colorbar and data range.
#'
#' @note This is useful for thresholding stuff like t-value maps. All values outside the range will be displayed as the background color.
#'
#' @seealso \code{\link{limit_fun_na_inside}} which will set the values inside the range to `NA`.
#'
#' @examples
#'    rglactions = list("trans_fun"=limit_fun_na(2,3));
#'
#' @export
limit_fun_na <- function(vmin, vmax) {
    limit_fun <- function(data) {
        data[data < vmin] = NA;
        data[data > vmax] = NA;
        return(data);
    };
    return(limit_fun);
}


#' @title Get data limiting function, setting values inside range to NA.
#'
#' @description Get data limiting function to use in \code{\link{rglactions}} as 'trans_fun' to transform data.
#'
#' @param vmin numerical scalar, the lower border. Data values between this and vmax will be set to `NA` in the return value.
#'
#' @param vmax numerical scalar, the upper border. See 'vmin'.
#'
#' @return a function that takes as argument the data, and clips it to the requested range. I.e., values inside the range will be set to `NA`. Designed to be used as \code{rglactions$trans_fun} in vis functions.
#'
#' @note This is useful for thresholding data plotted with a background. All values inside the range will set to NA and be transparent, and thus be displayed as the background color.
#'
#' @examples
#'    rglactions = list("trans_fun"=limit_fun_na_inside(2,3));
#'
#' @seealso \code{\link{limit_fun_na}} which will set the values outside the range to `NA`.
#'
#' @export
limit_fun_na_inside <- function(vmin, vmax) {
    limit_fun <- function(data) {
        data[which(data > vmin & data < vmax)] = NA;
        return(data);
    };
    return(limit_fun);
}


#' @title Check for a key in names of rglactions.
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions, see \code{\link{rglactions}}. The values can be used to specify parameters for the action.
#'
#' @return logical, whether the \code{\link{rglactions}} instance has the requested key as a name.
#'
#' @keywords internal
rglactions.has.key <- function(rglactions, key) {
    if(is.list(rglactions)) {
        return(key %in% names(rglactions));
    }
    return(FALSE);
}


#' @title Perform rglactions, like taking screenshots.
#'
#' @description Take a list specifying actions, see \code{\link{rglactions}}, and execute them. This function should be called once an rgl scene has been setup and rendered. A typical use case is to save a screenshot of the scene.
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action. See \code{\link{rglactions}}.
#'
#' @param at_index integer, the index to use in case of vectorized entries. Allows using different output_images for different views or similar.
#'
#' @param silent logical, whether to suppress messages
#'
#' @param ignore vector of character strings, actions to ignore
#'
#' @keywords internal
#' @importFrom rgl rgl.snapshot rgl.postscript
perform.rglactions <- function(rglactions, at_index=NULL, silent=TRUE, ignore = c()) {
    if(is.list(rglactions)) {

        #valid_rglactions = c("text", "snapshot_png", "snapshot_vec", "highlight_points", "shift_hemis_apart", "no_vis");
        #for(action in names(rglactions)) {
        #    if(! (action %in% valid_rglactions)) {
        #        warning(sprintf("Ignoring unsupported rglaction '%s'.\n", action));
        #    }
        #}

        if("text" %in% names(rglactions)) {
            if(!("text" %in% ignore)) {
                do.call(rgl::text3d, rglactions$text);
            }
        }
        if("snapshot_png" %in% names(rglactions)) {
            if(!("snapshot_png" %in% ignore)) {
                if(length(rglactions$snapshot_png) == 1 || is.null(at_index)) {
                    output_image = path.expand(rglactions$snapshot_png);
                } else {
                    if(length(rglactions$snapshot_png) < at_index) {
                        warning(sprintf("Requested rglaction at_index '%d' but only %d entries exist for action 'snapshot_png'.\n", at_index, length(rglactions$snapshot_png)));
                    }
                    output_image = path.expand(rglactions$snapshot_png[[at_index]]);
                }
                rgl::rgl.snapshot(output_image, fmt="png");
                if(! silent) {
                    message(sprintf("Bitmap screenshot written to '%s' (current working dir is '%s').\n", output_image, getwd()));
                }
            }
        }
        if("snapshot_vec" %in% names(rglactions)) {
            if(!("snapshot_vec" %in% ignore)) {
                snapshot_vec_format = "eps";
                if("snapshot_vec_format" %in% names(rglactions)) {
                    supported_formats = c("ps", "eps", "tex", "pdf", "svg", "pgf");
                    if(rglactions$snapshot_vec_format %in% supported_formats) {
                        snapshot_vec_format = rglactions$snapshot_vec_format;
                    } else {
                        stop(sprintf("rglactions: invalid snapshot_vec_format '%s'. Must be one of ''.", rglactions$snapshot_vec_format, paste(supported_formats, collapse = ", ")));
                    }
                }

                if(length(rglactions$snapshot_vec) == 1 || is.null(at_index)) {
                    output_image = path.expand(rglactions$snapshot_vec);
                } else {
                    if(length(rglactions$snapshot_vec) < at_index) {
                        warning(sprintf("Requested rglaction at_index '%d' but only %d entries exist for action 'snapshot_vec'.\n", at_index, length(rglactions$snapshot_vec)));
                    }
                    output_image = path.expand(rglactions$snapshot_vec[[at_index]]);
                }
                rgl::rgl.postscript(output_image, fmt = snapshot_vec_format);
                if(! silent) {
                    message(sprintf("Vector graphics screenshot written to '%s' in format '%s' (current working dir is '%s').\n", output_image, snapshot_vec_format, getwd()));
                }
            }
        }
        if("highlight_points" %in% names(rglactions)) {
            if(!("highlight_points" %in% ignore)) {
                hp = rglactions$highlight_points;
                highlight.points.spheres(hp$coords, color = hp$color, radius = hp$radius);
            }
        }
    }
}


#' @title Create rglactions list, suitable to be passed as parameter to vis functions.
#'
#' @note List of all available rglactions: (1) `snapshot_png=filepath` takes a screenshot in PNG format and saves it in at `filepath`. (2) `trans_fun=function` uses the transformation function trans_fun to the data before mapping data values to colors and plotting. Popular transformation functions are \code{\link{limit_fun}}, \code{\link{limit_fun_na}}, and \code{\link{clip_fun}}. (3) `text=arglist` calls \code{\link{text3d}} with the given args after plotting. (4) `snapshot_vec=filepath` takes a screenshot in vector format and saves it in at `filepath`. You also need to set the format via `snapshot_vec_format`, valid entries are one of "ps", "eps", "tex", "pdf", "svg", "pgf" (default is 'eps'). This is experimental and may take a while.
#'
#' @return named list, an example `rlgactions` instance that will save a screenshot of the plot produced by the vis function in the current working directory (see \code{getwd}), under the name 'fsbrain_out.png'.
#'
#' @examples
#'    rgla_screenie = list('snapshot_png'='fsbain_out.png');
#'    rgla_screenie = rglactions();   # same as above
#'    rgla_vec_scr = list('snapshot_vec'="~/fsbrain.pdf",
#'      "snapshot_vec_format"="pdf");
#'    rgla_clamp = list('trans_fun'=clip.data); # old style
#'    rgla_clamp = list('trans_fun'=clip_fun(0.05, 0.95)); # new style
#'    rgla_clamp = list('trans_fun'=clip_fun());            # equivalent.
#'    rgla_limit = list('trans_fun'=limit_fun(2,5));
#'    rgla_ls = list('trans_fun'=limit_fun_na(2,5), 'snapshot_png'='~/fig1.png');
#' @export
rglactions <- function() {
    return(list('snapshot_png'='fsbrain_out.png'));
}


