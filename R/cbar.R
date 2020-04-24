# colorbar functions

#' @title Draw coloredbar into background of current plot.
#'
#' @description Requires a rgl 3d visualisation to be open that already contains a rendered object. Uses \code{\link[rgl]{bgplot3d}} to add a colorbar in the background of the plot using \code{\link[fields]{image.plot}}. Experimental.
#'
#' @param coloredmesh fs.coloredmesh as returned by the coloredmesh.from.* functions.
#'
#' @param horizontal logical, whether the colorbar should be drawn in horizontal orientation. Defaults to `TRUE`.
#'
#' @param ... extra params passed to \code{\link[fields]{image.plot}}
#'
#' @importFrom rgl bgplot3d
#' @importFrom squash cmap makecmap
#' @importFrom fields image.plot
#' @keywords internal
draw.colorbar <- function(coloredmeshes, horizontal=FALSE, ...) {
    if(! is.list(coloredmeshes)) {
        stop("Parameter 'coloredmeshes' must be a list.");
    }

    if(length(coloredmeshes) < 1L) {
        warning("Requested to draw colorbar, but mesh list empty. Skipping.");
        return(invisible(NULL));
    }

    colorbar_type = "plain";   # 'fields' or 'squash' or 'plain'
    # Both colorbar functions are not optimal:
    #  - the squash::hkey/vkey one changes its size depending on the number of colors, it seems usable for about 10 colors. It is also ugly as hell.
    #  - when using fields::imageplot, sometimes the colorbar is empty (i.e., it is completely white instead of showing the colors)

    if(colorbar_type == "fields") {

        combined_data_range = coloredmeshes.combined.data.range(coloredmeshes);
        combined_colors = coloredmeshes.combined.colors(coloredmeshes);

        if(is.null(combined_data_range) | is.null(combined_colors)) {
            warning("Requested to draw colorbar, but meshes do not contain the required metadata. Skipping.");
        } else {
            rgl::bgplot3d({plot.new(); fields::image.plot(add=T, graphics.reset=F, legend.only = TRUE, zlim = combined_data_range, col = sort(combined_colors), horizontal = horizontal, ...)});
        }
    } else if(colorbar_type == "squash" | colorbar_type == 'plain') {
        cmap = coloredmeshes.combined.cmap(coloredmeshes);
        if(is.null(cmap)) {
            warning("Requested to draw colorbar, but meshes do not contain the required metadata. Skipping.");
        } else {
            if(horizontal) {
                if(colorbar_type == "squash") {
                    rgl::bgplot3d({plot.new(); squash::hkey(cmap, skip=2L, stretch = 3, x=0, y=0)});
                } else {
                    rgl::bgplot3d({plot.new(); plot.fsbrain.colorbar(cmap$colors, horizontal = horizontal)});
                }
            } else {
                if(colorbar_type == "squash") {
                    rgl::bgplot3d({plot.new(); squash::vkey(cmap, skip=2L, stretch = 3, x=0, y=0)});
                } else {
                    rgl::bgplot3d({plot.new(); plot.fsbrain.colorbar(cmap$colors, horizontal = horizontal)});
                }
            }
        }
    } else {
        warning("Invalid colormap type, skipping.");
    }
}


#' @title Draw a simple colorbar from colors.
#'
#' @param colors vector of colors, no special ordering is assumed
#'
#' @param horizontal logical, whether the colorbar should be plotted horizontally (or vertically).
#'
#' @note This function assumes that there is an open plot, use \code{plot.new()} to create one before calling this function if that is not the case
#'
#' @importFrom graphics plot
#' @importFrom grDevices as.raster
#' @keywords internal
plot.fsbrain.colorbar <- function(colors, horizontal=FALSE) {
    if(horizontal) {
        graphics::plot(t(grDevices::as.raster(cmap$colors)));
    } else {
        graphics::plot(t(grDevices::as.raster(cmap$colors)));
    }
}



#' @title Draw colorbar for coloredmeshes in separate 2D plot.
#'
#' @description Draw a colorbar for the coloredmeshes to a separate 2D plot. Due to the suboptimal handling of colorbar drawing in the three-dimensional multi-panel views, it is often desirable to plot the colorbar in a separate window, export it from there and then manually add it to the final plot version in some image manipulation software like Inkscape. If you need more control over the colormap than offered by this function (e.g., setting the color value for NA values or making a symmetric colormap to ensure that the zero point for divergent colormaps is a neutral color), you should write custom code, and the return value from this function will come in handy to do that.
#'
#' @param coloredmeshes list of coloredmeshes. A coloredmesh is a named list as returned by the `coloredmesh.from` functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh. The `vis*` functions (like \code{\link[fsbrain]{vis.subject.morph.native}}) all return a list of coloredmeshes.
#'
#' @param show logical, Whether to open the resulting plot. Defaults to `TRUE`.
#'
#' @param image.plot_extra_options named list of extra optins to pass to \code{\link[fields]{image.plot}}. This can be used to add a legend to the colorbar, rotate the colorbar, or whatever. The options "legend_only", "zlim", and "col" are computed and set for you  by this function, so there is no need to pass these. Your list will be merged with the internal options, so you could overwrite named arguments if needed.
#'
#' @param png_options Options to pass to \code{\link[grDevices]{png}}, see the docs of that function for details. Allow you to save the plot as a png bitmap image. Example: \code{png_options = list("filename"="outfile.png", "width"=800)}. Defaults to NULL, which will not save anything.
#'
#' @param silent logical, whether to suppress messages. Defaults to `FALSE`.
#'
#' @return named list with the following entries: "full_data": the combined data from all coloredmeshes (can be NULL if they have no data). "colormap": the colormap function from the coloredmeshes (can be NULL if they have none).
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    coloredmeshes = vis.subject.morph.native(subjects_dir, 'subject1',
#'     'thickness', 'lh', views=c('t4'));
#'    coloredmesh.plot.colorbar.separate(coloredmeshes);
#'
#'    # Or plot a colorbar with a label:
#'    coloredmesh.plot.colorbar.separate(coloredmeshes,
#'     image.plot_extra_options = list("legend.lab"="Thickness [mm]",
#'     horizontal=TRUE, legend.cex=1.5, legend.line=-3));
#' }
#'
#' @family colorbar functions
#'
#' @importFrom fields image.plot
#' @importFrom squash cmap makecmap
#' @importFrom graphics plot.new
#' @importFrom grDevices png pdf dev.off
#' @importFrom utils modifyList
#' @export
coloredmesh.plot.colorbar.separate <- function(coloredmeshes, show=TRUE, image.plot_extra_options = list("horizontal"=TRUE), png_options=NULL, silent=FALSE) {

    ret_list = list("full_data"=NULL, "colormap"=NULL);

    if(length(coloredmeshes) < 1) {
        message("Requested to draw separate colorbar, but mesh list is empty. Skipping.");
        return(invisible(ret_list));
    }


    col = coloredmeshes.combined.colors(coloredmeshes);
    data_range = coloredmeshes.combined.data.range(coloredmeshes);

    if(is.null(col) | is.null(data_range)) {
        warning("Requested to draw a colorbar based on meshes, but they do not contain the required metadata, skipping. Make sure the meshes contain colors and a data range.");
        return(ret_list);
    }

    col_sorted = sort(col);

    ret_list$col = col;
    ret_list$col_sorted = col_sorted;

    ret_list$data_range = data_range;
    image.plot_options_internal = list(legend.only=TRUE, zlim=data_range, col = col_sorted, add=TRUE, graphics.reset=TRUE);
    image.plot_options = modifyList(image.plot_options_internal, image.plot_extra_options);
    if(show) {
        plot.new();
        do.call(fields::image.plot, image.plot_options);
    }

    if(! is.null(png_options)) {
        do.call(png, png_options);
        plot.new();
        do.call(fields::image.plot, image.plot_options);
        dev.off();
        if(! is.null(png_options$filename)) {
            if(! silent) {
                message(sprintf("Colorbar image written to file '%s'.\n", png_options$filename));
            }
        }
    }


    return(invisible(ret_list));
}


#' @title Retrieve combined colors range from hemilist of coloredmeshes.
#'
#' @param coloredmeshes hemilist of coloredmeshes
#'
#' @return vector of colors, the combined vertex colors. Note that this is not sorted, and not unique.
#'
#' @keywords internal
coloredmeshes.combined.colors <- function(coloredmeshes) {
    combined_colors = c();
    for(cmesh in coloredmeshes) {
        if(hasIn(cmesh, c('col'))) {
            combined_colors = c(combined_colors, cmesh$col);
        }
    }
    return(combined_colors);
}


#' @title Retrieve combined cmap from hemilist of coloredmeshes.
#'
#' @inheritParams coloredmeshes.combined.colors
#'
#' @return the colormap, generated by \code{\link[squash]{makecmap}}
#'
#' @keywords internal
coloredmeshes.combined.cmap <- function(coloredmeshes) {
    # If any of the coloredmeshes has a map, it is the correct one: when there are 2 meshes, they share a map. Otherwise it is the map for that mesh.
    for(cmesh in coloredmeshes) {
        if(hasIn(cmesh, c('metadata', 'map'))) {
            return(cmesh$metadata$map);
        }
    }
    return(NULL);
}


#' @title Retrieve combined data range from hemilist of coloredmeshes.
#'
#' @inheritParams coloredmeshes.combined.colors
#'
#' @return numeric vector of length 2, the finite data range
#'
#' @keywords internal
coloredmeshes.combined.data.range <- function(coloredmeshes) {
    combined_data = c();
    for(cmesh in coloredmeshes) {
        if(hasIn(cmesh, c('metadata', 'src_data'))) {
            combined_data = c(combined_data, cmesh$metadata$src_data);
        }
    }
    return(range(combined_data, finite=TRUE));
}
