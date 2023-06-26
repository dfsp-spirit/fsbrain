# colorbar functions

#' @title Draw colorebar into background of current plot.
#'
#' @description Requires a rgl 3d visualisation to be open that already contains a rendered object. Uses \code{\link{bgplot3d}} to add a colorbar in the background of the plot using \code{\link[fields]{image.plot}}. Experimental.
#'
#' @param coloredmesh fs.coloredmesh as returned by the coloredmesh.from.* functions.
#'
#' @param horizontal logical, whether the colorbar should be drawn in horizontal orientation. Defaults to `TRUE`.
#'
#' @param ... extra params passed to \code{\link[fields]{image.plot}}
#'
#' @note To adapt or change the colormap, you should use the 'makecmap_options' parameter of the vis.* function used to construct the coloredmeshes (e.g., \code{\link[fsbrain]{vis.subject.morph.native}}).
#'
#' @importFrom rgl bgplot3d
#' @importFrom graphics par
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


    makecmap_options = coloredmeshes.get.md(coloredmeshes, 'makecmap_options');
    if(is.null(makecmap_options)) {
        warning("Requested to draw colorbar, but meshes contain no 'makecmap_options' metadata, falling back to defaults.");
        makecmap_options = mkco.seq();
    }

    if(hasIn(makecmap_options, 'range')) {
        combined_data_range = makecmap_options$range;
        makecmap_options$range = NULL;
        if(length(combined_data_range) != 2) {
            stop("In makecmap_options: 'range' must be a numerical vector of length 2 if given.");
        }
        force_range = TRUE;
    } else {
        combined_data_range = coloredmeshes.combined.data.range(coloredmeshes);
        force_range = FALSE;
    }

    if(can.plot.colorbar(combined_data_range, makecmap_options)) {
        is_symmetric = ifelse(is.null(makecmap_options$symm), FALSE, makecmap_options$symm);
        if(is_symmetric & (!force_range)) {
            zlim = symmrange(combined_data_range)
        } else {
            zlim = combined_data_range;
        }
        num_col = ifelse(is.null(makecmap_options$n), 100L, makecmap_options$n);

        # Check whether to plot in log10 scale.
        if('base' %in% names(makecmap_options)) {
            num_ticks_default = 5L;
            if (as.integer(makecmap_options$base) == 10L) {
                ticks = squash::prettyLog(combined_data_range, n=num_ticks_default);
            } else {
                stop(sprintf(" - Invalid 'base' value in 'makecmap_options', only 10 is supported for log 10 scale. (Do not set it at all for linear scale.)\n"));
            }
            axis.args = list('at'=log(ticks), 'labels'=ticks);
            rgl::bgplot3d({op = graphics::par(mar = rep(0.1, 4)); plot.new(); fields::image.plot(add=T, legend.only = TRUE, zlim = log(zlim), axis.args=axis.args, col = makecmap_options$colFn(num_col), horizontal = horizontal, ...); graphics::par(op);});
        } else {
            # Plot linear scale.
            rgl::bgplot3d({op = graphics::par(mar = rep(0.1, 4)); plot.new(); fields::image.plot(add=T, legend.only = TRUE, zlim = zlim, col = makecmap_options$colFn(num_col), horizontal = horizontal, ...); graphics::par(op);});
        }
    } else {
        warning("Requested to draw colorbar, but meshes do not contain the required metadata. Skipping.");
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
#' @param png_options Options to pass to \code{\link[grDevices]{png}}, see the docs of that function for details. Allow you to save the plot as a png bitmap image. Example: \code{png_options = list("filename"="fsbrain_cbar.png", "width"=800)}. Defaults to NULL, which will not save anything.
#'
#' @param silent logical, whether to suppress messages. Defaults to `FALSE`.
#'
#' @param trim_png logical, whether to trim the output PNG image using image magick, i.e., remove everything but the foreground. Ignored unless an output PNG image is actually written (see 'png_options') and the 'magick' package is installed.
#'
#' @param log_breaks logical, scalar int, or vector of ints. Whether to use log10 scale for plotting the cbar. If logical and TRUE, uses log scale with default number (=5) ticks auto-computed from the data. If a single integer N, uses N ticks auto-computed from the data instead. If a numeric vector, uses the supplied values in the vector as ticks, note that they must be on a `log(data)` scale. If the 'makecmap_options' stored in the passed 'coloredmeshes' contain a 'base' value of 10, log 10 is assumed (with the default 5 ticks), even if this parameter is left at its default value, logical FALSE.
#'
#' @note If you increase the output resolution of the colorbar (using 'png_options'), you will have to increase the font sizes as well (using 'image.plot_extra_options'), otherwise the axis and legend labels will be hard to read.
#'
#' @examples
#' \dontrun{
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
#' @return named list, entries: 'output_img_path': character string, the path to the output file, or NULL.
#'
#' @importFrom fields image.plot
#' @importFrom squash cmap makecmap
#' @importFrom graphics plot.new
#' @importFrom grDevices png pdf dev.off
#' @importFrom utils modifyList
#' @export
coloredmesh.plot.colorbar.separate <- function(coloredmeshes, show=FALSE, image.plot_extra_options = list(horizontal=FALSE, 'legend.cex'=1.8, 'legend.width'=2, 'legend.mar' = 12, 'axis.args'=list('cex.axis'=5.0)), png_options=list('filename'='fsbrain_cbar.png', 'width'=1400, 'height'=1400, 'bg'='#FFFFFF00'), silent=FALSE, trim_png=TRUE, log_breaks=FALSE) {

    if(length(coloredmeshes) < 1) {
        message("Requested to draw separate colorbar, but mesh list is empty. Skipping.");
        return(invisible(NULL));
    }

    makecmap_options = coloredmeshes.get.md(coloredmeshes, 'makecmap_options');

    if(is.null(makecmap_options)) {
        warning("Requested to draw colorbar, but meshes contain no 'makecmap_options' metadata, falling back to defaults.");
        makecmap_options = mkco.seq();
    }
    if(hasIn(makecmap_options, 'range')) {
        combined_data_range = makecmap_options$range;
        makecmap_options$range = NULL;
        if(length(combined_data_range) != 2) {
            stop("In makecmap_options: 'range' must be a numerical vector of length 2 if given.");
        }
        force_range = TRUE;
    } else {
        combined_data_range = coloredmeshes.combined.data.range(coloredmeshes);
        force_range = FALSE;
    }

    if(! can.plot.colorbar(combined_data_range, makecmap_options)) {
        warning("Requested to draw a colorbar based on meshes, but they do not contain the required metadata, skipping.");
        return(invisible(NULL));
    }

    is_symmetric = ifelse(is.null(makecmap_options$symm), FALSE, makecmap_options$symm);
    if(is_symmetric & !(force_range)) {
        zlim = symmrange(combined_data_range)
    } else {
        zlim = combined_data_range;
    }

    num_col = ifelse(is.null(makecmap_options$n), 100L, makecmap_options$n);  # for small meshes with < 100 verts, the user will have to set n to <= num_verts

    image.plot_options_internal = list(legend.only=TRUE, zlim = zlim, col = makecmap_options$colFn(num_col), add=TRUE, graphics.reset=TRUE);
    image.plot_options = modifyList(image.plot_options_internal, image.plot_extra_options);

    # Enable plotting log-scale color bar.
    # NOTE: Use 'area' instead of 'thickness' as PVD when testing this with the code from the example, thickness has a min of 0 (won't work with log).
    if((is.logical(log_breaks) && log_breaks) ||  is.numeric(log_breaks) || 'base' %in% names(makecmap_options)) {
        num_ticks_default = 5L;
        if(is.logical(log_breaks) && log_breaks) {
            log_breaks = num_ticks_default;  # If simply 'True', use default number of breaks = 5.
        }

        if(is.numeric(log_breaks) && length(log_breaks) == 1L) { # If a single number, treat it as the *number* of ticks to create.
            num_break_labels = as.integer(log_breaks);
            ticks = squash::prettyLog(combined_data_range, n=num_break_labels);
        } else if(is.numeric(log_breaks) && length(log_breaks) > 1L) {  # If a vector, treat the values in there as the desired ticks.
            ticks = log_breaks;
        } else {
            if (as.integer(makecmap_options$base) == 10L) {
                ticks = squash::prettyLog(combined_data_range, n=num_ticks_default);
            } else {
                stop(sprintf(" - Invalid 'base' value in 'makecmap_options', only 10 is supported for log 10 scale. (Do not set it at all for linear scale.)"));
            }

        }

        tick_axis_args = list('at'=log(ticks), 'labels'=ticks)
        if ("axis.args" %in% names(image.plot_options)) {
            image.plot_options$axis.args = modifyList(image.plot_options$axis.args, tick_axis_args);
        } else {
            image.plot_options$axis.args = tick_axis_args;
        }
        image.plot_options$zlim = log(image.plot_options$zlim);
    }

    if(show) {
        plot.new();
        do.call(fields::image.plot, image.plot_options);
    }

    is_horizontal = FALSE;
    if('horizontal' %in% names(image.plot_options)) {
        is_horizontal = image.plot_options$horizontal;
    }

    if(! is.null(png_options)) {
        do.call(png, png_options);
        plot.new();
        do.call(fields::image.plot, image.plot_options);
        dev.off();
        if(! is.null(png_options$filename)) {
            if(! silent) {
                orientation_string = ifelse(is_horizontal, 'Horizontal', 'Vertical');
                message(sprintf("%s colorbar image written to file '%s'.\n", orientation_string, png_options$filename));
            }
            if(trim_png) {
                if (requireNamespace("magick", quietly = TRUE)) {
                    cbar_img = magick::image_read(png_options$filename);
                    cbar_img_trimmed = magick::image_trim(cbar_img);
                    magick::image_write(cbar_img_trimmed, path = png_options$filename);
                } else {
                    warning("Ignored request to trim colorbar image: this functionality requires the 'magick' package.");
                }
            }
        }
    } else {
        png_options = list('filename'=NULL);
    }

    return(invisible(list('output_img_path'=png_options$filename)));
}


#' @title Retrieve metadata from hemilist of coloredmeshes.
#'
#' @param coloredmeshes hemilist of coloredmeshes
#'
#' @param mdname the key in the named metadata list
#'
#' @return the metadata value at the given key/mdname
#'
#' @keywords internal
coloredmeshes.get.md <- function(coloredmeshes, mdname) {
    for(cmesh in coloredmeshes) {
        if(hasIn(cmesh, c('metadata', mdname))) {
            return(cmesh$metadata[[mdname]]);
        }
    }
    return(NULL);
}


#' @title Retrieve combined data range from hemilist of coloredmeshes.
#'
#' @inheritParams coloredmeshes.get.md
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


#' @title Create a separate legend plot for a colortable or an annotation.
#'
#' @description This plots a legend for a colortable or an atlas (annotation), showing the region names and their assigned colors. This function creates a new plot.
#'
#' @param colortable dataframe, a colortable as returned by \code{\link[freesurferformats]{read.fs.colortable}} or the inner 'colortable_df' returned by \code{\link[fsbrain]{subject.annot}}. One can also pass an annotation (*fs.annot* instance).
#'
#' @param ncols positive integer, the number of columns to use when plotting
#'
#' @param plot_struct_index logical, whether to plot the region index from tge 'struct_index' field. If there is no such field, this is silently ignored.
#'
#' @note This function plots one or more legends (see \code{\link[graphics]{legend}}). You may have to adapt the device size before calling this function if you inted to plot a large colortable.
#'
#' @examples
#' \dontrun{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    annot = subject.annot(subjects_dir, 'subject1', 'lh', 'aparc');
#'    vis.colortable.legend(annot$colortable_df, ncols=3);
#' }
#'
#' @importFrom graphics legend par plot.new plot
#' @importFrom grDevices rgb
#' @export
vis.colortable.legend <- function(colortable, ncols=1L, plot_struct_index=TRUE) {

    if(! is.data.frame(colortable)) {
        if(freesurferformats::is.fs.annot(colortable)) {
            # An annotation was passed, extract the colortable from it.
            colortable = colortable$colortable_df;
        } else {
            stop("Parameter 'colortable' must be a colortable data.frame or a loaded annotation (fs.annot).");
        }
    }

    if(is.null(colortable$hex_color_string_rgb)) {
        colortable$hex_color_string_rgb = grDevices::rgb(colortable$r/255., colortable$g/255., colortable$b/255.);
    }

    graphics::plot.new();
    num_regions = nrow(colortable);               # the total number of regions in the colortable
    n_per_legend = ceiling(num_regions / ncols);  # we may plot several legends, this is how many regions are listed in each legend.

    graphics::plot.new();
    graphics::par(mfrow = c(1L, ncols));     # create a multi-plot drawing area consisting of 3x1 cells
    #cat(sprintf("Using %d columns, %d entries per legend/column\n", ncols, n_per_legend))

    for(legend_idx in seq.int(ncols)) {
        start_idx = ((legend_idx -1L) * n_per_legend) + 1L;
        end_idx = min(start_idx + n_per_legend -1L, num_regions);
        #cat(sprintf("- Plotting from %d to %d in column # %d\n", start_idx, end_idx, legend_idx))

        graphics::plot(1, type="n", axes=FALSE, xlab="", ylab=""); # Create empty plot, this is needed to switch to the next cell
        legend_text = colortable$struct_name[start_idx:end_idx];
        if(plot_struct_index & ! is.null(colortable$struct_index)) {
            legend_text = paste(colortable$struct_index[start_idx:end_idx], legend_text);
        }
        graphics::legend("topleft", legend=legend_text, fill=colortable$hex_color_string_rgb[start_idx:end_idx], bty = "n");
    }
}


#' @title Plot legend for a brain volume segmentation based on colorLUT.
#'
#' @param colortable a colortable data.frame, or a character string, which will be treated as a filename and loaded with \code{\link[freesurferformats]{read.fs.colortable}}. Typically \code{FS_HOME/FreeSurferColorLUT.txt}.
#'
#' @param segvol optional 3D or 4D array of integer data, the brain segmentation. Or a character string, which will be treated as a filename and loaded with \code{\link[freesurferformats]{read.fs.volume}}. If given, only colortable entries which actually occur in the volume data are plotted. If \code{NULL}, all entries are plotted, which may be a lot.
#'
#' @param ... passed on to \link{vis.colortable.legend}
#'
#' @examples
#' \dontrun{
#' ct = file.path(fs.home(), "FreeSurferColorLUT.txt");
#' seg = file.path(fs.home(), "subjects", "fsaverage", "mri", "aseg.mgz");
#' vis.seg.legend(ct, seg);
#'
#' }
#'
#' @export
vis.seg.legend <- function(colortable, segvol, ...) {
    if(is.character(colortable)) {
        colortable = freesurferformats::read.fs.colortable(colortable);
    }
    if(! is.null(segvol)) {
        if(is.character(segvol)) {
            segvol = freesurferformats::read.fs.volume(segvol);
        }
        seg_regions = unique(as.integer(segvol));
        colortable = colortable[colortable$struct_index %in% seg_regions, ];
    }
    vis.colortable.legend(colortable, ...);
}


#' @title Determine whether colorbar can be plotted with given metadata.
#'
#' @param combined_data_range numerical vector of length 2, the combined data range of the meshes as returned by \code{coloredmeshes.combined.data.range}
#'
#' @param makecmap_options the 'makecmap_options' from the metadata field of the 'coloredmeshes', see \code{coloredmeshes.get.md}
#'
#' @return logical, whether the metadata suffices to plot a colorbar
#'
#' @keywords internal
can.plot.colorbar <- function(combined_data_range, makecmap_options) {
    if(is.null(combined_data_range) | is.null(makecmap_options)) {
        return(FALSE);
    }
    if( ! is.function(makecmap_options$colFn)) {
        return(FALSE);
    }
    return(TRUE);
}


#' @title Determine whether colorbar can be plotted with given coloredmeshes.
#'
#' @param coloredmeshes hemilist of coloredmeshes
#'
#' @return logical, whether the metadata suffices to plot a colorbar
#'
#' @keywords internal
can.plot.colorbar.from.coloredmeshes <- function(coloredmeshes) {
    combined_data_range = coloredmeshes.combined.data.range(coloredmeshes);
    makecmap_options = coloredmeshes.get.md(coloredmeshes, 'makecmap_options');
    return(can.plot.colorbar(combined_data_range, makecmap_options));
}


#' @title Given data, compute symmetric range around zero.
#'
#' @param x the data, could be a range of course.
#'
#' @keywords internal
symmrange <- function(x) {
    dmin = min(x, na.rm = T);
    dmax = max(x, na.rm = T);
    abs_max = max(abs(dmin), abs(dmax));
    return(c(-abs_max, abs_max));
}


#' @title Draw a simple colorbar from colors.
#'
#' @param colors vector of colors, no special ordering is assumed
#'
#' @param horizontal logical, whether the colorbar should be plotted horizontally (or vertically).
#'
#' @note This function assumes that there is an open plot, use \code{plot.new()} to create one before calling this function if that is not the case.
#'
#' @importFrom graphics plot box
#' @importFrom grDevices as.raster
#' @keywords internal
plot.fsbrain.colorbar <- function(colors, horizontal=FALSE) {
    if(horizontal) {
        graphics::plot(t(grDevices::as.raster(colors)));
    } else {
        graphics::plot(grDevices::as.raster(colors));
    }
}


#' @title Return the standard fsbrain sequential colormap.
#'
#' @param report logical, whether to print a message with a name of the chosen colormap, in format \code{package::function#palette}.
#'
#' @note This returns a sequential, multi-hue palette.
#'
#' @export
cm.seq <- function(report=FALSE) {
    if(requireNamespace('grDevices', quietly = TRUE)) {
        if(exists('hcl.colors')) {
            if(report) { message('grDevices::hcl.colors#viridis'); }
            return(function(n) { grDevices::hcl.colors(n, palette = "viridis"); });
        }
    }
    if(requireNamespace('viridis', quietly = TRUE)) {
        if(report) { message('viridis::viridis#viridis'); }
        return(viridis::viridis);
    }
    if(requireNamespace('RColorBrewer', quietly = TRUE)) {
        if(report) { message('RColorBrewer::brewer.pal#YlGn'); }
        return(grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, name="YlGn")));
    }
    if(report) { message('viridis::viridis#viridis'); }
    return(viridis::viridis);
}


#' @title Return the standard fsbrain heat colormap.
#'
#' @inheritParams cm.seq
#'
#' @note The heat palette is a sequential, single-hue palette.
#'
#' @export
cm.heat <- function(report=FALSE) {
    if(requireNamespace('grDevices', quietly = TRUE)) {
        if(exists('hcl.colors')) {
            if(report) { message('grDevices::hcl.colors#YlOrRd'); }
            return(function(n) { grDevices::hcl.colors(n, palette = "YlOrRd"); });
        }
    }
    if(report) { message('grDevices::heat.colors#heat.colors'); }
    return(grDevices::heat.colors);
}


#' @title Get cyan blue red yellow colormap function.
#'
#' @note Returns a diverging palette with negative values in blue/cyan and positive ones in red/yellow, suitable for visualizing data that is centered around zero. Often used for clusters in neuroscience.
#'
#' @export
cm.cbry <- function() {
    # cyan (rgb=0,1,1), blue (0,0,1), red (1,0,0), yellow (1,1,0):
    step_colors = rgb(matrix(c(0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0), ncol = 3, byrow = FALSE));
    return(grDevices::colorRampPalette(step_colors));
}


#' @title Return the standard fsbrain diverging colormap.
#'
#' @inheritParams cm.seq
#'
#' @note Returns some diverging palette, suitable for visualizing data that is centered around zero.
#'
#' @export
cm.div <- function(report=FALSE) {
    if(requireNamespace('grDevices', quietly = TRUE)) {
        if(exists('hcl.colors')) {
            if(report) { message('grDevices::hcl.colors#Blue-Red 3'); }
            return(function(n) { grDevices::hcl.colors(n, palette = "Blue-Red 3"); });
        }
    }
    if(requireNamespace('RColorBrewer', quietly = TRUE)) {
        if(report) { message('RColorBrewer::brewer.pal#RdBu'); }
        return(grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, name="RdBu")));
    }
    if(report) { message('grDevices::cm.colors#cm.colors'); }
    return(grDevices::cm.colors);
}


#' @title Return the standard fsbrain qualitative colormap.
#'
#' @inheritParams cm.seq
#'
#' @note Returns some qualitative palette, suitable for visualizing categorical data.
#'
#' @export
cm.qual <- function(report=FALSE) {
    if(requireNamespace('grDevices', quietly = TRUE)) {
        if(exists('hcl.colors')) {
            if(report) { message('grDevices::hcl.colors#Dark 3'); }
            return(function(n) { grDevices::hcl.colors(n, palette = "Dark 3"); });
        }
    }
    if(requireNamespace('RColorBrewer', quietly = TRUE)) {
        if(report) { message('RColorBrewer::brewer.pal#Dark2'); }
        return( function(n) {
                if(n <= 11L) {
                    return(RColorBrewer::brewer.pal(n, name="Dark2"));
                } else {
                    return(grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, name="Dark2")));
                }
            }
        );
    }
    if(report) { message('grDevices::cm.colors#cm.colors'); }
    return(grDevices::cm.colors);
}


#' @title Return recommended 'makecmap_options' for sequential data.
#'
#' @description This function returns recommended visualization settings (a colormap function and suitable other settings) for the given type of data. The return value is meant to be passed as parameter 'makecmap_options' to the vis.* functions, e.g., \code{\link[fsbrain]{vis.subject.morph.native}}.
#'
#' @return named list, visualization settings to be used as 'makecmap_options' for sequential data.
#'
#' @export
mkco.seq <- function() {
    return(list('colFn'=cm.seq(), 'n'=100L, 'col.na'='#FEFEFE'));
}


#' @title Return recommended 'makecmap_options' for sequential data with heatmap style.
#'
#' @description This function returns recommended visualization settings (a colormap function and suitable other settings) for the given type of data. The return value is meant to be passed as parameter 'makecmap_options' to the vis.* functions, e.g., \code{\link[fsbrain]{vis.subject.morph.native}}.
#'
#' @return named list, visualization settings to be used as 'makecmap_options' for sequential data with heatmap style.
#'
#' @export
mkco.heat <- function() {
    return(list('colFn'=cm.heat(), 'n'=100L, 'col.na'='#FEFEFE'));
}


#' @title Return recommended 'makecmap_options' for diverging data.
#'
#' @description This function returns recommended visualization settings (a colormap function and suitable other settings) for the given type of data. The return value is meant to be passed as parameter 'makecmap_options' to the vis.* functions, e.g., \code{\link[fsbrain]{vis.subject.morph.native}}.
#'
#' @return named list, visualization settings to be used as 'makecmap_options' for diverging data.
#'
#' @export
mkco.div <- function() {
    return(list('colFn'=cm.div(), 'n'=100L, 'symm'=TRUE, 'col.na'='#FEFEFE'));
}


#' @title Return recommended 'makecmap_options' for diverging cluster data.
#'
#' @description This function returns recommended visualization settings (a colormap function and suitable other settings) for the given type of data. The return value is meant to be passed as parameter 'makecmap_options' to the vis.* functions, e.g., \code{\link[fsbrain]{vis.subject.morph.native}}.
#'
#' @return named list, visualization settings to be used as 'makecmap_options' for diverging data.
#'
#' @note This uses a cyan blue red yellow colormap, which is popular for displaying clusters in neuroscience.
#'
#' @export
mkco.cluster <- function() {
    return(list('colFn'=cm.cbry(), 'n'=100L, 'symm'=TRUE, 'col.na'='#FEFEFE'));
}
