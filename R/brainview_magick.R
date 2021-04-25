# Functions that use the 'magick' package (ImageMagick) to combine separate brainview images into one image.
# Currently 'magick' is an optional dependency only.


#' @title Combine several brainview images into a new figure.
#'
#' @description Create a new image from several image tiles, the exact layout depends on the number of given images.
#'
#' @param brainview_images vector of character strings, paths to the brainview images, usually in PNG format
#'
#' @param output_img path to output image that including the file extension
#'
#' @param colorbar_img path to the main image containing the separate colorbar, usually an image in PNG format
#'
#' @param silent logical, whether to suppress messages
#'
#' @param grid_like logical, whether to arrange the images in a grid-like fashion. If FALSE, they will all be merged horizontally.
#'
#' @param border_geometry string, a geometry string passed to \code{magick::image_border} to define the borders to add to each image tile. The default value adds 5 pixels, both horizontally and vertically.
#'
#' @param background_color hex color string, such as "#DDDDDD" or "#FFFFFF". The color to use when extending images (e.g., when creating the border). WARNING: Do not use color names (like 'gray'), as their interpretation differs between rgl and image magick!
#'
#' @param map_bg_to_transparency logical, whether to map the background_color to transparency for the final PNG export.
#'
#' @return named list with entries: 'brainview_images': vector of character strings, the paths to the input images. 'output_img_path': character string, path to the output image. 'merged_img': the magick image instance.
#'
#' @export
arrange.brainview.images <- function(brainview_images, output_img, colorbar_img=NULL, silent=TRUE, grid_like=TRUE, border_geometry="5x5", background_color = "white", map_bg_to_transparency = FALSE) {

    if (requireNamespace("magick", quietly = TRUE)) {

        # load image files
        images = magick::image_read(brainview_images);

        # trim images
        images = magick::image_trim(images);

        # Add tiny border back (to prevent them from touching each other)
        images = magick::image_border(images, background_color, border_geometry);
        num_img = length(images);

        if(grid_like) {
            if(num_img == 3 || num_img == 4) {
                top_row = wrapped.image.append(images[1:2], background_color = background_color);
                bottom_row = wrapped.image.append(images[3:num_img], background_color = background_color);
                merged_img = wrapped.image.append(c(top_row, bottom_row), stack = TRUE, background_color = background_color);
            } else if(num_img == 8 || num_img == 9) {
                top_row = wrapped.image.append(images[1:3], background_color = background_color);
                mid_row = wrapped.image.append(images[4:6], background_color = background_color);
                top_and_mid = wrapped.image.append(c(top_row, mid_row), stack = TRUE, background_color = background_color);

                if(num_img == 9) {
                    bottom_row = wrapped.image.append(images[7:num_img], background_color = background_color);
                } else {
                    # For 8 images, it gets a bit trickier. It looks very bad if the two
                    # images are on the left and the right is empty, so we add an empty
                    # image of appropriate size between them.
                    width_top_and_mid = magick::image_info(top_and_mid)$width;
                    bottom_row_so_far = wrapped.image.append(images[7:num_img], background_color = background_color);
                    width_bottom_so_far = magick::image_info(images[7])$width + magick::image_info(images[8])$width;
                    width_missing = width_top_and_mid - width_bottom_so_far;
                    if(width_missing > 0L) {
                        mid = magick::image_blank(width_missing, magick::image_info(bottom_row_so_far)$height, background_color);
                        bottom_row = wrapped.image.append(c(images[7], mid, images[8]), background_color = background_color);
                    } else {
                        bottom_row = bottom_row_so_far;
                    }
                }

                merged_img = wrapped.image.append(c(top_and_mid, bottom_row), stack = TRUE, background_color = background_color);
            } else {
                if(num_img <= 10L) {
                    merged_img = wrapped.image.append(images, background_color = background_color);
                } else {
                    # For more than 10 images, plot 10 per row.
                    num_per_row = 10L;
                    num_rows = as.integer(ceiling(num_img / num_per_row));
                    num_in_last_row = (num_rows * num_per_row) %% num_img;
                    start_img_idx = 1L;
                    for(row_idx in seq.int(num_rows)) {
                        img_row = wrapped.image.append(images[start_img_idx:min((start_img_idx+num_per_row-1L),num_img)], background_color = background_color);
                        if(row_idx == 1L) {
                            merged_img = img_row;
                        } else {
                            width_so_far = magick::image_info(merged_img)$width;
                            width_img_row = magick::image_info(img_row)$width;
                            width_missing = width_so_far - width_img_row;
                            if(width_missing > 0L) {
                                blank_right_img = magick::image_blank(width_missing, magick::image_info(img_row)$height, background_color);
                                img_row = wrapped.image.append(c(img_row, blank_right_img), background_color = background_color);
                            }
                            merged_img = wrapped.image.append(c(merged_img, img_row), stack = TRUE, background_color = background_color);
                        }
                        start_img_idx = start_img_idx + num_per_row;
                    }
                }
            }
        } else {
            merged_img = wrapped.image.append(images, background_color = background_color);
        }

        if(map_bg_to_transparency) {
            merged_img = image.remap.color(merged_img, source_color=background_color, source_point = NULL);
        }

        magick::image_write(merged_img, path = output_img);
        if(! silent) {
            message(sprintf("Merged image written to '%s' (current working dir is '%s').\n", output_img, getwd()));
        }
        return(invisible(list('merged_img'=merged_img, 'brainview_images'=brainview_images, 'output_img_path'=output_img)));

    } else {
        warning("The 'magick' package must be installed to use this functionality. Merged image NOT written.");
        return(invisible(NULL));
    }
}


#' @title Remap a color in an image, typically used to set the background color to transparent.
#'
#' @description Offers 2 algorithm: remap color by flood-filling from a given pixel, or remap a hardcoded color throughout the entire image. Provide one of 'source_color' or 'source_point' by setting the other to NULL. If both are given, source_color takes precedence and source_point is silently ignored.
#'
#' @param source_color the source color that should be replaced in the whole image. Set to NULL to disable.
#'
#' @param source_point the source pixel in which to start the flood filling. Set to NULL to disable.
#'
#' @param target_color an image magick color string, use 'none' for transparency. Only used with flood fill.
#'
#' @keywords internal
image.remap.color <- function(source_img, source_color=NULL, source_point="+1+1", target_color="none") {
    if(is.null(source_color)) {
        if(is.null(source_point)) {
            stop("One of 'source_color' or 'source_point' must be provided.");
        } else {
            remapped_img = magick::image_fill(source_img, target_color, point = source_point, fuzz = 0);
        }
    } else {
        remapped_img = magick::image_transparent(source_img, source_color, fuzz = 0);
    }

    return(remapped_img);
}


#' @title Combine several brainview images as a grid into a new figure.
#'
#' @description Create a new image from several image tiles, the exact layout is a grid with n per row.
#'
#' @inheritParams arrange.brainview.images
#'
#' @param output_img path to output image that including the file extension
#'
#' @param num_per_row positive integer, the number of image tiles per row.
#'
#' @param captions vector of character strings or NULL, the (optional) text annotations for the images. Useful to print the subject identifier onto the individual tiles. Length must match number of image tiles in 'brainview_images'.
#'
#' @return named list with entries: 'brainview_images': vector of character strings, the paths to the input images. 'output_img_path': character string, path to the output image. 'merged_img': the magick image instance.
#'
#' @note The tiles are written row-wise, in the order in which they occur in the parameter 'brainview_images'.
#'
#' @export
arrange.brainview.images.grid <- function(brainview_images, output_img, colorbar_img=NULL, silent=TRUE, num_per_row=10L, border_geometry="5x5", background_color = "white", captions=NULL) {
    if (requireNamespace("magick", quietly = TRUE)) {

        # load image files
        images = magick::image_read(brainview_images);

        # trim images
        images = magick::image_trim(images);

        # Add tiny border back (to prevent them from touching each other)
        images = magick::image_border(images, background_color, border_geometry);
        num_img = length(images);

        images = images.rescale.to.max.canvas(images, background = background_color);

        # annotate if requested
        images = images.annotate(images, captions, background = background_color);



        num_rows = as.integer(ceiling(num_img / num_per_row));
        num_in_last_row = (num_rows * num_per_row) %% num_img;
        start_img_idx = 1L;
        for(row_idx in seq.int(num_rows)) {
            row_start_idx = start_img_idx;
            row_end_idx = min((start_img_idx+num_per_row-1L),num_img);
            img_row = wrapped.image.append(images[row_start_idx:row_end_idx], background_color = background_color);

            if(row_idx == 1L) {
                merged_img = img_row;
            } else {
                width_so_far = magick::image_info(merged_img)$width;
                width_img_row = magick::image_info(img_row)$width;
                width_missing = width_so_far - width_img_row;
                if(width_missing > 0L) {
                    blank_right_img = magick::image_blank(width_missing, magick::image_info(img_row)$height, background_color);
                    img_row = wrapped.image.append(c(img_row, blank_right_img), background_color = background_color);
                }
                merged_img = wrapped.image.append(c(merged_img, img_row), stack = TRUE, background_color = background_color);
            }
            start_img_idx = start_img_idx + num_per_row;
        }

        magick::image_write(merged_img, path = output_img);
        if(! silent) {
            message(sprintf("Merged image written to '%s' (current working dir is '%s').\n", output_img, getwd()));
        }
        return(invisible(list('merged_img'=merged_img, 'brainview_images'=brainview_images, 'output_img_path'=output_img)));
    } else {
        warning("The 'magick' package must be installed to use this functionality. Merged image NOT written.");
        return(invisible(NULL));
    }
}

#' @title Rescale all images canvas to match the largest one.
#'
#' @param images vector of magick images
#'
#' @param background color string, like 'white' or '#00FF00'
#'
#' @return vector of magick images, the rescaled images
#'
#' @keywords internal
images.rescale.to.max.canvas <- function(images, background="white") {
    if (requireNamespace("magick", quietly = TRUE)) {
        num_img = length(images);
        # Determine max tile heigth and width to resize canvas of all tiles, so all images have the same width and height.
        tile_widths = rep(NA, num_img);
        tile_heights = rep(NA, num_img);
        for(img_idx in seq.int(num_img)) {
            tile_widths[img_idx] = magick::image_info(images[img_idx])$width;
            tile_heights[img_idx] = magick::image_info(images[img_idx])$height;
        }
        max_tile_width = max(tile_widths);
        max_tile_height = max(tile_heights);
        #cat(sprintf("The min and max tile widths are %d and %d.\n", min(tile_widths), max_tile_width));
        #cat(sprintf("The min and max tile heights are %d and %d.\n", min(tile_heights), max_tile_height));

        geom_string = sprintf("%dx%d", max_tile_width, max_tile_height);

        #cat(sprintf("Using geom string '%s' for the %d tiles.\n", geom_string, num_img));

        imgs_rescaled = images;
        for(img_idx in seq.int(num_img)) {
            imgs_rescaled[img_idx] = magick::image_extent(images[img_idx], geom_string, color = background);
        }
        return(imgs_rescaled);

    } else {
        warning("The 'magick' package must be installed to use this functionality.");
        return(invisible(NULL));
    }
}


#' @title Annotate image with text.
#'
#' @inheritParams images.rescale.to.max.canvas
#'
#' @param annotations vector of character strings, the strings to print onto the tiles
#'
#' @param do_extend logical, whether to add the space for the annotation text below the existing image tile
#'
#' @return vector of magick images, the annotated images
#'
#' @keywords internal
images.annotate <- function(images, annotations, do_extend = TRUE, background = 'white') {

    if (requireNamespace("magick", quietly = TRUE)) {
        num_img = length(images);

        if(is.null(annotations)) {
            return(images);
        }
        annotations = recycle(annotations, num_img);
        imgs_annotated = images;
        for(img_idx in seq.int(num_img)) {
            font_size = 30L;
            if(do_extend) {
                extend_height_by = font_size * 2L;
                lower_extension_img = magick::image_blank(magick::image_info(images[img_idx])$width, extend_height_by, background);
                merged_img = wrapped.image.append(c(images[img_idx], lower_extension_img), stack = TRUE, background_color = background);
            } else {
                merged_img = images[img_idx];
            }

            font_color = "black";
            if(background == "black" || background == "#000000") {
                font_color = "white";
            }
            imgs_annotated[img_idx] = magick::image_annotate(merged_img, annotations[img_idx], size = font_size, gravity = "south", color = font_color);
        }
        return(imgs_annotated);
    } else {
        warning("The 'magick' package must be installed to use this functionality.");
        return(invisible(NULL));
    }
}


#' @title Visualize coloredmeshes from several angles and combine the images into a new figure.
#'
#' @description Create a tight layout view of coloredmeshes from several angles. Creates separate `sd_<angle>` images, then crops and finally merges them into a single output image with image magick. The `coloredmeshes` to pass to this function are usually obtained by running any `vis*` function (like \code{\link[fsbrain]{vis.subject.morph.native}}, \code{\link[fsbrain]{vis.subject.morph.standard}}, \code{\link[fsbrain]{vis.subject.label}}, \code{\link[fsbrain]{vis.subject.annot}}, and others). That means you can use this function to visualize all kinds of data, e.g., morphometry data in native and standard space, labels, and brain atlases.
#
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the `coloredmesh.from*` functions (like \code{\link{coloredmesh.from.morph.native}}). It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh. The `vis*` functions (like \code{\link[fsbrain]{vis.subject.morph.native}}) all return a list of coloredmeshes.
#'
#' @param view_angles list of strings. See \code{\link{get.view.angle.names}} for all valid strings.
#'
#' @param rgloptions option list passed to \code{\link{par3d}}. Example: \code{rgloptions = list("windowRect"=c(50,50,1000,1000))}.
#'
#' @param rglactions named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @param style character string, a rendering style, e.g., 'default', 'shiny' or 'semitransparent'. Alternatively, a named list of style parameters (see \code{\link{material3d}}), e.g., \code{list("shininess"=50, specular="black", alpha=0.5)}. Use the magic word 'from_mesh' to use the 'style' field of each coloredmesh instead of a single, global style. In that case, you will have to make sure your meshes have such a field, if not, the style 'default' is used as a fallback for those which don't.
#'
#' @param output_img string, path to the output file. Defaults to "fsbrain_arranged.png"
#'
#' @param silent logical, whether to suppress all messages
#'
#' @param grid_like logical, whether to arrange the images in a grid-like fashion. If FALSE, they will all be merged horizontally. Passed to \code{\link[fsbrain]{arrange.brainview.images}}.
#'
#' @param background_color hex color string (like '#FFFFFF'), the color to use for the background. Ignored if 'transparency_color' is not NULL. To get a transparent background, use 'transparency_color' instead of this parameter. WARNING: Do not use color names (like 'gray'), as their interpretation differs between rgl and image magick!
#'
#' @param transparency_color hex color string (like '#FFFFFF'), the temporary background color that will get mapped to transparency, or NULL if you do not want a transparent background. If used, it can be any color that does not occur in the foreground. Try '#FFFFFF' (white) or '#000000' (black) if in doubt. WARNING: Do not use color names (like 'gray'), as their interpretation differs between rgl and image magick!
#'
#' @return named list, see \code{\link{arrange.brainview.images}} for details
#'
#' @examples
#' \dontrun{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    # Use any vis function to get coloredmeshes. You can visualize morphometry,
#'    #  labels, an atlas, whatever. You can suppress the view unless you need it.
#'    coloredmeshes = vis.subject.morph.native(subjects_dir, "subject1", "thickness",
#'     cortex_only=TRUE, rglactions=list("clip_data"=c(0.05, 0.95)),
#'     views=NULL);
#'    # The meshes contain the surface, data, and color information and can be
#'    #  visualized. You could adapt the rendering style while doing so:
#'    vislayout.from.coloredmeshes(coloredmeshes, style='shiny');
#'    # You could change the rendering style on a per-mesh basis.
#'    coloredmeshes[[1]]$style = list("shininess"=50, alpha=0.5);
#'    vislayout.from.coloredmeshes(coloredmeshes, style='from_mesh');
#' }
#'
#'
#' @family visualization functions
#' @export
vislayout.from.coloredmeshes <- function(coloredmeshes, view_angles=get.view.angle.names(angle_set = "t4"), rgloptions = rglo(), rglactions=list(), style="default", output_img="fsbrain_arranged.png", silent=FALSE, grid_like=TRUE, background_color = "white", transparency_color=NULL) {

    if (requireNamespace("magick", quietly = TRUE)) {
        view_images = tempfile(view_angles, fileext = ".png");   # generate one temporary file name for each image

        map_bg_to_transparency = FALSE;
        if(! is.null(transparency_color)) {
            map_bg_to_transparency = TRUE;
            if(background_color != "white") {
                warning("Parameter 'transparency_color' is set, ignoring custom value for parameter 'background_color'.");
            }
            background_color = transparency_color;
        }

        # Create the temporary images at the temp paths
        for(view_idx in seq_len(length(view_angles))) {
            view = view_angles[[view_idx]];
            view_image = view_images[[view_idx]];

            internal_rglactions = list("snapshot_png"=view_image);
            if(rglactions.has.key(rglactions, "snapshot_png")) {
                warning("The key 'snapshot_png' in the 'rglactions' parameter is not supported for this function, it will be ignored. Use 'output_img' instead.");
                rglactions$snapshot_png = NULL;
            }
            final_rglactions = modifyList(rglactions, internal_rglactions);

            brainviews(c(view), coloredmeshes, rgloptions = rgloptions, rglactions = final_rglactions, style = style, background = background_color);
        }

        # Now merge them into one
        return(invisible(arrange.brainview.images(view_images, output_img, silent=silent, grid_like=grid_like, background_color = background_color, map_bg_to_transparency = map_bg_to_transparency)));
    } else {
        warning("The 'magick' package must be installed to use this functionality. Image with manual layout NOT written.");
        return(invisible(NULL));
    }
}


#' @title Export high-quality brainview image with a colorbar.
#'
#' @description This function serves as an easy (but slightly inflexible) way to export a high-quality, tight-layout, colorbar figure to disk. If no colorbar is required, one can use \code{vislayout.from.coloredmeshes} instead.
#'
#' @inheritParams vislayout.from.coloredmeshes
#'
#' @param colorbar_legend character string or NULL, the title for the colorbar.
#'
#' @param img_only logical, whether to return only the resulting image
#'
#' @param horizontal logical, whether to plot the colorbar horizontally (TRUE) or vertically (FALSE). Pass 'NULL' to force no colorbar at all.
#'
#' @param silent logical, whether to suppress messages
#'
#' @param quality integer, an arbitrary quality. This is the resolution per tile before trimming, divided by 1000, in pixels. Example: 1L means 1000x1000 pixels per tile before trimming. Currently supported values: \code{1L..2L}. Note that the resolution you can get is also limited by your screen resolution.
#'
#' @param image.plot_extra_options named list, custom options for fields::image.plot. Overwrites those derived from the quality setting. If in doubt, leave this alone.
#'
#' @param large_legend logical, whether to plot extra large legend text, affects the font size of the colorbar_legend and the tick labels.
#'
#' @param style the rendering style, see \code{material3d} or use a predefined style like 'default' or 'shiny'.
#'
#' @param grid_like logical, passed to \code{vislayout.from.coloredmeshes}.
#'
#' @return magick image instance or named list, depending on the value of 'img_only'. If the latter, the list contains the fields 'rev_vl', 'rev_cb', and 'rev_ex', which are the return values of the functions \code{vislayout.from.coloredmeshes}, \code{coloredmesh.plot.colorbar.separate}, and {combine.colorbar.with.brainview.image}, respectively.
#'
#' @note This function also exports the resulting image to disk in PNG format, in the current working directory, named 'fsbrain_merged.png'. Note that your screen resolution has to be high enough to generate the final image.
#'
#' @examples
#' \dontrun{
#'     rand_data = rnorm(327684, 5, 1.5);
#'     cm = vis.data.on.fsaverage(morph_data_both=rand_data, rglactions=list('no_vis'=T));
#'     vis.export.from.coloredmeshes(cm, colorbar_legend='Random data');
#' }
#'
#' @export
vis.export.from.coloredmeshes <- function(coloredmeshes, colorbar_legend=NULL, img_only=TRUE, horizontal=TRUE, silent = TRUE, quality=1L, output_img="fsbrain_arranged.png", image.plot_extra_options=NULL, large_legend=TRUE, view_angles = get.view.angle.names(angle_set = "t4"), style = 'default', grid_like = TRUE, background_color = "white", transparency_color=NULL) {

    if (requireNamespace("magick", quietly = TRUE)) {
        quality = as.integer(quality);
        if(quality < 1L | quality > 2L) {
            stop("The parameter 'quality' must be an integer in range 1-2.");
        }

        if(is.null(image.plot_extra_options)) {
            if(quality == 1L) {
                if(large_legend) {
                    font_s = 4;
                    image.plot_extra_options = list(horizontal = horizontal, legend.cex = font_s, legend.width = 4, legend.mar = 18, legend.line=-6, legend.lab=colorbar_legend, axis.args = list(cex.axis = font_s, mgp=c(3,(max(1.0, font_s -1)),0)));
                } else {
                    font_s = 1.8;
                    image.plot_extra_options = list(horizontal = horizontal, legend.cex = font_s, legend.width = 2, legend.mar = 12, legend.line=-4, legend.lab=colorbar_legend, axis.args = list(cex.axis = font_s, mgp=c(3,(max(1.0, font_s -1)),0)));
                }
            } else { # quality 2
                if(large_legend) {
                    font_s = 4;
                    image.plot_extra_options = list(horizontal = horizontal, legend.cex = font_s, legend.width = 4, legend.mar = 18, legend.line=-6, legend.lab=colorbar_legend, axis.args = list(cex.axis = font_s, mgp=c(3,(max(1.0, font_s -1)),0)));
                } else {
                    font_s = 2.6;
                    image.plot_extra_options = list(horizontal = horizontal, legend.cex = font_s, legend.width = 4, legend.mar = 18, legend.line=-6, legend.lab=colorbar_legend, axis.args = list(cex.axis = font_s, mgp=c(3,(max(1.0, font_s -1)),0)));
                }
            }
        }
        rgloptions = list('windowRect'=c(50, 50, 1000 * quality, 1000 * quality));

        if(can.plot.colorbar.from.coloredmeshes(coloredmeshes) && !(is.null(horizontal))) {
            tmp_img = tempfile(fileext = ".png");
            res_vl = vislayout.from.coloredmeshes(coloredmeshes, rgloptions = rgloptions, view_angles = view_angles, silent = silent, output_img = tmp_img, style = style, grid_like = grid_like, background_color = background_color);
            png_options=list('filename'='fsbrain_cbar.png', 'width'=1400, 'height'=1400, 'bg'=background_color);
            res_cb = coloredmesh.plot.colorbar.separate(coloredmeshes, image.plot_extra_options=image.plot_extra_options, silent = silent, png_options=png_options);
            res_ex = combine.colorbar.with.brainview.image(horizontal = horizontal, silent = silent, brainview_img = tmp_img, output_img = output_img, background_color = background_color, transparency_color = transparency_color);
            if(img_only) {
                return(res_ex$merged_img);
            }
        } else {
            res_vl = vislayout.from.coloredmeshes(coloredmeshes, rgloptions = rgloptions, view_angles = view_angles, silent = silent, output_img = output_img, style = style, grid_like = grid_like, background_color = background_color, transparency_color = transparency_color);
            res_cb = NULL;
            rex_ex = NULL;
            if(img_only) {
                return(res_vl$merged_img);
            }
        }
        return(invisible(list('res_vl'=res_vl, 'res_cb'=res_cb, 'res_ex'=res_ex)));
    } else {
        warning("The 'magick' package must be installed to use this functionality. Image NOT written.");
        return(invisible(NULL));
    }
}

