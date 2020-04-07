# Functions that use the 'magick' package (ImageMagick) to combine a separate colorbar image and a brain visualization into a single image.
# Currently 'magick' is an optional dependency only.


#' @title Combine a colorbar and a brainview image into a new figure.
#'
#' @param brainview_img path to the main image containing the view of the brain, usually an image in PNG format
#'
#' @param colorbar_img path to the main image containing the separate colorbar, usually an image in PNG format
#'
#' @param output_img path to output image that including the file extension
#'
#' @param offset offset string passed to \code{\link[magick]{image_composite}}. Allows you to shift the location of the colorbar in the final image.
#'
#' @param extend_brainview_img_height_by integer value in pixels, the size of the lower border to add to the brainview_img. Use this if the lower part of the colorbar is off the image canvas.
#'
#' @param silent logical, whether to silence all messages
#'
#' @param allow_colorbar_shrink logical, whether to shrink the colorbar to the width of the animation in case it is considerably wider (more than 20 percent). Defaults to TRUE.
#'
#' @family colorbar functions
#' @export
combine.colorbar.with.brainview.image <- function(brainview_img, colorbar_img, output_img, offset="+0+0", extend_brainview_img_height_by=0L, silent=FALSE, allow_colorbar_shrink=TRUE) {
    if (requireNamespace("magick", quietly = TRUE)) {

        background_color = "white"; # Background color to use when extending images.

        main_img = magick::image_read(brainview_img);
        cbar_img = magick::image_read(colorbar_img);

        height_main = magick::image_info(main_img)$height;
        width_main = magick::image_info(main_img)$width;
        width_cbar = magick::image_info(cbar_img)$width;

        # Add a white border below the main image if requested. This leaves more space for the colorbar.
        if(extend_brainview_img_height_by != 0L) {
            extend_dims = sprintf("%dx%d", width_main, (height_main + extend_brainview_img_height_by));
            main_img = magick::image_extent(main_img, extend_dims, gravity="north", color="white");
        }

        # Trim the colorbar (remove all whitespace around it):
        cbar_img_trimmed = magick::image_trim(cbar_img);
        width_cbar_trimmed = magick::image_info(cbar_img_trimmed)$width;

        if(width_cbar_trimmed > 1.2 * width_main) {
            if(allow_colorbar_shrink) {
                cbar_img_trimmed = magick::image_resize(cbar_img_trimmed, sprintf("%dx", width_main));
                if(! silent) {
                    message(sprintf("Colorbar resized to width %d px.\n", width_main));
                }
            } else {
                if(!silent) {
                    message(sprintf("The colorbar (width %d px) is considerably wider than the main image (%d px). The colorbar may not fit onto the canvas and will be cut off. Please ensure roughly equal size.\n", width_cbar_trimmed, width_main));
                }
            }
        }



        # So far, the colorbar has been trimmed, and this looks bad in the final image because its lowest pixel is directly at the image border.
        # This looks like something has been (almost) cut off. To counteract this, we add a very thin lower white border back to the colorbar.
        extend_cbar_height_by = 5L; # in px
        if(extend_cbar_height_by != 0L) {
            width_cbar_trimmed = magick::image_info(cbar_img_trimmed)$width;
            height_cbar_trimmed = magick::image_info(cbar_img_trimmed)$height;
            extend_cbar_dims = sprintf("%dx%d", width_cbar_trimmed, (height_cbar_trimmed + extend_cbar_height_by));
            cbar_img_trimmed = magick::image_extent(cbar_img_trimmed, extend_cbar_dims, gravity="north", color=background_color);
        }

        # Overlay the colorbar over the bottom part of the main image.
        combined_img = magick::image_composite(main_img, cbar_img_trimmed, gravity="south", offset=offset);
        magick::image_write(combined_img, path = output_img);
        if(! silent) {
            message(sprintf("Combined image written to '%s'.\n", output_img));
        }

    } else {
        warning("The 'magick' package must be installed to use this functionality. Combined image NOT written.");
    }
}


#' @title Combine a colorbar and a brain animation in gif format into a new animation.
#'
#' @param brain_animation path to the brain animation in gif format
#'
#' @param colorbar_img path to the main image containing the separate colorbar, usually an image in PNG format
#'
#' @param output_animation path to output image in gif format, must include the '.gif' file extension
#'
#' @param offset offset string passed to \code{\link[magick]{image_composite}}. Allows you to shift the location of the colorbar in the final image.
#'
#' @param extend_brainview_img_height_by integer value in pixels, the size of the lower border to add to the brainview_img. Use this if the lower part of the colorbar is off the image canvas.
#'
#' @param silent logical, whether to silence all messages
#'
#' @param allow_colorbar_shrink logical, whether to shrink the colorbar to the width of the animation in case it is considerably wider (more than 20 percent). Defaults to TRUE.
#'
#' @family colorbar functions
#' @export
combine.colorbar.with.brainview.animation <- function(brain_animation, colorbar_img, output_animation, offset="+0+0", extend_brainview_img_height_by=0L, silent=FALSE, allow_colorbar_shrink=TRUE) {
    if (requireNamespace("magick", quietly = TRUE)) {
        # brain_animation = "~/fsbrain_mov_main.gif"; colorbar_img = "~/fsbrain_img_cbar.gif"; extend_brainview_img_height_by = 20; offset="+0+0"; extend_brainview_img_height_by = 20;

        background_color = "white"; # Background color to use, e.g., when extending images.

        main_mov = magick::image_read(brain_animation);
        cbar_img = magick::image_read(colorbar_img);

        height_mov = max(magick::image_info(main_mov)$height);
        width_mov = max(magick::image_info(main_mov)$width);
        width_cbar = magick::image_info(cbar_img)$width;


        # Add a lower border to each frame in the original gif if requested. This solves the problem that the brain may overlap with the colorbar.
        if(extend_brainview_img_height_by != 0L) {
            extend_dims = sprintf("%dx%d", width_mov, (height_mov + extend_brainview_img_height_by));
            main_mov = magick::image_extent(main_mov, extend_dims, gravity="north", color=background_color);
        }

        # Crop all unneeded whitespace around the colorbar (remove all white borders).
        cbar_img_trimmed = magick::image_trim(cbar_img);
        width_cbar_trimmed = magick::image_info(cbar_img_trimmed)$width;

        if(width_cbar_trimmed > 1.2 * width_mov) {
            if(allow_colorbar_shrink) {
                cbar_img_trimmed = magick::image_resize(cbar_img_trimmed, sprintf("%dx", width_mov));
                if(! silent) {
                    message(sprintf("Colorbar resized to width %d px.\n", width_mov));
                }
            } else {
                if(!silent) {
                    message(sprintf("The colorbar (width %d px) is considerably wider than the animation (%d px) even after trimming. The colorbar may not fit onto the canvas and will be cut off. Please ensure roughly equal size.\n", width_cbar_trimmed, width_mov));
                }
            }
        }

        # So far, the colorbar has been trimmed, and this looks bad in the final image because its lowest pixel is directly at the image border.
        # This looks like something has been (almost) cut off. To counteract this, we add a very thin lower white border back to the colorbar.
        extend_cbar_height_by = 5L; # in px
        if(extend_cbar_height_by != 0L) {
            width_cbar_trimmed = magick::image_info(cbar_img_trimmed)$width;
            height_cbar_trimmed = magick::image_info(cbar_img_trimmed)$height;
            extend_cbar_dims = sprintf("%dx%d", width_cbar_trimmed, (height_cbar_trimmed + extend_cbar_height_by));
            cbar_img_trimmed = magick::image_extent(cbar_img_trimmed, extend_cbar_dims, gravity="north", color=background_color);
        }

        # Create a composite image (the colorbar overlayed over the lower part of the image) for each frame in the animation.
        if(! silent) {
            message("Adding colorbar to all frames of the animation...");
        }
        frames = magick::image_composite(main_mov, cbar_img_trimmed, gravity="south", offset=offset);

        # Create a new animation from the frames.
        if(! silent) {
            message("Creating new animation from the frames. This can take a while...");
        }
        animation = magick::image_animate(frames, fps = 20, loop = 0);

        # Save the new animation to a gif image.
        magick::image_write(animation, path = output_animation);
        if(! silent) {
            message(sprintf("Combined animation with colorbar written to '%s'.\n", output_animation));
        }

    } else {
        warning("The 'magick' package must be installed to use this functionality. Combined animation NOT written.");
    }
}
