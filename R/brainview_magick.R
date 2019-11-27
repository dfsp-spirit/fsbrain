# Functions that use the 'magick' package (ImageMagick) to combine separate brainview images into one image.
# Currently 'magick' is an optional dependency only.


#' @title Combine several brainview images into a new figure.
#'
#' @param brainview_images vector of character strings, paths to the brainview images, usually in PNG format
#'
#' @param output_img path to output image that including the file extension
#'
#' @param colorbar_img path to the main image containing the separate colorbar, usually an image in PNG format
#'
#' @param silent logical, whether to suppress messages
#'
#' @param arrange logical, whether to arrange the images ina grid-like fashion. If FALSE, they will all be merged horizontally.
#'
#' @export
arrange.brainview.images <- function(brainview_images, output_img, colorbar_img=NULL, silent=FALSE, arrange=TRUE) {

    if (requireNamespace("magick", quietly = TRUE)) {

        background_color = "white"; # Background color to use when extending images.

        # load image files
        images = magick::image_read(brainview_images);

        # trim images
        images = magick::image_trim(images);

        # Add tiny border back (to prevent them from touching each other)
        images = magick::image_border(images, background_color, "5x5");
        num_img = length(images);

        if(arrange) {
            if(num_img == 3 || num_img == 4) {
                top_row = magick::image_append(images[1:2]);
                bottom_row = magick::image_append(images[3:num_img]);
                merged_img = magick::image_append(c(top_row, bottom_row), stack = TRUE);
            } else if(num_img == 8 || num_img == 9) {
                top_row = magick::image_append(images[1:3]);
                mid_row = magick::image_append(images[4:6]);
                top_and_mid = magick::image_append(c(top_row, mid_row), stack = TRUE);

                if(num_img == 9) {
                    bottom_row = magick::image_append(images[7:num_img]);
                } else {
                    # For 8 images, it gets a bit trickier. It looks very bad if the two
                    # images are on the left and the right is empty, so we add an empty
                    # image of appropriate size between them.
                    width_top_and_mid = magick::image_info(top_and_mid)$width;
                    bottom_row_so_far = magick::image_append(images[7:num_img]);
                    width_bottom_so_far = magick::image_info(images[7])$width + magick::image_info(images[8])$width;
                    width_missing = width_top_and_mid - width_bottom_so_far;
                    if(width_missing > 0L) {
                        mid = magick::image_blank(width_missing, magick::image_info(bottom_row_so_far)$height, background_color);
                        bottom_row = magick::image_append(c(images[7], mid, images[8]));
                    } else {
                        bottom_row = bottom_row_so_far;
                    }
                }

                merged_img = magick::image_append(c(top_and_mid, bottom_row), stack = TRUE);
            } else {
                merged_img = magick::image_append(images);
            }
        } else {
            merged_img = magick::image_append(images);
        }

        magick::image_write(merged_img, path = output_img);
        if(! silent) {
            message(sprintf("Merged image written to '%s'.\n", output_img));
        }

    } else {
        warning("The 'magick' package must be installed to use this functionality. Merged image NOT written.");
    }

}


