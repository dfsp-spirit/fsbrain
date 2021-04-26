# Helper functions related to image magick.


#' @title Wrapper around magick::image_append that allows specifying the background color when working with images of different width/height.
#'
#' @param images a vector/stack of magick images. See \code{magick::image_blank} or other methods to get one.
#'
#' @param stack whether to append vertically, default is FALSE / horizontally.
#'
#' @param background_color hex color string, the background color to use if the images have different sizes and one needs to be extended. Do not use color names like 'gray', which differ between R and magick.
#'
#' @return a single magick image, the stacked version.
#'
#' @keywords internal
wrapped.image.append <- function(images, stack = FALSE, background_color = "#FFFFFF") {

    if(is.null(images)) { stop("Parameter 'images' must not be NULL."); }
    if(length(images) == 0) { stop("Parameter 'images' must not be empty."); }

    if(! is.logical(stack)) {
        stop("Parameter 'stack' must be logical.");
    }

    if(stack) {
        # The width of the images must match.
        images = images.same.width(images, background_color = background_color);
    } else {
        # The height must match.
        images = images.same.height(images, background_color = background_color);
    }

    img_appended = magick::image_append(images, stack = stack);
    return(img_appended);
}


#' @title Extent all images to the width of the image with maximal width.
#'
#' @inheritParams wrapped.image.append
#'
#' @return a vector/stack of magick images, all with the same width.
#'
#' @keywords internal
images.same.width <- function(images, background_color = "white") {
    max_width = images.dimmax(images)$width;
    extended_images = NULL;
    for (img_idx in seq(length(images))) {
        img = images[img_idx];
        img_width = magick::image_info(img)$width;

        if(img_width < max_width) {
            img_height = magick::image_info(img)$height;
            img_target_width = img_width + (max_width - img_width)
            extent_dims = sprintf("%dx%d", img_target_width, img_height);
            img = magick::image_extent(img, extent_dims, gravity = "east", color = background_color);
        }
        if(is.null(extended_images)) {
            extended_images = img;
        } else {
            extended_images = c(extended_images, img);
        }

    }
    return(extended_images);
}


#' @title Extent all images to the height of the image with maximal height.
#'
#' @inheritParams wrapped.image.append
#'
#' @return a vector/stack of magick images, all with the same height.
#'
#' @keywords internal
images.same.height <- function(images, background_color = "white") {
    max_height = images.dimmax(images)$height;
    extended_images = c();
    for (img_idx in seq(length(images))) {
        img = images[img_idx];
        img_height = magick::image_info(img)$height;

        if(img_height < max_height) {
            img_width = magick::image_info(img)$width;
            img_target_height = img_height + (max_height - img_height)
            extent_dims = sprintf("%dx%d", img_width, img_target_height);
            img = magick::image_extent(img, extent_dims, gravity = "south", color = background_color);
        }
        if(is.null(extended_images)) {
            extended_images = img;
        } else {
            extended_images = c(extended_images, img);
        }
    }
    return(extended_images);
}


#' @title Compute max width and height of magick images.
#'
#' @inheritParams wrapped.image.append
#'
#' @return named list with entries 'width' and 'height'
#'
#' @keywords internals
images.dimmax <- function(images) {
    if(is.null(images)) { stop("Parameter 'images' must not be NULL."); }
    if(length(images) == 0) { stop("Parameter 'images' must not be empty."); }
    max_width = 0L;
    max_height = 0L;
    for (img_idx in seq(length(images))) {
        img = images[img_idx];
        img_info = magick::image_info(img);
        if(img_info$width > max_width) {
            max_width = img_info$width;
        }
        if(img_info$height > max_height) {
            max_height = img_info$height;
        }
    }
    return(list("width" = max_width, "height" = max_height));
}

