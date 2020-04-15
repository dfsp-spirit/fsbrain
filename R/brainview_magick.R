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
#' @param grid_like logical, whether to arrange the images in a grid-like fashion. If FALSE, they will all be merged horizontally.
#'
#' @param border_geometry string, a geometry string passed to \code{\link[magick]{image_border}} to define the borders to add to each image tile. The default value adds 5 pixels, both horizontally and vertically.
#'
#' @param background_color string, a valid ImageMagick color string such as "white" or "#000080". The color to use when extending images (e.g., when creating the border).
#'
#' @export
arrange.brainview.images <- function(brainview_images, output_img, colorbar_img=NULL, silent=FALSE, grid_like=TRUE, border_geometry="5x5", background_color = "white") {

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
            message(sprintf("Merged image written to '%s' (current working dir is '%s').\n", output_img, getwd()));
        }

    } else {
        warning("The 'magick' package must be installed to use this functionality. Merged image NOT written.");
    }

}


#' @title Visualize coloredmeshes from several angles and combine the images into a new figure.
#'
#' @description Create a tight layout view of coloredmeshes from several angles. Creates separate `sd_<angle>` images, then crops and finally merges them into a single output image with image magick. The `coloredmeshes` to pass to this function are usually obtained by running any `vis*` function (like \code{\link[fsbrain]{vis.subject.morph.native}}, \code{\link[fsbrain]{vis.subject.morph.standard}}, \code{\link[fsbrain]{vis.subject.label}}, \code{\link[fsbrain]{vis.subject.annot}}, and others). That means you can use this function to visualize all kinds of data, e.g., morphometry data in native and standard space, labels, and brain atlases.
#
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the `coloredmesh.from*` functions (like \code{\link[fsbrain]{coloredmesh.from.morph.native}}). It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh. The `vis*` functions (like \code{\link[fsbrain]{vis.subject.morph.native}}) all return a list of coloredmeshes.
#'
#' @param view_angles list of strings. See \code{\link[fsbrain]{get.view.angle.names}} for all valid strings.
#'
#' @param rgloptions option list passed to \code{\link[rgl]{par3d}}. Example: \code{rgloptions = list("windowRect"=c(50,50,1000,1000))}.
#'
#' @param rglactions named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @param style character string, a rendering style, e.g., 'default', 'shiny' or 'semitransparent'. Alternatively, a named list of style parameters (see \code{\link[rgl]{material3d}}), e.g., \code{list("shininess"=50, specular="black", alpha=0.5)}. Use the magic word 'from_mesh' to use the 'style' field of each coloredmesh instead of a single, global style. In that case, you will have to make sure your meshes have such a field, if not, the style 'default' is used as a fallback for those which don't.
#'
#' @param output_img string, path to the output file. Defaults to "fsbrain_arranged.png"
#'
#' @param silent logical, whether to suppress all messages
#'
#' @param grid_like logical, whether to arrange the images in a grid-like fashion. If FALSE, they will all be merged horizontally. Passed to \code{\link[fsbrain]{arrange.brainview.images}}.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
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
vislayout.from.coloredmeshes <- function(coloredmeshes, view_angles=get.view.angle.names(angle_set = "t4"), rgloptions=list(), rglactions=list(), style="default", output_img="fsbrain_arranged.png", silent=FALSE, grid_like=TRUE) {

    if (requireNamespace("magick", quietly = TRUE)) {
        view_images = tempfile(view_angles, fileext = ".png");   # generate one temporary file name for each image

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

            brainviews(c(view), coloredmeshes, rgloptions = rgloptions, rglactions = final_rglactions, style = style);
        }

        # Now merge them into one
        arrange.brainview.images(view_images, output_img, silent=silent, grid_like=grid_like);
    } else {
        warning("The 'magick' package must be installed to use this functionality. Image with manual layout NOT written.");
    }
}

