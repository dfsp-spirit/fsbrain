#' @title Visualize coloredmeshes as an interactive rgl WebGL widget for use in R Shiny apps and RMarkdown documents.
#'
#' @description Create an interactive 3D brain view that can be embedded in R Shiny applications or RMarkdown/HTML documents. Unlike the standard \code{vis.*} functions which open an X11/OpenGL window and are designed for static screenshot export, this function renders to a headless rgl device and returns an \code{\link[rgl]{rglwidget}} object that provides interactive 3D rendering in a web browser. The user can rotate, zoom, and pan the brain in the widget.
#'
#' @param coloredmeshes, a hemilist of coloredmeshes (as returned by \code{\link[fsbrain]{vis.subject.morph.native}} and similar functions when called with \code{rglactions=list('no_vis'=TRUE)}) or a flat list of \code{\link[fsbrain]{coloredmesh}} instances.
#'
#' @param background string, background color passed to \code{\link[rgl]{bg3d}}. Defaults to "white".
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'render' set to FALSE. Defaults to TRUE.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param rgloptions, named list. Parameters passed to \code{\link[rgl]{par3d}}. Defaults to the value returned by \code{\link[fsbrain]{rglo}}.
#'
#' @param ... extra arguments passed to \code{\link[rgl]{rglwidget}}.
#'
#' @return an htmlwidget object from the rgl package, suitable for use with \code{\link[rgl]{rglwidgetOutput}} / \code{\link[rgl]{renderRglwidget}} in Shiny, or for direct embedding in RMarkdown HTML output.
#'
#' @examples
#' \dontrun{
#'    fsbrain::download_optional_data();
#'    fsbrain::download_fsaverage(accept_freesurfer_license=TRUE);
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'
#'    # Get coloredmeshes without opening a window:
#'    cm = vis.subject.annot(subjects_dir, 'subject1', 'aparc', 'both',
#'          rglactions=list('no_vis'=TRUE));
#'
#'    # Create an interactive WebGL widget:
#'    widget = vis.rglwidget(cm);
#'
#'    # In RMarkdown, just print it:
#'    # widget
#'
#'    # In Shiny, use rglwidgetOutput / renderRglwidget (see the shiny demo app).
#' }
#'
#' @family visualization functions
#'
#' @importFrom rgl open3d bg3d par3d close3d rglwidget
#'
#' @export
vis.rglwidget <- function(coloredmeshes, background="white", skip_all_na=TRUE,
                           style="default", rgloptions=rglo(), ...) {

    if(!is.list(coloredmeshes)) {
        stop("Parameter 'coloredmeshes' must be a list of coloredmesh instances.");
    }

    if(length(coloredmeshes) < 1) {
        warning("Nothing to visualize: empty coloredmeshes list.");
        return(invisible(NULL));
    }

    # Flatten hemilist (list(lh=cm_lh, rh=cm_rh)) into a list of coloredmeshes
    # that vis.renderable() can handle.
    if("lh" %in% names(coloredmeshes) || "rh" %in% names(coloredmeshes)) {
        flat_list = list();
        if("lh" %in% names(coloredmeshes) && !is.null(coloredmeshes$lh)) {
            flat_list = c(flat_list, list(coloredmeshes$lh));
        }
        if("rh" %in% names(coloredmeshes) && !is.null(coloredmeshes$rh)) {
            flat_list = c(flat_list, list(coloredmeshes$rh));
        }
        coloredmeshes = flat_list;
    }

    # Open a headless rgl device: no X11 window, renders off-screen.
    rgl::open3d(useNULL = TRUE);
    on.exit(rgl::close3d(), add = TRUE);

    do.call(rgl::par3d, rgloptions);
    rgl::bg3d(background);

    for(cmesh in coloredmeshes) {
        if(fsbrain.renderable(cmesh)) {
            vis.renderable(cmesh, skip_all_na = skip_all_na, style = style);
        }
    }

    return(rgl::rglwidget(...));
}
