
#' @title Show one or more views of the given meshes in rgl windows.
#'
#' @param views list of strings. Valid entries include: 'si': single interactive view. 'sd_<angle>': single view from angle <angle>. The <angle> part must be one of the strings returned by \code{\link[fsbrain]{get.view.angle.names}}. Example: 'sd_caudal'. 'sr': single rotating view. 't4': tiled view showing the brain from 4 angles. 't9': tiled view showing the brain from 9 angles.
#'
#' @param coloredmeshes list of coloredmesh or renderable. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param rgloptions option list passed to \code{\link{par3d}}. Example: \code{rgloptions = list("windowRect"=c(50,50,1000,1000))}
#'
#' @param rglactions named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @param style character string, a rendering style, e.g., 'default', 'shiny' or 'semitransparent'.
#'
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE. See \code{\link[fsbrain]{coloredmesh.plot.colorbar.separate}} for an alternative.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @seealso \code{\link[fsbrain]{get.view.angle.names}}
#'
#' @export
brainviews <- function(views, coloredmeshes, rgloptions = rglo(), rglactions = list(), style="default", draw_colorbar = FALSE) {

    # Wrap a single instance into a list if needed
    if(fsbrain.renderable(coloredmeshes)) {
        message("Wrapping single renderable instance into list.");
        coloredmeshes = list(coloredmeshes);
    }

    if(length(views)) {
        for(view in views) {
            if(view == "t4") {
                invisible(brainview.t4(coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar, style = style));
            } else if(view == "t9") {
                invisible(brainview.t9(coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar, style = style));
            } else if(view == "si") {
                invisible(brainview.si(coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar, style = style));
            } else if(view == "sr") {
                invisible(brainview.sr(coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar, style = style));
            } else if(startsWith(view, "sd_")) {
                angle = substr(view, 4, nchar(view));
                invisible(brainview.sd(coloredmeshes, angle, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar, style = style));
            } else {
                stop(sprintf("Invalid view '%s'. Valid ones include 'si', 'sr', 'sd_<angle>', 't4' and 't9'.\n", view));
            }
        }
    }
    return(invisible(coloredmeshes));
}


#' @title Get list of valid view angle names.
#'
#' @description The returned strings are used as constants to identify a view of type `sd_<angle>`. They can be used to construct entries for the parameter `views` of functions like \code{\link[fsbrain]{vis.subject.morph.native}}, or directly as parameter 'view_angles' for functions like \code{\link[fsbrain]{vislayout.from.coloredmeshes}}.
#'
#' @param angle_set string, which view subset to return. Available subsets are: 'all' (or alias 't9'): for all 9 angles. 't4': for the t4 views. 'medial': the 2 medial views, one for each hemi. 'lateral': the 2 lateral views, one for each hemi. 'lh': medial and laterial for the left hemisphere. 'rh': medial and laterial for the right hemisphere.
#'
#' @param add_sd_prefix logical, whether the prefix 'sd_' should be added to the string. This will construct full view names. If set to false, only the substring after the prefix 'sd_' will be returned. This is used internally only and should not be needed in general.
#'
#' @return vector of character strings, all valid view angle strings.
#' @export
get.view.angle.names <- function(angle_set="all", add_sd_prefix=TRUE) {
    if(angle_set == "all" | angle_set == "t9" | angle_set == "t8") {
        angles = c('lateral_lh', 'dorsal', 'lateral_rh', 'medial_lh', 'ventral', 'medial_rh', 'rostral', 'caudal');
    } else if(angle_set == "t4") {
        angles = c('lateral_lh', 'lateral_rh', 'medial_lh', 'medial_rh');
    } else if(angle_set == "medial" | angle_set == "t2") {
        angles = c('medial_lh', 'medial_rh');
    } else if(angle_set == "lateral") {
        angles = c('lateral_lh', 'lateral_rh');
    } else if(angle_set == "lh") {
        angles = c('lateral_lh', 'medial_lh');
    }  else if(angle_set == "rh") {
        angles = c('lateral_rh', 'medial_rh');
    } else {
        stop(sprintf("Invalid 'angle_set' parameter: '%s'.\n", angle_set));
    }

    if(add_sd_prefix) {
        angles = paste("sd_", angles, sep="");
    }
    return(angles);
}


#' @title Visualize a list of colored meshes from a single viewpoint, interactively.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to \code{\link{bg3d}}.
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'render' set to FALSE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param draw_labels logical, whether to draw label text for the different views that show information on the view direction and hemisphere displayed in a subplot. Defaults to FALSE.
#'
#' @param rgloptions, named list. Parameters passed to \code{\link{par3d}}. Defaults to the empty list.
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. Defaults to the empty list.
#'
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d wire3d shade3d mfrow3d next3d text3d rgl.viewpoint
brainview.si <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", draw_labels = FALSE, rgloptions = rglo(), rglactions=list(), draw_colorbar = FALSE) {

    if(!is.list(coloredmeshes)) {
        stop("Parameter 'coloredmeshes' must be a list.");
    }

    coloredmeshes = shift.hemis.rglactions(coloredmeshes, rglactions);

    return(invisible(vis.coloredmeshes(coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar, style = style)));
}


#' @title Visualize a list of colored meshes, rotating the camera around them.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to \code{\link{bg3d}}.
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'render' set to FALSE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param draw_labels logical, whether to draw label text for the different views that show information on the view direction and hemisphere displayed in a subplot. Defaults to FALSE.
#'
#' @param x rotation x axis value, passed to \code{\link{spin3d}}. Defaults to 0.
#'
#' @param y rotation y axis value, passed to \code{\link{spin3d}}. Defaults to 1.
#'
#' @param z rotation z axis value, passed to \code{\link{spin3d}}. Defaults to 0.
#'
#' @param rpm rotation rpm value, passed to \code{\link{spin3d}}. Defaults to 15.
#'
#' @param duration rotation duration value, passed to \code{\link{spin3d}}. Defaults to 20.
#'
#' @param rgloptions, named list. Parameters passed to \code{\link{par3d}}. Defaults to the empty list.
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. Defaults to the empty list.
#'
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d wire3d shade3d mfrow3d next3d text3d rgl.viewpoint
brainview.sr <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", draw_labels = FALSE, x=0, y=1, z=0, rpm=6, duration=10, rgloptions = rglo(), rglactions=list(), draw_colorbar = FALSE) {

    if(!is.list(coloredmeshes)) {
        stop("Parameter 'coloredmeshes' must be a list.");
    }

    coloredmeshes = shift.hemis.rglactions(coloredmeshes, rglactions);

    return(invisible(vis.coloredmeshes.rotating(coloredmeshes, x=x, y=y, z=z, rpm=rpm, duration=duration, rgloptions = rgloptions, rglactions = rglactions, style = style)));
}


#' @keywords internal
get.sorted.cmeshes <- function(coloredmeshes) {
    if("lh" %in% names(coloredmeshes) | "rh" %in% names(coloredmeshes)) {
        # Use the new style of coloredmeshes: a list with entries 'lh' and 'rh', each of which contains a single cmesh
        if("lh" %in% names(coloredmeshes)) {
            lh_meshes = list(coloredmeshes$lh);
        } else {
            lh_meshes = NULL;
        }

        if("rh" %in% names(coloredmeshes)) {
            rh_meshes = list(coloredmeshes$rh);
        } else {
            rh_meshes = NULL;
        }
    } else {
        # This is the old style of passing the list: unsorted with unmerged colormaps. We need to fiddle with the data.
        # Some functions still use this, but they will all be reworked in the future. Once done, this part can be removed.
        warning("The old style of passing coloredmeshes should not be in use anymore.");
        hemi_sorted_cmeshes = sort.coloredmeshes.by.hemi(coloredmeshes);
        lh_meshes = hemi_sorted_cmeshes$lh;
        rh_meshes = hemi_sorted_cmeshes$rh;
    }
    return(list("lh"=lh_meshes, "rh"=rh_meshes));
}


#' @title Visualize a list of colored meshes from four angles.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to \code{\link{bg3d}}.
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'render' set to FALSE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param draw_labels logical, whether to draw label text for the different views that show information on the view direction and hemisphere displayed in a subplot. Defaults to FALSE.
#'
#' @param rgloptions, named list. Parameters passed to \code{\link{par3d}}. Defaults to the empty list.
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d wire3d shade3d mfrow3d next3d text3d rgl.viewpoint
brainview.t4 <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", draw_labels = FALSE, rgloptions = rglo(), rglactions = list(), draw_colorbar = FALSE) {

    label_shift_y = -20;

    if(!is.list(coloredmeshes)) {
        stop("Parameter 'coloredmeshes' must be a list.");
    }

    horizontal = FALSE;
    if(draw_colorbar == TRUE) {
        draw_colorbar = "vertical";
    }

    if(draw_colorbar == "vertical") {
        layout_mat = matrix(c(1, 2, 5, 3, 4, 6), ncol=3, byrow = T);
        layout_column_widths = c(3L, 3L, 1L);
        layout_row_heights = rep(1L, nrow(layout_mat));
    } else if(draw_colorbar == "horizontal") {
        horizontal = TRUE;
        layout_mat = matrix(seq.int(6), ncol=2, byrow = T);
        layout_row_heights = c(3L, 3L, 1L);
        layout_column_widths = rep(1L, ncol(layout_mat));
    } else if(draw_colorbar == FALSE) {
        # assume FALSE
        layout_mat = matrix(seq.int(4), ncol=2, byrow = T);
        layout_column_widths = rep(1L, ncol(layout_mat));
        layout_row_heights = rep(1L, nrow(layout_mat));
    } else {
        stop("Invalid setting for 'draw_colorbar'. Use a logical value or one of 'horizontal' or 'vertical'.");
    }


    sorted_meshes = get.sorted.cmeshes(coloredmeshes);
    lh_meshes = sorted_meshes$lh;
    rh_meshes = sorted_meshes$rh;

    rgl::open3d();
    do.call(rgl::par3d, rgloptions);
    Sys.sleep(1);
    rgl::bg3d(background);
    rgl::layout3d(layout_mat, widths=layout_column_widths, height=layout_row_heights);

    # Create the upper left view: draw only the left hemi, from the left
    rgl::next3d(reuse=TRUE);
    vis.rotated.coloredmeshes(lh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"lateral lh");
    }

    # Create the upper right view
    rgl::next3d(reuse=FALSE);
    vis.rotated.coloredmeshes(rh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"lateral rh");
    }


    # Create the lower left view
    rgl::next3d(reuse=FALSE);
    vis.rotated.coloredmeshes(lh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"medial lh");
    }


    # Create the lower right view
    rgl::next3d(reuse=FALSE);
    vis.rotated.coloredmeshes(rh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"medial rh");
    }

    if(is.character(draw_colorbar)) {
        rgl::next3d(reuse=FALSE);
        draw.colorbar(coloredmeshes, horizontal=horizontal);
        rgl::next3d(reuse=FALSE);
        draw.colorbar(coloredmeshes, horizontal=horizontal);
    }


    perform.rglactions(rglactions);
    return(invisible(coloredmeshes));
}


#' @title Visualize a list of colored meshes from nine angles.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to \code{\link{bg3d}}.
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'render' set to FALSE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param draw_labels logical, whether to draw label text for the different views that show information on the view direction and hemisphere displayed in a subplot. Defaults to FALSE.
#'
#' @param rgloptions, named list. Parameters passed to \code{\link{par3d}}. Defaults to the empty list. To increase plot resolution to 2000x1600 px, try: \code{rgloptions=list("windowRect"=c(50,50,2000,1600))}.
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d wire3d shade3d mfrow3d next3d text3d rgl.viewpoint
brainview.t9 <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", draw_labels = FALSE, rgloptions = rglo(), rglactions = list(), draw_colorbar = FALSE) {


    label_shift_y = -20;
    label_shift_y_dorsal = -120;
    label_shift_y_ventral = -80;

    if(!is.list(coloredmeshes)) {
        stop("Parameter 'coloredmeshes' must be a list.");
    }

    layout_dim_x = 3;
    layout_dim_y = 3;
    num_views = layout_dim_x * layout_dim_y;

    sorted_meshes = get.sorted.cmeshes(coloredmeshes);
    lh_meshes = sorted_meshes$lh;
    rh_meshes = sorted_meshes$rh;

    horizontal = FALSE;
    if(draw_colorbar == TRUE) {
        draw_colorbar = "vertical";
    }

    if(draw_colorbar == "vertical") {
        layout_mat = matrix(c(1, 2, 3, 10, 4, 5, 6, 11, 7, 8, 9, 12), ncol=4, byrow = T);
        layout_column_widths = c(3L, 3L, 3L, 1L);
        layout_row_heights = rep(1L, nrow(layout_mat));
    } else if(draw_colorbar == "horizontal") {
        horizontal = TRUE;
        layout_mat = matrix(seq.int(12), ncol=3, byrow = T);
        layout_row_heights = c(3L, 3L, 3L, 1L);
        layout_column_widths = rep(1L, ncol(layout_mat));
    } else if(draw_colorbar == FALSE) {
        # assume FALSE
        layout_mat = matrix(seq.int(9), ncol=3, byrow = T);
        layout_column_widths = rep(1L, ncol(layout_mat));
        layout_row_heights = rep(1L, nrow(layout_mat));
    } else {
        stop("Invalid setting for 'draw_colorbar'. Use a logical value or one of 'horizontal' or 'vertical'.");
    }

    rgl::open3d();
    do.call(rgl::par3d, rgloptions);
    Sys.sleep(1);
    rgl::bg3d(background);
    rgl::layout3d(layout_mat, widths=layout_column_widths, height=layout_row_heights);

    coloredmeshes_potentially_shifted = shift.hemis.rglactions(coloredmeshes, rglactions);


    #  ------------------ Row 1 --------------------


    # Create the upper left view: draw only the left hemi, from the left
    rgl::next3d(reuse=TRUE);
    vis.rotated.coloredmeshes(lh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"lateral lh");
    }

    # Create the upper central view: draw both hemis from above (top view)
    rgl::next3d(reuse=FALSE);
    vis.rotated.coloredmeshes(coloredmeshes_potentially_shifted, 0, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(0, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y_dorsal,0,"dorsal");
    }

    # Create the upper right view
    rgl::next3d(reuse=FALSE);
    vis.rotated.coloredmeshes(rh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"lateral rh");
    }


    #  ------------------ Row 2 --------------------


    # Create the 2nd row left view
    rgl::next3d(reuse=FALSE);
    vis.rotated.coloredmeshes(lh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"medial lh");
    }

    # Create the 2nd row central view: draw both hemis from below (bottom view)
    rgl::next3d(reuse=FALSE);
    vis.rotated.coloredmeshes(coloredmeshes_potentially_shifted, pi, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(0, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y_ventral,0,"ventral");
    }


    # Create the 2nd row right view
    rgl::next3d(reuse=FALSE);
    vis.rotated.coloredmeshes(rh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"medial rh");
    }


    #  ------------------ Row 3 --------------------


    # Create the bottom left view: draw only the left hemi, from the left
    rgl::next3d(reuse=FALSE);
    vis.rotated.coloredmeshes(coloredmeshes_potentially_shifted, pi/2, 1, 0, 0, style=style);
    rgl.viewpoint(0, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"rostral");
    }

    # Create the bottom central view.
    rgl::next3d(reuse=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"(empty)");
    }

    # Create the bottom right view
    rgl::next3d(reuse=FALSE);
    vis.rotated.coloredmeshes(coloredmeshes_potentially_shifted, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(180, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"caudal");
    }

    if(is.character(draw_colorbar)) {
        rgl::next3d(reuse=FALSE);
        draw.colorbar(coloredmeshes, horizontal=horizontal);
        rgl::next3d(reuse=FALSE);
        draw.colorbar(coloredmeshes, horizontal=horizontal);
        rgl::next3d(reuse=FALSE);
        draw.colorbar(coloredmeshes, horizontal=horizontal);
    }


    perform.rglactions(rglactions);
    return(invisible(coloredmeshes));
}


#' @title Visualize a list of colored meshes from a single defined angle.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the `coloredmesh.from*` functions (like \code{\link[fsbrain]{coloredmesh.from.morph.native}}). It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh. Note that the `vis*` functions (like \code{\link[fsbrain]{vis.subject.morph.native}}) all return a list of coloredmeshes.
#'
#' @param view_angle character string, the view angle. One of 'lateral_lh', 'dorsal', 'lateral_rh', 'medial_lh', 'ventral', 'medial_rh', 'rostral' or 'caudal'. See \code{\link[fsbrain]{get.view.angle.names}}.
#'
#' @param background string, background color passed to \code{\link{bg3d}}.
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'render' set to FALSE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param rgloptions, named list. Parameters passed to \code{\link{par3d}}. Defaults to the empty list. To increase plot resolution to 2000x1600 px, try: \code{rgloptions=list("windowRect"=c(50,50,2000,1600))}.
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d rgl.viewpoint
brainview.sd <- function(coloredmeshes, view_angle, background="white", skip_all_na=TRUE, style="default", rgloptions = rglo(), rglactions = list(), draw_colorbar = FALSE) {


    if(!is.list(coloredmeshes)) {
        stop("Parameter 'coloredmeshes' must be a list.");
    }

    horizontal = FALSE;
    if(draw_colorbar == TRUE) {
        draw_colorbar = "vertical";
    }

    if(draw_colorbar == "vertical") {
        layout_mat = matrix(c(1, 2), ncol=2, byrow = T);
        layout_column_widths = c(3L, 1L);
        layout_row_heights = rep(1L, nrow(layout_mat));
    } else if(draw_colorbar == "horizontal") {
        horizontal = TRUE;
        layout_mat = matrix(c(1, 2), ncol=1, byrow = T);
        layout_row_heights = c(3L, 1L);
        layout_column_widths = rep(1L, ncol(layout_mat));
    } else if(draw_colorbar == FALSE) {
        # assume FALSE
        layout_mat = NULL;
        layout_column_widths = NULL;
        layout_row_heights = NULL;
    } else {
        stop("Invalid setting for 'draw_colorbar'. Use a logical value or one of 'horizontal' or 'vertical'.");
    }


    sorted_meshes = get.sorted.cmeshes(coloredmeshes);
    lh_meshes = sorted_meshes$lh;
    rh_meshes = sorted_meshes$rh;


    rgl::open3d();
    do.call(rgl::par3d, rgloptions);
    Sys.sleep(1);
    rgl::bg3d(background);

    if(is.character(draw_colorbar)) {
        rgl::layout3d(layout_mat, widths=layout_column_widths, height=layout_row_heights);
        rgl::next3d(reuse=TRUE);
    }


    if(view_angle == "lateral_lh") {
        vis.rotated.coloredmeshes(lh_meshes, pi/2, 1, 0, 0, style=style);
        rgl::rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
    } else if (view_angle == "dorsal") {
        vis.rotated.coloredmeshes(coloredmeshes, 0, 1, 0, 0, style=style);
        rgl::rgl.viewpoint(0, 0, fov=0, interactive=FALSE);
    } else if(view_angle == "lateral_rh") {
        vis.rotated.coloredmeshes(rh_meshes, pi/2, 1, 0, 0, style=style);
        rgl::rgl.viewpoint(90, 0, fov=0, interactive=FALSE);
    } else if(view_angle == "medial_lh") {
        vis.rotated.coloredmeshes(lh_meshes, pi/2, 1, 0, 0, style=style);
        rgl::rgl.viewpoint(90, 0, fov=0, interactive=FALSE);
    } else if(view_angle == "ventral") {
        vis.rotated.coloredmeshes(coloredmeshes, pi, 1, 0, 0, style=style);
        rgl::rgl.viewpoint(0, 0, fov=0, interactive=FALSE);
    } else if(view_angle == "medial_rh") {
        vis.rotated.coloredmeshes(rh_meshes, pi/2, 1, 0, 0, style=style);
        rgl::rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
    } else if(view_angle == "rostral") {
        vis.rotated.coloredmeshes(coloredmeshes, pi/2, 1, 0, 0, style=style);
        rgl.viewpoint(0, 0, fov=0, interactive=FALSE);
    } else if(view_angle == "caudal") {
        vis.rotated.coloredmeshes(coloredmeshes, pi/2, 1, 0, 0, style=style);
        rgl::rgl.viewpoint(180, 0, fov=0, interactive=FALSE);
    } else {
        stop(sprintf("Invalid view_angle '%s'. Must be one of 'lateral_lh', 'dorsal', 'lateral_rh', 'medial_lh', 'ventral', 'medial_rh', 'rostral' or 'caudal'.\n", view_angle));
    }

    if(is.character(draw_colorbar)) {
        rgl::next3d(reuse=FALSE);
        draw.colorbar(coloredmeshes, horizontal=horizontal);
    }

    perform.rglactions(rglactions);

    return(invisible(coloredmeshes));
}


#' @title Shift hemispheres apart.
#'
#' @description  Modify mesh coordinates of a hemilist of coloredmeshes to introduce a gap between the two hemispheres.
#'
#' @param coloredmeshes_hl hemilist of coloredmeshes
#'
#' @param shift_by numerical vector of length 2, the amount by which to shift the hemis. The first value is for the left hemi, the second for the right hemi (values can be negative). Pass `NULL` to determine the shift automatically from the mesh coordinates, and adapt 'hemi_order_on_axis' to define how that happens.
#'
#' @param axis positive integer, one of 1L, 2L or 3L. The axis on which to shift (x,y,z).
#'
#' @param hemi_order_on_axis character string, one of 'auto', 'auto_flipped', 'lr' or 'rl'. Defines how to determine the order of the hemis on the axes. This is ignored unless 'shift_by' is `NULL`. The 'auto' setting assumes that the hemisphere with the smaller minimal vertex coordinate (on the given axis) comes first. Note that if the overlap (or shift) is extreme, this may not hold anymore. Therefore, the default value is 'lr', which works for FreeSurfer data. The 'auto_flipped' setting will always return the inverse of 'auto', so if 'auto' did not work, 'auto_flipped' will.
#'
#' @param min_dist numerical scalar, the minimal distance of the hemispheres. Ignored unless 'shift_by' is `NULL`.
#'
#' @return hemilist of coloredmeshes, the shifted meshes
#'
#' @export
shift.hemis.apart <- function(coloredmeshes_hl, shift_by=NULL, axis=1L, hemi_order_on_axis='lr', min_dist=0) {
    axis = as.integer(axis);

    if(axis < 1L | axis > 3L) {
        stop("Parameter 'axis' must be 1, 2 or 3.");
    }

    if(! hemi_order_on_axis %in% c('auto', 'auto_flipped', 'lr', 'rl')) {
        stop("Parameter 'hemi_order_on_axis' must be one of 'auto', 'auto_flipped', 'lr', or 'rl'.");
    }

    if(is.null(coloredmeshes_hl$lh) | is.null(coloredmeshes_hl$rh)) {
        # Silently do nothing if there is only a single hemisphere to plot (avoids pointless warning below).
        return(coloredmeshes_hl);
    }

    if(hasIn(coloredmeshes_hl, c('lh', 'metadata', 'fs_mesh')) & hasIn(coloredmeshes_hl, c('rh', 'metadata', 'fs_mesh'))) {
        lh_src_mesh = coloredmeshes_hl$lh$metadata$fs_mesh;
        rh_src_mesh = coloredmeshes_hl$rh$metadata$fs_mesh;

        if(is.null(shift_by)) {
            if(startsWith(hemi_order_on_axis, 'auto')) {
                if(min(lh_src_mesh$vertices[,axis]) < min(rh_src_mesh$vertices[,axis])) {
                    lh_is_first_on_axis = TRUE;
                } else {
                    lh_is_first_on_axis = FALSE;
                }
                if(hemi_order_on_axis == 'auto_flipped') {
                    lh_is_first_on_axis = !(lh_is_first_on_axis);
                }
            } else if(hemi_order_on_axis == 'lr') {
                lh_is_first_on_axis = TRUE;
            } else {
                lh_is_first_on_axis = FALSE;
            }

            if(lh_is_first_on_axis) {
                overlap = max(lh_src_mesh$vertices[,axis]) - min(rh_src_mesh$vertices[,axis]) + min_dist;
                #cat(sprintf("overlap=%f, min_dist=%f\n", overlap, min_dist));
                if(overlap > 0L) {
                    shift_by = c(-overlap/2.0, overlap/2.0);
                } else {
                    return(coloredmeshes_hl);
                }
            } else {
                overlap = max(rh_src_mesh$vertices[,axis]) - min(lh_src_mesh$vertices[,axis]) + min_dist;
                if(overlap > 0L) {
                    shift_by = c(overlap/2.0, -overlap/2.0);
                } else {
                    return(coloredmeshes_hl);
                }
            }
        }

        lh_src_mesh$vertices[,axis] = lh_src_mesh$vertices[,axis] + shift_by[1];
        lh_new_tmesh = rgl::tmesh3d(c(t(lh_src_mesh$vertices)), c(t(lh_src_mesh$faces)), homogeneous=FALSE);
        coloredmeshes_hl$lh$mesh$vb = lh_new_tmesh$vb;
        coloredmeshes_hl$lh$mesh$it = lh_new_tmesh$it;


        rh_src_mesh$vertices[,axis] = rh_src_mesh$vertices[,axis] + shift_by[2];
        rh_new_tmesh = rgl::tmesh3d(c(t(rh_src_mesh$vertices)), c(t(rh_src_mesh$faces)), homogeneous=FALSE);
        coloredmeshes_hl$rh$mesh$vb = rh_new_tmesh$vb;
        coloredmeshes_hl$rh$mesh$it = rh_new_tmesh$it;
    } else {
        warning("Missing coloredmesh metadata, ignored request to shift meshes apart");
    }

    return(coloredmeshes_hl);
}


