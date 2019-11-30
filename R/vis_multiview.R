

#' @title Visualize a list of colored meshes from a single viewpoint, interactively.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to \code{\link[rgl]{bg3d}}.
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'morph_data_was_all_na' set to TRUE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param draw_labels logical, whether to draw label text for the different views that show information on the view direction and hemisphere displayed in a subplot. Defaults to FALSE.
#'
#' @param rgloptions, named list. Parameters passed to \code{\link[rgl]{par3d}}. Defaults to the empty list.
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. Defaults to the empty list.
#'
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d wire3d shade3d mfrow3d next3d text3d rgl.viewpoint
brainview.si <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", draw_labels = FALSE, rgloptions = list(), rglactions=list(), draw_colorbar = FALSE) {

    if(!is.list(coloredmeshes)) {
        stop("Parameter 'coloredmeshes' must be a list.");
    }
    invisible(vis.coloredmeshes(coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar));
}


#' @title Visualize a list of colored meshes, rotating the camera around them.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to \code{\link[rgl]{bg3d}}.
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'morph_data_was_all_na' set to TRUE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param draw_labels logical, whether to draw label text for the different views that show information on the view direction and hemisphere displayed in a subplot. Defaults to FALSE.
#'
#' @param x rotation x axis value, passed to \code{\link[rgl]{spin3d}}. Defaults to 0.
#'
#' @param y rotation y axis value, passed to \code{\link[rgl]{spin3d}}. Defaults to 1.
#'
#' @param z rotation z axis value, passed to \code{\link[rgl]{spin3d}}. Defaults to 0.
#'
#' @param rpm rotation rpm value, passed to \code{\link[rgl]{spin3d}}. Defaults to 15.
#'
#' @param duration rotation duration value, passed to \code{\link[rgl]{spin3d}}. Defaults to 20.
#'
#' @param rgloptions, named list. Parameters passed to \code{\link[rgl]{par3d}}. Defaults to the empty list.
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. Defaults to the empty list.
#'
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d wire3d shade3d mfrow3d next3d text3d rgl.viewpoint
brainview.sr <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", draw_labels = FALSE, x=0, y=1, z=0, rpm=6, duration=10, rgloptions = list(), rglactions=list(), draw_colorbar = FALSE) {

    if(!is.list(coloredmeshes)) {
        stop("Parameter 'coloredmeshes' must be a list.");
    }
    invisible(vis.coloredmeshes.rotating(coloredmeshes, x=x, y=y, z=z, rpm=rpm, duration=duration, rgloptions = rgloptions, rglactions = rglactions));
}


#' @title Visualize a list of colored meshes from four angles.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to \code{\link[rgl]{bg3d}}.
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'morph_data_was_all_na' set to TRUE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param draw_labels logical, whether to draw label text for the different views that show information on the view direction and hemisphere displayed in a subplot. Defaults to FALSE.
#'
#' @param rgloptions, named list. Parameters passed to \code{\link[rgl]{par3d}}. Defaults to the empty list.
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d wire3d shade3d mfrow3d next3d text3d rgl.viewpoint
brainview.t4 <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", draw_labels = FALSE, rgloptions = list(), rglactions = list(), draw_colorbar = FALSE) {

    label_shift_y = -20;

    if(!is.list(coloredmeshes)) {
        stop("Parameter 'coloredmeshes' must be a list.");
    }

    layout_dim_x = 2;
    layout_dim_y = 2;
    num_views = layout_dim_x * layout_dim_y;

    coloredmeshes = unify.coloredmeshes.colormaps(coloredmeshes);

    hemi_sorted_cmeshes = sort.coloredmeshes.by.hemi(coloredmeshes);

    lh_meshes = hemi_sorted_cmeshes$lh;
    rh_meshes = hemi_sorted_cmeshes$rh;


    rgl::open3d();
    do.call(rgl::par3d, rgloptions);
    Sys.sleep(1);
    rgl::bg3d(background);
    rgl::mfrow3d(layout_dim_x, layout_dim_y);

    # Create the upper left view: draw only the left hemi, from the left
    rgl::next3d();
    vis.rotated.coloredmeshes(lh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"lateral lh");
    }

    # Create the upper right view
    rgl::next3d();
    vis.rotated.coloredmeshes(rh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"lateral rh");
    }


    # Create the lower left view
    rgl::next3d();
    vis.rotated.coloredmeshes(lh_meshes, pi/2, 1, 0, 0, style=style, draw_colorbar=(draw_colorbar && (length(rh_meshes)==0)));
    rgl::rgl.viewpoint(90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"medial lh");
    }


    # Create the lower right view
    rgl::next3d();
    vis.rotated.coloredmeshes(rh_meshes, pi/2, 1, 0, 0, style=style, draw_colorbar=draw_colorbar);
    rgl::rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"medial rh");
    }


    perform.rglactions(rglactions);
    invisible(coloredmeshes);
}


#' @title Perform rglactions, like taking screenshots.
#'
#' @description Take a list specifying actions and execute them. This function should be called once an rgl scene has been setup and rendered. A typical usecase is to save a screenshot of the scene.
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @param at_index integer, the index to use in case of vectorized entries. Allows using different output_images for different views or similar.
#'
#' @keywords internal
#' @importFrom rgl rgl.snapshot
perform.rglactions <- function(rglactions, at_index=NULL) {
    if(is.list(rglactions)) {
        if("snapshot_png" %in% names(rglactions)) {
            if(length(rglactions$snapshot_png) == 1 || is.null(at_index)) {
                output_image = path.expand(rglactions$snapshot_png);
            } else {
                if(length(rglactions$snapshot_png) < at_index) {
                    warning(sprintf("Requested rglaction at_index '%d' but only %d entries exist for action 'snapshot_png'.\n", at_index, length(rglactions$snapshot_png)));
                }
                output_image = path.expand(rglactions$snapshot_png[[at_index]]);
            }
            rgl::rgl.snapshot(output_image, fmt="png");
            message(sprintf("Screenshot written to '%s' (current working dir is '%s').\n", output_image, getwd()));
        }
    }
}


#' @title Check for a key in names of rglactions.
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @return logical, whether the rglactions instance has the requested key as a name.
#'
#' @keywords internal
rglactions.has.key <- function(rglactions, key) {
    if(is.list(rglactions)) {
        return(key %in% names(rglactions));
    }
    return(FALSE);
}


#' @title Visualize a list of colored meshes from nine angles.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to \code{\link[rgl]{bg3d}}.
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'morph_data_was_all_na' set to TRUE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param draw_labels logical, whether to draw label text for the different views that show information on the view direction and hemisphere displayed in a subplot. Defaults to FALSE.
#'
#' @param rgloptions, named list. Parameters passed to \code{\link[rgl]{par3d}}. Defaults to the empty list. To increase plot resolution to 2000x1600 px, try: \code{rgloptions=list("windowRect"=c(50,50,2000,1600))}.
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d wire3d shade3d mfrow3d next3d text3d rgl.viewpoint
brainview.t9 <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", draw_labels = FALSE, rgloptions = list(), rglactions = list(), draw_colorbar = FALSE) {


    label_shift_y = -20;
    label_shift_y_dorsal = -120;
    label_shift_y_ventral = -80;

    if(!is.list(coloredmeshes)) {
        stop("Parameter 'coloredmeshes' must be a list.");
    }

    layout_dim_x = 3;
    layout_dim_y = 3;
    num_views = layout_dim_x * layout_dim_y;

    coloredmeshes = unify.coloredmeshes.colormaps(coloredmeshes);

    hemi_sorted_cmeshes = sort.coloredmeshes.by.hemi(coloredmeshes);
    lh_meshes = hemi_sorted_cmeshes$lh;
    rh_meshes = hemi_sorted_cmeshes$rh;


    rgl::open3d();
    do.call(rgl::par3d, rgloptions);
    Sys.sleep(1);
    rgl::bg3d(background);
    rgl::mfrow3d(layout_dim_x, layout_dim_y, sharedMouse = TRUE);


    #  ------------------ Row 1 --------------------


    # Create the upper left view: draw only the left hemi, from the left
    rgl::next3d();
    vis.rotated.coloredmeshes(lh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"lateral lh");
    }

    # Create the upper central view: draw both hemis from above (top view)
    rgl::next3d();
    vis.rotated.coloredmeshes(coloredmeshes, 0, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(0, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y_dorsal,0,"dorsal");
    }

    # Create the upper right view
    rgl::next3d();
    vis.rotated.coloredmeshes(rh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"lateral rh");
    }


    #  ------------------ Row 2 --------------------


    # Create the 2nd row left view
    rgl::next3d();
    vis.rotated.coloredmeshes(lh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"medial lh");
    }

    # Create the 2nd row central view: draw both hemis from below (bottom view)
    rgl::next3d();
    vis.rotated.coloredmeshes(coloredmeshes, pi, 1, 0, 0, style=style, draw_colorbar = draw_colorbar);
    rgl::rgl.viewpoint(0, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y_ventral,0,"ventral");
    }


    # Create the 2nd row right view
    rgl::next3d();
    vis.rotated.coloredmeshes(rh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"medial rh");
    }


    #  ------------------ Row 3 --------------------


    # Create the bottom left view: draw only the left hemi, from the left
    rgl::next3d();
    vis.rotated.coloredmeshes(coloredmeshes, pi/2, 1, 0, 0, style=style);
    rgl.viewpoint(0, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"rostral");
    }

    # Create the bottom central view.
    rgl::next3d();
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"(empty)");
    } else {
        # If we do not draw anything, the next3d() calls seems to get ignored,
        #  and the next image gets places here instead of into the last field.
        #  Therefore, we draw empty text for now.
        rgl::text3d(0,label_shift_y,0,"");

    }

    # Create the bottom right view
    rgl::next3d();
    vis.rotated.coloredmeshes(coloredmeshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(180, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"caudal");
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
#' @param background string, background color passed to \code{\link[rgl]{bg3d}}.
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'morph_data_was_all_na' set to TRUE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param rgloptions, named list. Parameters passed to \code{\link[rgl]{par3d}}. Defaults to the empty list. To increase plot resolution to 2000x1600 px, try: \code{rgloptions=list("windowRect"=c(50,50,2000,1600))}.
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d rgl.viewpoint
brainview.sd <- function(coloredmeshes, view_angle, background="white", skip_all_na=TRUE, style="default", rgloptions = list(), rglactions = list(), draw_colorbar = FALSE) {


    if(!is.list(coloredmeshes)) {
        stop("Parameter 'coloredmeshes' must be a list.");
    }

    coloredmeshes = unify.coloredmeshes.colormaps(coloredmeshes);

    hemi_sorted_cmeshes = sort.coloredmeshes.by.hemi(coloredmeshes);
    lh_meshes = hemi_sorted_cmeshes$lh;
    rh_meshes = hemi_sorted_cmeshes$rh;


    rgl::open3d();
    do.call(rgl::par3d, rgloptions);
    Sys.sleep(1);
    rgl::bg3d(background);


    if(view_angle == "lateral_lh") {
        vis.rotated.coloredmeshes(lh_meshes, pi/2, 1, 0, 0, style=style, draw_colorbar = draw_colorbar);
        rgl::rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
    } else if (view_angle == "dorsal") {
        vis.rotated.coloredmeshes(coloredmeshes, 0, 1, 0, 0, style=style, draw_colorbar = draw_colorbar);
        rgl::rgl.viewpoint(0, 0, fov=0, interactive=FALSE);
    } else if(view_angle == "lateral_rh") {
        vis.rotated.coloredmeshes(rh_meshes, pi/2, 1, 0, 0, style=style, draw_colorbar = draw_colorbar);
        rgl::rgl.viewpoint(90, 0, fov=0, interactive=FALSE);
    } else if(view_angle == "medial_lh") {
        vis.rotated.coloredmeshes(lh_meshes, pi/2, 1, 0, 0, style=style, draw_colorbar = draw_colorbar);
        rgl::rgl.viewpoint(90, 0, fov=0, interactive=FALSE);
    } else if(view_angle == "ventral") {
        vis.rotated.coloredmeshes(coloredmeshes, pi, 1, 0, 0, style=style, draw_colorbar = draw_colorbar);
        rgl::rgl.viewpoint(0, 0, fov=0, interactive=FALSE);
    } else if(view_angle == "medial_rh") {
        vis.rotated.coloredmeshes(rh_meshes, pi/2, 1, 0, 0, style=style, draw_colorbar = draw_colorbar);
        rgl::rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
    } else if(view_angle == "rostral") {
        vis.rotated.coloredmeshes(coloredmeshes, pi/2, 1, 0, 0, style=style, draw_colorbar = draw_colorbar);
        rgl.viewpoint(0, 0, fov=0, interactive=FALSE);
    } else if(view_angle == "caudal") {
        vis.rotated.coloredmeshes(coloredmeshes, pi/2, 1, 0, 0, style=style, draw_colorbar = draw_colorbar);
        rgl::rgl.viewpoint(180, 0, fov=0, interactive=FALSE);
    } else {
        stop(sprintf("Invalid view_angle '%s'. Must be one of 'lateral_lh', 'dorsal', 'lateral_rh', 'medial_lh', 'ventral', 'medial_rh', 'rostral' or 'caudal'.\n", view_angle));
    }

    perform.rglactions(rglactions);

    return(invisible(coloredmeshes));
}

