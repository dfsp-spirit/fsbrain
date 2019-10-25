

#' @title Visualize a list of colored meshes from a single viewpoint, interactively.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to rgl::bg3d()
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'morph_data_was_all_na' set to TRUE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param draw_labels logical, whether to draw label text for the different views that show information on the view direction and hemisphere displayed in a subplot. Defaults to FALSE.
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d wire3d shade3d mfrow3d next3d text3d rgl.viewpoint
brainview.si <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", draw_labels = FALSE, rgloptions = list()) {

    if(!is.list(coloredmeshes)) {
        stop("Parameter 'coloredmeshes' must be a list.");
    }
    invisible(vis.coloredmeshes(coloredmeshes, rgloptions = rgloptions));
}


#' @title Visualize a list of colored meshes, rotating the camera around them.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to rgl::bg3d()
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'morph_data_was_all_na' set to TRUE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param draw_labels logical, whether to draw label text for the different views that show information on the view direction and hemisphere displayed in a subplot. Defaults to FALSE.
#'
#' @param x rotation x axis value, passed to [rgl::spin3d()]. Defaults to 0.
#'
#' @param y rotation y axis value, passed to [rgl::spin3d()]. Defaults to 1.
#'
#' @param z rotation z axis value, passed to [rgl::spin3d()]. Defaults to 0.
#'
#' @param rpm rotation rpm value, passed to [rgl::spin3d()]. Defaults to 15.
#'
#' @param duration rotation duration value, passed to [rgl::spin3d()]. Defaults to 20.
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d wire3d shade3d mfrow3d next3d text3d rgl.viewpoint
brainview.sr <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", draw_labels = FALSE, x=0, y=1, z=0, rpm=15, duration=20, rgloptions = list()) {

    if(!is.list(coloredmeshes)) {
        stop("Parameter 'coloredmeshes' must be a list.");
    }
    invisible(vis.coloredmeshes.rotating(coloredmeshes, x=x, y=y, z=z, rpm=rpm, duration=duration, rgloptions = rgloptions));
}


#' @title Visualize a list of colored meshes from four angles.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to rgl::bg3d()
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'morph_data_was_all_na' set to TRUE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param draw_labels logical, whether to draw label text for the different views that show information on the view direction and hemisphere displayed in a subplot. Defaults to FALSE.
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d wire3d shade3d mfrow3d next3d text3d rgl.viewpoint
brainview.t4 <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", draw_labels = FALSE, rgloptions = list()) {

    label_shift_y = -20;

    if(!is.list(coloredmeshes)) {
        stop("Parameter 'coloredmeshes' must be a list.");
    }

    layout_dim_x = 2;
    layout_dim_y = 2;
    num_views = layout_dim_x * layout_dim_y;

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
    vis.rotated.coloredmeshes(lh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"medial lh");
    }


    # Create the lower right view
    rgl::next3d();
    vis.rotated.coloredmeshes(rh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"medial rh");
    }

    #rgl::highlevel(integer()); # To trigger display as rglwidget

    invisible(coloredmeshes);
}



#' @title Visualize a list of colored meshes from nine angles.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to rgl::bg3d()
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'morph_data_was_all_na' set to TRUE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param draw_labels logical, whether to draw label text for the different views that show information on the view direction and hemisphere displayed in a subplot. Defaults to FALSE.
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d wire3d shade3d mfrow3d next3d text3d rgl.viewpoint
brainview.t9 <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", draw_labels = FALSE, rgloptions = list()) {

    #do_draw_colorbar = FALSE;

    label_shift_y = -20;
    label_shift_y_dorsal = -120;
    label_shift_y_ventral = -80;

    if(!is.list(coloredmeshes)) {
        stop("Parameter 'coloredmeshes' must be a list.");
    }

    layout_dim_x = 3;
    layout_dim_y = 3;
    num_views = layout_dim_x * layout_dim_y;

    hemi_sorted_cmeshes = sort.coloredmeshes.by.hemi(coloredmeshes);
    lh_meshes = hemi_sorted_cmeshes$lh;
    rh_meshes = hemi_sorted_cmeshes$rh;


    rgl::open3d();
    do.call(rgl::par3d, rgloptions);
    Sys.sleep(1);
    rgl::bg3d(background);
    rgl::mfrow3d(layout_dim_x, layout_dim_y);


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
    vis.rotated.coloredmeshes(coloredmeshes, pi, 1, 0, 0, style=style);
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
        rgl::text3d(0,label_shift_y,0,"rostal");
    }

    # Create the bottom central view. Empty, could later draw colorbar here.
    rgl::next3d();
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"(empty)");
    } else {
        # If we do not draw anything, the next3d() calls seems to get ignored,
        #  and the next image gets places here instead of into the last field.
        #  Therefore, we draw empty text for now.
        rgl::text3d(0,label_shift_y,0,"");

        #if(do_draw_colorbar) {
        #    rgl::bgplot3d({
        #        graphics::plot.new();
        #        plotrix::color.legend(0.1, 0.1, 0.9, 0.9,
        #                     rect.col=grDevices::rainbow(1000),
        #                     legend=(-3):3, gradient="y", cex = 1.5)
        #    });
        #}
    }

    # Create the bottom right view
    rgl::next3d();
    vis.rotated.coloredmeshes(coloredmeshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(180, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"caudal");
    }

    #rgl::highlevel(integer()); # To trigger display as rglwidget
    invisible(coloredmeshes);
}

