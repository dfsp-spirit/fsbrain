
#' @title Visualize a list of colored meshes from several angles. Test function, will be removed.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to rgl::bg3d()
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'morph_data_was_all_na' set to TRUE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#' @export
# @keywords internal
#' @importFrom rgl open3d bg3d wire3d
vis.mult.coloredmeshes <- function(coloredmeshes, background="white", skip_all_na=TRUE) {

    if(!is.list(coloredmeshes)) {
        stop("Parameter coloredmeshes must be a list.");
    }
    num_sub_meshes = length(coloredmeshes); # The number of submeshes that each object consists of.

    layout_dim_x = 2;
    layout_dim_y = 3;
    num_views = layout_dim_x * layout_dim_y;

    rgl::open3d();
    rgl::bg3d(background);
    rgl::mfrow3d(layout_dim_x, layout_dim_y);
    for (view_idx in 1:num_views) {
        rgl::next3d();
        for (mesh_idx in 1:num_sub_meshes) {
            orig_mesh = coloredmeshes[[mesh_idx]]$mesh;
            rotation_angle = pi/2 * (view_idx - 1);
            rotated_mesh = rgl::rotate3d(orig_mesh, rotation_angle, 0, 1, 0);
            rgl::shade3d(rotated_mesh, col = coloredmeshes[[mesh_idx]]$col);
        }
    }

    rgl::highlevel(integer()); # To trigger display as rglwidget
}


#' @title Visualize a list of colored meshes from four angles.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to rgl::bg3d()
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'morph_data_was_all_na' set to TRUE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param draw_labels logical, whether to draw label text for the different views that show information on the view direction and hemisphere displayed in a subplot. Defaults to TRUE.
#'
#' @export
# @keywords internal
#' @importFrom rgl open3d bg3d wire3d shade3d mfrow3d next3d text3d
vis.mult.coloredmeshes.stdview4 <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", draw_labels = TRUE) {

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
    rgl::bg3d(background);
    rgl::mfrow3d(layout_dim_x, layout_dim_y);

    # Create the upper left view: draw only the left hemi, from the left
    rgl::next3d();
    vis.rotated.coloredmeshes(lh_meshes, pi/2, 1, 0, 0, style=style);
    rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
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

    rgl::highlevel(integer()); # To trigger display as rglwidget
}



#' @title Visualize a list of colored meshes from nine angles.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to rgl::bg3d()
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'morph_data_was_all_na' set to TRUE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param draw_labels logical, whether to draw label text for the different views that show information on the view direction and hemisphere displayed in a subplot. Defaults to TRUE.
#'
#' @export
# @keywords internal
#' @importFrom rgl open3d bg3d wire3d shade3d mfrow3d next3d text3d
vis.mult.coloredmeshes.stdview9 <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", draw_labels = TRUE) {

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
    rgl::bg3d(background);
    rgl::mfrow3d(layout_dim_x, layout_dim_y);


    #  ------------------ Row 1 --------------------


    # Create the upper left view: draw only the left hemi, from the left
    rgl::next3d();
    vis.rotated.coloredmeshes(lh_meshes, pi/2, 1, 0, 0, style=style);
    rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"lateral lh");
    }

    # Create the upper central view: draw both hemis from above (top view)
    rgl::next3d();
    vis.rotated.coloredmeshes(coloredmeshes, 0, 1, 0, 0, style=style);
    rgl.viewpoint(0, 0, fov=0, interactive=FALSE);
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
    rgl.viewpoint(0, 0, fov=0, interactive=FALSE);
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
        rgl::text3d(0,label_shift_y,0,"");
    }

    # Create the bottom right view
    rgl::next3d();
    vis.rotated.coloredmeshes(coloredmeshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(180, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"caudal");
    }

    rgl::highlevel(integer()); # To trigger display as rglwidget
}

