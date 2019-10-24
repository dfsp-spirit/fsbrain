
#' @title Visualize a list of colored meshes from several angles.
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


#' @title Visualize a list of colored meshes from several angles.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to rgl::bg3d()
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'morph_data_was_all_na' set to TRUE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#' @export
# @keywords internal
#' @importFrom rgl open3d bg3d wire3d shade3d mfrow3d next3d text3d
vis.mult.coloredmeshes.stdview4 <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default") {

    draw_labels = TRUE;
    label_shift_y = -20;

    if(!is.list(coloredmeshes)) {
        stop("Parameter coloredmeshes must be a list.");
    }
    num_sub_meshes = length(coloredmeshes); # The number of submeshes that each object consists of.

    layout_dim_x = 2;
    layout_dim_y = 2;
    num_views = layout_dim_x * layout_dim_y;

    lh_meshes = list();
    rh_meshes = list();
    for (mesh_idx in 1:num_sub_meshes) {
        cmesh = coloredmeshes[[mesh_idx]];
        if(! ('hemi' %in% names(cmesh))) {
            warning(sprintf("Ignoring coloredmesh # %d which has no hemi value at all.\n", mesh_idx));
        } else {
            if(cmesh$hemi == 'lh') {
                mesh_name = sprintf("mesh%d", mesh_idx);
                lh_meshes[[mesh_name]] = cmesh;
                cat(sprintf("Assigning mesh %d named %s to lh\n", mesh_idx, mesh_name));
            } else if(cmesh$hemi == 'rh') {
                rh_meshes[[mesh_name]] = cmesh;
                cat(sprintf("Assigning mesh %d named %s to rh\n", mesh_idx, mesh_name));
            } else {
                warning(sprintf("Ignoring mesh # %d with invalid hemi value '%s'.\n", mesh_idx, cmesh$hemi));
            }
        }
    }
    cat(sprintf("Received %d lh meshes and %d rh meshes. Received %d meshes total.\n", length(lh_meshes), length(rh_meshes), num_sub_meshes));

    rgl::open3d();
    rgl::bg3d(background);
    rgl::mfrow3d(layout_dim_x, layout_dim_y);

    # Create the upper left view: draw only the left hemi, from the left
    rgl::next3d();
    for (mesh_idx in seq_len(length(lh_meshes))) {     # usually this will only run once for the single lh mesh.
        orig_mesh = lh_meshes[[mesh_idx]]$mesh;
        rotation_angle = pi/2;
        rotated_mesh = rgl::rotate3d(orig_mesh, rotation_angle, 1, 0, 0);
        rgl::shade3d(rotated_mesh, col = lh_meshes[[mesh_idx]]$col);
    }
    rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"lateral lh");
    }

    # Create the upper right view
    rgl::next3d();
    for (mesh_idx in seq_len(length(rh_meshes))) {     # usually this will only run once for the single rh mesh.
        orig_mesh = rh_meshes[[mesh_idx]]$mesh;
        rotation_angle = pi/2;
        rotated_mesh = rgl::rotate3d(orig_mesh, rotation_angle, 1, 0, 0);
        rgl::shade3d(rotated_mesh, col = rh_meshes[[mesh_idx]]$col);
    }
    rgl::rgl.viewpoint(90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"lateral rh");
    }


    # Create the lower left view
    rgl::next3d();
    #for (mesh_idx in seq_len(length(lh_meshes))) {     # usually this will only run once for the single lh mesh.
    #    orig_mesh = lh_meshes[[mesh_idx]]$mesh;
    #    rotation_angle = pi/2;
    #    rotated_mesh = rgl::rotate3d(orig_mesh, rotation_angle, 1, 0, 0);
    #    rgl::shade3d(rotated_mesh, col = lh_meshes[[mesh_idx]]$col);
    #}
    vis.rotated.coloredmeshes(lh_meshes, pi/2, 1, 0, 0, style=style);
    rgl::rgl.viewpoint(90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"medial lh");
    }


    # Create the lower right view
    rgl::next3d();
    for (mesh_idx in seq_len(length(rh_meshes))) {     # usually this will only run once for the single lh mesh.
        orig_mesh = rh_meshes[[mesh_idx]]$mesh;
        rotation_angle = pi/2;
        rotated_mesh = rgl::rotate3d(orig_mesh, rotation_angle, 1, 0, 0);
        #rgl::shade3d(rotated_mesh, col = rh_meshes[[mesh_idx]]$col);
        rotated_cmesh = rh_meshes[[mesh_idx]];
        rotated_cmesh$mesh = rotated_mesh;
        vis.coloredmesh(rotated_cmesh);
    }
    rgl::rgl.viewpoint(-90, 0, fov=0, interactive=FALSE);
    if(draw_labels) {
        rgl::text3d(0,label_shift_y,0,"medial rh");
    }



    rgl::highlevel(integer()); # To trigger display as rglwidget
}


#' @keywords internal
vis.rotated.coloredmeshes <- function(coloredmeshes, rotation_angle, ax, ay, az, style="default") {
    for (mesh_idx in seq_len(length(coloredmeshes))) {     # usually this will only run once for the single mesh of a hemisphere.
        orig_cmesh = coloredmeshes[[mesh_idx]];
        orig_mesh = orig_cmesh$mesh;
        rotated_mesh = rgl::rotate3d(orig_mesh, rotation_angle, ax, ay, az);
        rotated_cmesh = orig_cmesh;         # copy coloredmesh
        rotated_cmesh$mesh = rotated_mesh;  # replace inner mesh with rotated version
        vis.coloredmesh(rotated_cmesh, style=style);
    }
}

#' @title Draw a coloredmesh using a style.
#'
#' @param cmesh, a coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @keywords internal
vis.coloredmesh <- function(cmesh, style="default") {
    if(is.list(style)) {
        style_params = style;
    } else if (is.character(style)) {
        style_params = get.rglstyle(style);
    } else {
        stop("Parameter 'style' must be a named list of style parameters or a string specifying an available style by name (e.g., 'default' or 'shiny').");
    }
    do.call(rgl::shade3d, c(list(cmesh$mesh, col=cmesh$col), style_params));
}


#' @title Get the default visualization style parameters as a named list.
#'
#' @param style, string. A style name. Available styles are one of: "default", "shiny".
#'
#' @keywords internal
get.rglstyle <- function(style) {
    if(style == "default") {
        return(get.rglstyle.default());
    } else if (style == "shiny") {
        return(get.rglstyle.shiny());
    } else {
        stop(sprintf("No such style: '%s'.\n", style));
    }
}


#' @title Get the default visualization style parameters as a named list.
#'
#' @return named list, style parameters that can be passed to [rgl::shade3d()] via [base::do.call()].
#'
#' @keywords internal
get.rglstyle.default <- function() {
    return(list("shininess"=50, specular="black"));
}


#' @title Get a shiny visualization style.
#'
#' @return named list, style parameters that can be passed to [rgl::shade3d()] via [base::do.call()].
#'
#' @keywords internal
get.rglstyle.shiny <- function() {
    return(list("shininess"=50, specular="white"));
}
