# Low-level visualization function for meshes.


#' @title Visualize a list of colored meshes in a single scene.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to rgl::bg3d()
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'morph_data_was_all_na' set to TRUE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param rgloptions option list passed to [rgl::par3d()]. Example: rgloptions = list("windowRect"=c(50,50,1000,1000));
#'
#' @return the list of visualized coloredmeshes
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d wire3d
vis.coloredmeshes <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", rgloptions=list()) {

    if(!is.list(coloredmeshes)) {
        stop("Parameter coloredmeshes must be a list.");
    }

    #rgloptions = list(windowRect=c(50,50,1000,1000));

    rgl::open3d();
    do.call(rgl::par3d, rgloptions);
    Sys.sleep(1);

    rgl::bg3d(background);
    for(cmesh in coloredmeshes) {
        if(skip_all_na && cmesh$morph_data_was_all_na) {
            next;
        }
        vis.coloredmesh(cmesh, style = style);
    }

    invisible(coloredmeshes);
}


#' @title Visualize a list of colored meshes in a single scene and rotate them.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to rgl::bg3d()
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'morph_data_was_all_na' set to TRUE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
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
#' @param rgloptions option list passed to [rgl::par3d()]. Example: rgloptions = list("windowRect"=c(50,50,1000,1000));
#'
#' @return the list of visualized coloredmeshes
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d wire3d
vis.coloredmeshes.rotating <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", x=0, y=1, z=0, rpm=15, duration=20, rgloptions=list()) {

    if(!is.list(coloredmeshes)) {
        stop("Parameter coloredmeshes must be a list.");
    }

    #rgloptions = list("windowRect"=c(50,50,1000,1000));

    rgl::open3d();
    do.call(rgl::par3d, rgloptions);
    Sys.sleep(1);
    rgl::bg3d(background);
    for(cmesh in coloredmeshes) {
        if(skip_all_na && cmesh$morph_data_was_all_na) {
            next;
        }
        vis.coloredmesh(cmesh, style = style);
    }

    if (!rgl::rgl.useNULL()) {
        rgl::play3d(rgl::spin3d(axis = c(x, y, z), rpm = rpm), duration = duration);
    } else {
        warning("Cannot show rotating scene with NULL device.");
    }
    invisible(coloredmeshes);
}



#' @title Rotate and visualize coloredmeshes, applying a style.
#'
#' @param coloredmeshes, list of coloredmeshes. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param rotation_angle, angle in radians. Passed to [rgl:rotated3d()].
#'
#' @param x, x value passed to [rgl:rotated3d()].
#'
#' @param y, y value passed to [rgl:rotated3d()].
#'
#' @param z, z value passed to [rgl:rotated3d()].
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @keywords internal
vis.rotated.coloredmeshes <- function(coloredmeshes, rotation_angle, x, y, z, style="default") {
    for (mesh_idx in seq_len(length(coloredmeshes))) {     # usually this will only run once for the single mesh of a hemisphere.
        orig_cmesh = coloredmeshes[[mesh_idx]];
        orig_mesh = orig_cmesh$mesh;
        rotated_mesh = rgl::rotate3d(orig_mesh, rotation_angle, x, y, z);
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


#' @title Sort coloredmeshes into 2 lists by their 'hemi' property.
#'
#' @param coloredmeshes list of coloredmeshes
#'
#' @return named list with two entries: "lh": list of coloredmeshes that have property hemi set to 'lh'. "rh": list of coloredmeshes that have property hemi set to 'rh'. The rest is ignored.
#'
#' @keywords internal
sort.coloredmeshes.by.hemi <- function(coloredmeshes) {
    lh_meshes = list();
    rh_meshes = list();
    for (mesh_idx in seq_len(length(coloredmeshes))) {
        cmesh = coloredmeshes[[mesh_idx]];
        if(! ('hemi' %in% names(cmesh))) {
            warning(sprintf("Ignoring coloredmesh # %d which has no hemi value at all.\n", mesh_idx));
        } else {
            if(cmesh$hemi == 'lh') {
                mesh_name = sprintf("mesh%d", mesh_idx);
                lh_meshes[[mesh_name]] = cmesh;
            } else if(cmesh$hemi == 'rh') {
                rh_meshes[[mesh_name]] = cmesh;
            } else {
                warning(sprintf("Ignoring mesh # %d with invalid hemi value '%s'.\n", mesh_idx, cmesh$hemi));
            }
        }
    }
    return(list("lh"=lh_meshes, "rh"=rh_meshes));
}

