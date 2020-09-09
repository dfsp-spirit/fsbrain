# Low-level visualization function for meshes.


#' @title Visualize a list of colored meshes in a single scene.
#'
#' @param coloredmeshes list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to rgl::bg3d()
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'render' set to FALSE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style, a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param rgloptions option list passed to \code{\link{par3d}}. Example: \code{rgloptions = list("windowRect"=c(50,50,1000,1000))};
#'
#' @param rglactions named list. A list in which the names are from a set of pre-defined actions. Defaults to the empty list.
#'
#' @param draw_colorbar logical. Whether to draw a colorbar. WARNING: Will only show up if there is enough space in the plot area and does not resize properly. Defaults to FALSE. See \code{\link[fsbrain]{coloredmesh.plot.colorbar.separate}} for an alternative.
#'
#' @note To change or adapt the colorbar, you should use the makecmap_options parameter when constructing them in a vis function. See the example.
#'
#' @return the list of visualized coloredmeshes
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    cm = vis.subject.morph.native(subjects_dir, 'subject1', 'thickness',
#'     makecmap_options=list('n'=100));
#'    # You could mess with the meshes here.
#'    vis.coloredmeshes(cm);
#' }
#'
#' @importFrom rgl open3d bg3d wire3d par3d
#' @export
vis.coloredmeshes <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", rgloptions = rglo(), rglactions=list(), draw_colorbar=FALSE) {

    if(!is.list(coloredmeshes)) {
        stop("Parameter coloredmeshes must be a list.");
    } else {
        # The check above does not help a lot, since all classes are also derived from 'list', so that will be true in most cases.
        if(length(coloredmeshes) < 1) {
            warning("Nothing to visualize.");
            return(invisible(NULL));
        } else {
            if(! fsbrain.renderable(coloredmeshes[[1]])) {
                stop("Pass a list of 'fs.coloredmesh', 'fs.coloredvoxels', or 'Triangles3D' instances.");
            }
        }
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

    rgl::open3d();
    do.call(rgl::par3d, rgloptions);
    Sys.sleep(1);

    rgl::bg3d(background);

    if(is.character(draw_colorbar)) {
        rgl::layout3d(layout_mat, widths=layout_column_widths, height=layout_row_heights);
        rgl::next3d(reuse=TRUE);
    }

    for(cmesh in coloredmeshes) {
        vis.renderable(cmesh, skip_all_na=skip_all_na, style=style);
    }

    if(is.character(draw_colorbar)) {
        rgl::next3d(reuse=FALSE);
        draw.colorbar(coloredmeshes, horizontal=horizontal);
    }

    perform.rglactions(rglactions);
    return(invisible(coloredmeshes));
}


#' @title Check whether object can be rendered by fsbrain
#'
#' @param x any `R` object
#'
#' @return TRUE if *x* is an instance of a class that can be rendered by fsbrain visualization functions, and FALSE otherwise. Currently, the following types are renderable: `fs.coloredvoxels`, `fs.coloredmesh`, `Triangles3D`.
#'
#' @seealso \code{\link[fsbrain]{is.Triangles3D}}
#'
#' @keywords internal
fsbrain.renderable <- function(x) {
    return(is.fs.coloredvoxels(x) | is.fs.coloredmesh(x) | is.Triangles3D(x));
}


#' @title Check whether object is a Triangles3D instance
#'
#' @param x any `R` object
#'
#' @return TRUE if its argument is a Triangles3D instance (that is, has "Triangles3D" amongst its classes) and FALSE otherwise.
#'
#' @keywords internal
is.Triangles3D <- function(x) inherits(x, "Triangles3D")


#' @title Visualize a renderable object
#'
#' @description Renders instances of `coloredmesh`, `coloredvoxels` and `Triangles3D`.
#'
#' @param cmesh an instance of one of the supported renderable classes
#'
#' @param skip_all_na logical, whether to skip rendering hidden instances
#'
#' @param style a rendering style, can be a style name or a list defining an rgl material style
#'
#' @seealso \code{\link[fsbrain]{fsbrain.renderable}}
#'
#' @keywords internal
#' @importFrom utils modifyList
#' @importFrom rgl triangles3d
vis.renderable <- function(cmesh, skip_all_na=TRUE, style="default") {
    if(is.fs.coloredmesh(cmesh)) {
        if(!(skip_all_na && !cmesh$render)) {
            vis.coloredmesh(cmesh, style = style);
        }
    } else if (is.fs.coloredvoxels(cmesh)) {
        style_params = get.rglstyle.parameters(cmesh, style);
        if(hasIn(cmesh, 'color')) {
            style_params = modifyList(style_params, list("color"=cmesh$color));
        }
        do.call(rgl::triangles3d, c(list(cmesh$voxeltris), style_params));
    } else if(is.Triangles3D(cmesh)) {
        if (requireNamespace("misc3d", quietly = TRUE)) {
            style_params = get.rglstyle.parameters(cmesh, style);
            style_params = modifyList(style_params, list("add"=TRUE)); # Add image to existing scene, otherwise only the last one will be visible.
            do.call(misc3d::drawScene.rgl, c(list(cmesh), style_params));
        } else {
            warning("The 'misc3d' package must be installed to render 'Triangles3D' instances. Skipping visualization."); # nocov
        }
    } else {
        stop(sprintf("Received object with classes '%s', cannot render this. Pass an 'fs.coloredmesh', 'fs.coloredvoxels', or 'Triangles3D' instance.\n", paste(class(cmesh), collapse=" ")));  # nocov
    }
}


#' @title Visualize a list of colored meshes in a single scene and rotate them, movie-style.
#'
#' @param coloredmeshes list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to rgl::bg3d()
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'rendner' set to FALSE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @param style a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
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
#' @param rgloptions option list passed to \code{\link{par3d}}. Example: rgloptions = list("windowRect"=c(50,50,1000,1000));
#'
#' @param rglactions named list. A list in which the names are from a set of pre-defined actions. Defaults to the empty list.
#'
#' @return the list of visualized coloredmeshes
#'
#' @export
#' @importFrom rgl open3d bg3d wire3d play3d spin3d
vis.coloredmeshes.rotating <- function(coloredmeshes, background="white", skip_all_na=TRUE, style="default", x=0, y=0, z=1, rpm=6, duration=10, rgloptions = rglo(), rglactions = list()) {

    if(!is.list(coloredmeshes)) {
        stop("Parameter coloredmeshes must be a list.");
    }

    rgl::open3d();
    do.call(rgl::par3d, rgloptions);
    Sys.sleep(1);
    rgl::bg3d(background);
    for(cmesh in coloredmeshes) {
        vis.renderable(cmesh, skip_all_na=TRUE, style=style);
    }
    rgl::rgl.viewpoint(-90, 0);

    if (!rgl::rgl.useNULL()) {
        if(rglactions.has.key(rglactions, 'movie')) {
            movie = rglactions$movie;
            rgl::movie3d(rgl::spin3d(axis = c(x, y, z), rpm = rpm), duration = duration, movie = movie, fps = 20, dir=path.expand("~"));
            expected_movie_path = file.path(path.expand("~"), sprintf("%s.gif", movie));
            message(sprintf("Tried to write gif movie to user home, check file '%s'.\n", expected_movie_path));
        } else {
            rgl::play3d(rgl::spin3d(axis = c(x, y, z), rpm = rpm), duration = duration);
        }
    } else {
        warning("Cannot show rotating scene with NULL device.");    # nocov
    }

    perform.rglactions(rglactions);
    invisible(coloredmeshes);
}



#' @title Rotate and visualize coloredmeshes, applying a style.
#'
#' @param coloredmeshes list of renderables. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param rotation_angle angle in radians. Passed to \code{\link{rotate3d}}.
#'
#' @param x x value passed to \code{\link{rotate3d}}.
#'
#' @param y y value passed to \code{\link{rotate3d}}.
#'
#' @param z z value passed to \code{\link{rotate3d}}.
#'
#' @param style a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style.
#'
#' @param draw_colorbar logical. Whether to draw a colorbar.
#'
#' @keywords internal
vis.rotated.coloredmeshes <- function(renderables, rotation_angle, x, y, z, style="default", draw_colorbar=FALSE) {
    if(is.null(renderables)) {
        return(invisible(NULL));
    }
    for (mesh_idx in seq_len(length(renderables))) {     # usually this will only run once for the single mesh of a hemisphere.
        orig_renderable = renderables[[mesh_idx]];
        do_vis = TRUE;
        if(is.fs.coloredmesh(orig_renderable)) {
            orig_mesh = orig_renderable$mesh;
            rotated_mesh = rgl::rotate3d(orig_mesh, rotation_angle, x, y, z);
            rotated_renderable = orig_renderable;         # copy coloredmesh
            rotated_renderable$mesh = rotated_mesh;  # replace inner mesh with rotated version
        } else if(is.fs.coloredvoxels(orig_renderable)) {
            colvox = orig_renderable;
            colvox$voxeltris = rgl::rotate3d(colvox$voxeltris, rotation_angle, x, y, z);
            rotated_renderable = colvox;
        } else if(is.Triangles3D(orig_renderable)) {
            tris3d = orig_renderable;
            tris3d$v1 = rgl::rotate3d(tris3d$v1, rotation_angle, x, y, z);
            tris3d$v2 = rgl::rotate3d(tris3d$v2, rotation_angle, x, y, z);
            tris3d$v3 = rgl::rotate3d(tris3d$v3, rotation_angle, x, y, z);
            rotated_renderable = tris3d;
        } else {
            warning(sprintf("Rotation not supported for object of type '%s'. Not rendering object.\n", paste(class(orig_renderable), collapse = " ")));
            do_vis = FALSE;
        }

        if(do_vis) {
            vis.renderable(rotated_renderable, style=style);
        }
    }

    if(draw_colorbar) {
        draw.colorbar(renderables);
    }
    return(invisible(NULL));
}




#' @title Draw a coloredmesh using a style.
#'
#' @param cmesh a coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param style a named list of style parameters or a string specifying an available style by name (e.g., 'shiny'). Defaults to 'default', the default style. Pass the magic word 'from_mesh' to try to retrieve a style (as a name or a style list) from the field `style` of the mesh, or default to "default" if the mesh has no such field.
#'
#' @seealso \code{\link[fsbrain]{vis.renderable}}
#'
#' @keywords internal
#' @importFrom rgl shade3d
vis.coloredmesh <- function(cmesh, style="default") {
    if(! is.fs.coloredmesh(cmesh)) {
        stop("Parameter cmesh must be an 'fs.coloredmesh' instance.");
    }
    style_params = get.rglstyle.parameters(cmesh, style);
    do.call(rgl::shade3d, c(list(cmesh$mesh, col=cmesh$col), style_params));
}


#' @title Produce the named list of style parameters from style definition.
#'
#' @description A style definition can be a character string like "shiny", already a parameter list, or a command like 'from_mesh' that tells us to get the style from the renderable. This function creates the final parameters from the definition and the renderable.
#'
#' @param renderable A renderable (or any list) which includes a 'style' key. If it does not include such a key, the 'default' style will be used.
#'
#' @param style A style definition. Can be a character string like 'shiny' or 'from_mesh', or already a named list of material properties (which will be returned as-is).
#'
#' @return a style, resolved to a parameter list compatible with \code{\link{material3d}}.
#'
#' @keywords internal
get.rglstyle.parameters <- function(renderable, style) {
    if(is.list(style)) {
        style_params = style;
    } else if (is.character(style)) {
        if(style == 'from_mesh') {
            if(!is.null(renderable$style)) {
                style = renderable$style;
            } else {
                style = 'default';
            }
        } else {
            style_params = get.rglstyle(style);
        }
    } else {
        stop("Parameter 'style' must be a named list of style parameters or a string specifying an available style by name (e.g., 'default' or 'shiny').");
    }
    return(style_params);
}


#' @title Get the default visualization style parameters as a named list.
#'
#' @description Run \code{\link{material3d}} without arguments to see valid style keywords to create new styles.
#'
#' @param style string. A style name. Available styles are one of: "default", "shiny", "semitransparent", "edges".
#'
#' @return a style, resolved to a parameter list compatible with \code{\link{material3d}}.
#'
#' @seealso \code{\link{shade3d}} can use the returned style
#'
#' @export
get.rglstyle <- function(style) {
    if(style == "default") {
        return(get.rglstyle.default());
    } else if (style == "shiny") {
        return(get.rglstyle.shiny());
    } else if (style == "edges") {
        return(get.rglstyle.edges());
    } else if (style == "semitransparent") {
        return(get.rglstyle.semitransparent());
    } else {
        stop(sprintf("No such rendering style: '%s'. Try something like 'default', 'shiny', 'edges' or 'semitransparent'.\n", style));
    }
}


#' @title Get the default visualization style parameters as a named list.
#'
#' @description The default rendering style, which is is rather plain. Does not look super fancy, but allows for clear data visualization without distractions. Hint: Run \code{\link{material3d}} without arguments to see valid style keywords to create new styles.
#'
#' @return named list, style parameters that can be passed to \code{\link{shade3d}} via \code{\link{do.call}}.
#'
#' @keywords internal
get.rglstyle.default <- function() {
    return(list("shininess"=50, specular="black"));
}


#' @title Get the semi-transparent visualization style parameters as a named list.
#'
#' @description Semitransparent rendering style. This style has a very negative impact on rendering performance. Hint: Run \code{\link{material3d}} without arguments to see valid style keywords to create new styles.
#'
#' @return named list, style parameters that can be passed to \code{\link{shade3d}} via \code{\link{do.call}}.
#'
#' @keywords internal
get.rglstyle.semitransparent <- function() {
    return(list("shininess"=50, specular="black", alpha=0.5, front="filled", back="lines"));
}


#' @title Get the mesh edges visualization style parameters as a named list.
#'
#' @description Mesh edges rendering style.
#'
#' @return named list, style parameters that can be passed to \code{\link{shade3d}} via \code{\link{do.call}}.
#'
#' @keywords internal
get.rglstyle.edges <- function() {
    return(list(front="lines", back="lines", lwd=2.0, size=5.0));
}



#' @title Get a shiny visualization style.
#'
#' @description A shiny or glossy rendering style. Looks a bit more modern, but the resulting highlights may make the interpretation of the plotted data a bit harder in some areas. Hint: Run \code{\link{material3d}} without arguments to see valid style keywords to create new styles.
#'
#' @return named list, style parameters that can be passed to to \code{\link{shade3d}} via \code{\link{do.call}}.
#'
#' @keywords internal
get.rglstyle.shiny <- function() {
    return(list("shininess"=50, specular="white"));
}


#' @title Sort coloredmeshes into 2 lists by their 'hemi' property.
#'
#' @param coloredmeshes list of coloredmeshes or other renderables
#'
#' @return named list with two entries: "lh": list of coloredmeshes that have property hemi set to 'lh'. "rh": list of coloredmeshes that have property hemi set to 'rh'. The rest is ignored.
#'
#' @keywords internal
sort.coloredmeshes.by.hemi <- function(coloredmeshes) {
    lh_meshes = list();
    rh_meshes = list();
    for (mesh_idx in seq_len(length(coloredmeshes))) {
        cmesh = coloredmeshes[[mesh_idx]];
        mesh_name = sprintf("mesh%d", mesh_idx);
        if(! ('hemi' %in% names(cmesh))) {
            if(is.fs.coloredmesh(cmesh)) {
                warning(sprintf("Assigning coloredmesh # %d which has no hemi value at all to both hemispheres.\n", mesh_idx));
            }
            lh_meshes[[mesh_name]] = cmesh;
            rh_meshes[[mesh_name]] = cmesh;
        } else {
            if(cmesh$hemi == 'lh') {
                lh_meshes[[mesh_name]] = cmesh;
            } else if(cmesh$hemi == 'rh') {
                rh_meshes[[mesh_name]] = cmesh;
            } else if(cmesh$hemi == 'both') {
                lh_meshes[[mesh_name]] = cmesh;
                rh_meshes[[mesh_name]] = cmesh;
            } else {
                warning(sprintf("Ignoring mesh # %d with invalid hemi value '%s'.\n", mesh_idx, cmesh$hemi));
            }
        }
    }
    return(list("lh"=lh_meshes, "rh"=rh_meshes));
}
