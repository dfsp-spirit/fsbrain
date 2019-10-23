
#' @title Visualize a list of colored meshes from several angles.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param background string, background color passed to rgl::bg3d()
#'
#' @param skip_all_na logical, whether to skip (i.e., not render) meshes in the list that have the property 'morph_data_was_all_na' set to TRUE. Defaults to TRUE. Practically, this means that a hemisphere for which the data was not given is not rendered, instead of being rendered in a single color.
#'
#' @keywords internal
#' @importFrom rgl open3d bg3d wire3d
vis.mult.coloredmeshes <- function(coloredmeshes, background="white", skip_all_na=TRUE) {

    if(!is.list(coloredmeshes)) {
        stop("Parameter coloredmeshes must be a list.");
    }
    num_sub_meshes = length(coloredmeshes); # The number of submeshes that each object consists of.

    layout_dim_x = 3;
    layout_dim_y = 2;
    num_views = layout_dim_x * layout_dim_y;

    rgl::open3d();
    rgl::bg3d(background);
    rgl::mfrow3d(layout_dim_x, layout_dim_y);
    for (view_idx in 1:num_views) {
        rgl::next3d();
        for (mesh_idx in 1:num_sub_meshes) {
            rgl::shade3d(coloredmeshes[[mesh_idx]]$mesh, col = coloredmeshes[[mesh_idx]]$col);
        }
    }

    rgl::highlevel(integer()); # To trigger display as rglwidget
}
