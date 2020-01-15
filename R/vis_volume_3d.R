# Volume visualization in 3D, based on RGL.
# These functions work by isosurfaces or rendering voxels as boxes.

#' @title Voxel-based visualization of volume mask.
#'
#' @param volume numeric 3d array, voxels which should not be plotted must have value `NA`. Take care not to plot too many.
#'
#' @param max_render integer, the maximal number of voxels to render. If there are more voxels in the volume which are not `NA`, a warning will be issued and the rest will not be rendered. Set to `prod(dim(volume))` to allow to render all, but be aware that this may take ages.
#'
#' @param render_every integer, how many to skip before rendering the next one. Use higher values to see a less dense representation of your data that still allows one to see the general shape, but at lower computational burden. Set to 1 to render an object at every voxel.
#'
#' @param ... material properties, passed to \code{\link[rgl]{triangles3d}}. Example: \code{color = "#0000ff", lit=FALSE}.
#'
#' @export
volvis.voxels <- function(volume, max_render=500000, render_every=8, ...) {
    voxel_crs = which(!is.na(volume), arr.ind = TRUE);

    rendered_voxels = seq(1, nrow(voxel_crs), render_every);

    if(render_every == 8) {
        message(sprintf("About to render %d voxels (one in %d voxels only). Set parameter 'render_every' to 1 to render all %d voxels.\n", length(rendered_voxels), render_every, nrow(voxel_crs)));
    }

    if(length(rendered_voxels) > max_render) {
        warning(sprintf("About to render %d voxels, but max_render is set to %d. Will stop early, the rest will not appear in the output. Try increasing one of the parameters 'max_render' or 'render_every'.\n", length(rendered_voxels), max_render));
        rendered_voxels = rendered_voxels[1:max_render];
    }

    if(nrow(voxel_crs) > 0) {
        voxel_crs = cbind(voxel_crs, 1);
        surface_ras = matrix(rep(0, length(rendered_voxels)*3), ncol=3);
        vox2surface_ras_matrix = vox2ras_tkr();
        for(idx in seq(length(rendered_voxels))) {
            row_idx = rendered_voxels[idx];
            surface_ras[idx,] = (vox2surface_ras_matrix %*% voxel_crs[row_idx,])[1:3];
        }
        #rgl::rgl.spheres(surface_ras, r = 0.5, ...);
        rglvoxels(surface_ras, r = 1.0, ...);
    } else {
        warning("No voxels to be visualized.");
    }
}


#' @title Draw 3D boxes at locations using rgl.
#'
#' @description Draw 3D boxes at all given coordinates using rgl, analogous to \code{\link[rgl]{rgl.spheres}}. Constructs the coordinates for triangles making up the boxes, then uses \code{\link[rgl]{triangles3d}} to render them.
#'
#' @param centers numerical matrix with 3 columns. Each column represents the x, y, z coordinates of a center at which to create a cube.
#'
#' @param r numerical vector or scalar, the edge length. The vector must have length 1 (same edge length for all cubes), or the length must be identical to the number of rows in parameter `centers`.
#'
#' @param ... material properties, passed to \code{\link[rgl]{triangles3d}}. Example: \code{color = "#0000ff", lit=FALSE}.
#'
#'
#' @examples
#'    # Plot a 3D cloud of 20000 red voxels:
#'    centers = matrix(rnorm(20000*3)*100, ncol=3);
#'    rglvoxels(centers, color="red");
#'
#' @export
rglvoxels <- function(centers, r=1.0, ...) {
    rgl::triangles3d(cubes3D.tris(centers, edge_length = r), ...);
}


