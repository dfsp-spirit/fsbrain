# Volume visualization in 3D, based on RGL.
# These functions work by isosurfaces or rendering voxels as boxes.

#' @title Voxel-based visualization of volume mask.
#'
#' @param volume numeric 3d array, voxels which should not be plotted must have value `NA`. Take care not to plot too many.
#'
#' @param max_render integer, the maximal number of voxels to render. If there are more voxels in the volume which are not `NA`, a warning will be issued and the rest will not be rendered. Set to `prod(dim(volume))` to allow to render all, but be aware that this may take ages.
#'
#' @param render_every integer, how many to skip before rendering the next one. Set to 1 to render an object at every voxel.
#'
#' @export
volvis.voxels <- function(volume, max_render=25000, render_every=8) {
    voxel_crs = which(!is.na(volume), arr.ind = TRUE);

    rendered_voxels = seq(1, nrow(voxel_crs), render_every);

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
        rgl::rgl.spheres(surface_ras, r = 0.5, color = "#0000ff", lit=FALSE);   # boxes would be nice, but there seems to be no rgl function for them.
    } else {
        warning("No voxels to be visualized.");
    }
}


