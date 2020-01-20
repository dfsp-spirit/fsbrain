# Volume visualization in 3D, based on RGL.
# These functions work by isosurfaces or rendering voxels as boxes.

#' @title Voxel-based visualization of volume mask at surface RAS positions.
#'
#' @description Plots a 3D box at every *foreground* voxel in the given volume. All voxels which do not have their intensity value set to `NA` are considered *foreground* voxels. The locations at which to plot the voxels is computed from the voxel CRS indices using the FreeSurfer \code{\link[fsbrain]{vox2ras_tkr}} matrix. This means that the position of the rendered data fits to the surface coordinates (in files like `surf/lh.white`), and that you can call this function while an active surface rendering window is open (e.g., from calling \code{\link[fsbrain]{vis.subject.morph.native}}), to superimpose the surface and volume data. **On coloring the voxels** (using *rgl materials*): Note that you can call this function several times for the active plot, and color the voxels differently by passing different material properties in each call. Alternatively, check the `voxelcol` parameter.
#'
#' @param volume numeric 3d array, voxels which should not be plotted must have value `NA`. Take care not to plot too many.
#'
#' @param render_every integer, how many to skip before rendering the next one (to improve performance and/or see deeper structures). Use higher values to see a less dense representation of your data that usually still allows you to see the general shape, but at lower computational burden. Set to 1 to render every (foreground) voxel.
#'
#' @param voxelcol character string or a *voxel coloring*. A *voxel coloring* can be specified in three ways: 1) the string 'from_intensities' will compute colors based on the intensity values of the foreground voxels in the volume, applying normalization of the intensity values if needed. 2) an array of RGB color strings: will be used to retrieve the colors for all foreground vertices, at their CRS indices. 3) A vector with length identical to the number of foreground voxels in the volume: will be applied directly.  Obvisouly, you should not pass a color material parameter (see `...`) when using this.
#'
#' @param ... material properties, passed to \code{\link[rgl]{triangles3d}}. Example: \code{color = "#0000ff", lit=FALSE}.
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    brain = subject.volume(subjects_dir, 'subject1', 'brain');
#'    # Plot all voxels of the brain:
#'    brain[which(brain==0, arr.ind = TRUE)] = NA;  # mark background
#'    volvis.voxels(brain);
#' }
#'
#' @export
volvis.voxels <- function(volume, render_every=8, voxelcol=NULL, ...) {
    num_volume_voxels = prod(dim(volume));

    voxel_crs = which(!is.na(volume), arr.ind = TRUE);   # foreground voxels, as CRS indices.
    num_foreground_voxels = nrow(voxel_crs);


    rendered_voxels = seq(1, num_foreground_voxels, render_every);  # Each number represents a voxel, encoded as the row index in 'voxel_crs'.
    num_rendered_voxels = length(rendered_voxels);

    # Create voxel colors as a vector of color strings that has length equal to 'num_foreground_voxels':
    if(!is.null(voxelcol)) {
        if(length(voxelcol) == 1) {
            if(voxelcol == 'from_intensity') {
                voxelcol = vol.intensity.to.color(volume[voxel_crs], scale='normalize_if_needed');
            } else {
                stop("If parameter 'voxelcol' has length 1, the only allowed value is the character string 'from_intensity'.");
            }
        } else {
            if(is.array(voxelcol) & is.character(voxelcol) & all.equal(dim(volume), dim(voxelcol))) {
                voxelcol = voxelcol[voxel_crs];
            } else if(is.vector(voxelcol) & is.character(voxelcol) & length(voxelcol) == num_foreground_voxels) {
                # do nothing, it was already passed in exactly as required: one color per foreground voxel.
            }
            else {
                stop(sprintf("If not NULL, parameter 'voxelcol' must be the character string 'from_intensity' or an array representing rgb colors with dimensions identical to those of 'volume', or a color vector with length identical to the number of foreground voxels (%d).\n", num_foreground_voxels));
            }
        }

        if(length(voxelcol) != num_foreground_voxels) {
            stop(sprintf("Bug: Voxel color mismatch. Computed %d voxel colors for %d foreground voxels.\n", length(voxelcol), num_foreground_voxels));
        }

        # Filter the colors by the voxels which will actually be rendered:
        if(render_every != 1) {
            voxelcol = voxelcol[rendered_voxels];
        }

        if(length(voxelcol) != num_rendered_voxels) {
            stop(sprintf("Bug: Voxel color mismatch. Computed %d voxel colors for %d rendered voxels.\n", length(voxelcol), num_rendered_voxels));
        }
    }



    if(render_every == 8) {
        message(sprintf("About to render %d voxels (one in %d voxels only). Set parameter 'render_every' to 1 to render all %d voxels.\n", length(rendered_voxels), render_every, nrow(voxel_crs)));
    }


    if(num_foreground_voxels > 0) {
        voxel_crs = cbind(voxel_crs, 1); # turn coords into homogeneous repr.
        surface_ras = matrix(rep(0, length(rendered_voxels)*3), ncol=3);
        vox2surface_ras_matrix = vox2ras_tkr();
        for(idx in seq(length(rendered_voxels))) {
            row_idx = rendered_voxels[idx];
            surface_ras[idx,] = (vox2surface_ras_matrix %*% voxel_crs[row_idx,])[1:3];
        }
        #rgl::rgl.spheres(surface_ras, r = 0.5, ...);
        rglvoxels(surface_ras, r = 1.0, voxelcol=voxelcol, ...);
    } else {
        warning("No foreground voxels in volume, nothing to visualize.");
    }
}


#' Apply matmult transformation to input.
#'
#' @description Apply affine transformation, like a vox2ras_tkr transformation, to input. This is just matrix multiplication for different input objects.
#'
#' @param m numerical vector/matrix or Triangles3D instance
#'
#' @param matrix_fun a 4x4 affine matrix or a function returning auch a matrix
#'
#' @return the input after application of the affine matrix (matrix multiplication)
#'
#' @export
apply.transform <- function(m, matrix_fun=fsbrain::vox2ras_tkr) {
    if(is.function(matrix_fun)) {
        affine_matrix = matrix_fun();
    } else if (is.matrix(matrix_fun)) {
        affine_matrix = matrix_fun;
    } else {
        stop("Parameter 'matrix_fun' must be a function or a matrix.");
    }

    if(is.vector(m)) {
        if(length(m) == 3) {
            m = c(m, 1L);
        }
        return((affine_matrix %*% m)[1:3]);
    }
    else if(is.matrix(m)) {
        surface_ras = matrix(rep(0, nrow(m)*3), ncol=3);
        m_cp = cbind(m, 1); # turn coords into homogeneous repr.
        for(idx in seq(nrow(m))) {
            surface_ras[idx,] = (affine_matrix %*% m_cp[idx,])[1:3];
        }
        return(surface_ras);
    } else if(class(m) == 'Triangles3D') {
        m$v1 = apply.transform(m$v1);   # v1 is a matrix
        m$v2 = apply.transform(m$v2);
        m$v3 = apply.transform(m$v3);
        return(m);
    } else {
        stop("Input type of parameter 'm' not supported. Must be numerical vector/matrix or Triangles3D.");
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
#' @param voxelcol vector of rgb color strings for the individual voxels. Its length must be identical to \code{nrow(centers)} if given.
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
rglvoxels <- function(centers, r=1.0, voxelcol=NULL, ...) {
    if(is.null(voxelcol)) {
        rgl::triangles3d(cubes3D.tris(centers, edge_length = r), ...);
    } else {
        if(length(voxelcol) != nrow(centers)) {
            stop(sprintf("Mismatch between voxel centers (%d rows) and voxel colors (length %d), sizes must match.\n", nrow(centers), length(voxelcol)));
        }
        for(rgbcol in unique(voxelcol)) {
            voxel_indices_this_color = which(voxelcol==rgbcol);
            cat(sprintf("Rendering %d voxels with color '%s'.\n", length(voxel_indices_this_color), rgbcol));
            rgl::triangles3d(cubes3D.tris(centers[voxel_indices_this_color,], edge_length = r), color = rgbcol, ...);
        }
    }
}


