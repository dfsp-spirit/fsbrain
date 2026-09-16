# Volume visualization in 3D, based on RGL.
# These functions work by isosurfaces or rendering voxels as boxes.

#' @title Voxel-based visualization of volume mask at surface RAS positions.
#'
#' @description Plots a 3D box at every *foreground* voxel in the given volume. All voxels which do not have their intensity value set to `NA` are considered *foreground* voxels. The locations at which to plot the voxels is computed from the 1-based R array indices of the voxels using the \code{\link[fsbrain]{index2ras_tkr}} matrix (see \code{\link[fsbrain]{vox2ras_tkr}} for the CRS-based variant). This means that the position of the rendered data fits to the surface coordinates (in files like `surf/lh.white`), and that you can call this function while an active surface rendering window is open (e.g., from calling \code{\link[fsbrain]{vis.subject.morph.native}}), to superimpose the surface and volume data. **On coloring the voxels** (using *rgl materials*): Note that you can call this function several times for the active plot, and color the voxels differently by passing different material properties in each call. Alternatively, check the `voxelcol` parameter.
#'
#' @param volume numeric 3d array, voxels which should not be plotted must have value `NA`. Take care not to plot too many.
#'
#' @param render_every integer, how many to skip before rendering the next one (to improve performance and/or see deeper structures). Use higher values to see a less dense representation of your data that usually still allows you to see the general shape, but at lower computational burden. Set to 1 to render every (foreground) voxel.
#'
#' @param voxelcol character string or a *voxel coloring*. A *voxel coloring* can be specified in three ways: 1) the string 'from_intensity' will compute colors based on the intensity values of the foreground voxels in the volume, applying normalization of the intensity values if needed. 2) an array of RGB color strings: will be used to retrieve the colors for all foreground vertices, at their CRS indices. 3) A vector with length identical to the number of foreground voxels in the volume: will be applied directly.  Obvisouly, you should not pass a color material parameter (see `...`) when using this.
#'
#' @param ... material properties, passed to \code{\link[rgl]{triangles3d}}. Example: \code{color = "#0000ff", lit=FALSE}.
#'
#' @examples
#' \dontrun{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    brain = subject.volume(subjects_dir, 'subject1', 'brain');
#'    # Plot all voxels of the brain:
#'    brain[which(brain==0L, arr.ind = TRUE)] = NA;  # mark background
#'    brain = vol.hull(brain); # remove inner triangles
#'    volvis.voxels(brain);
#' }
#'
#' @export
volvis.voxels <- function(volume, render_every=1, voxelcol=NULL, ...) {
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
                if(is.character(voxelcol)) {
                    voxelcol = rep(voxelcol, num_foreground_voxels);

                } else {
                    stop("If parameter 'voxelcol' has length 1, the only allowed value is the character string 'from_intensity'.");
                }
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
            stop(sprintf("Bug: Voxel color mismatch. Computed %d voxel colors for %d foreground voxels.\n", length(voxelcol), num_foreground_voxels)); # nocov
        }

        # Filter the colors by the voxels which will actually be rendered:
        if(render_every != 1) {
            voxelcol = voxelcol[rendered_voxels];
        }

        if(length(voxelcol) != num_rendered_voxels) {
            stop(sprintf("Bug: Voxel color mismatch. Computed %d voxel colors for %d rendered voxels.\n", length(voxelcol), num_rendered_voxels)); # nocov
        }
    }

    if(num_foreground_voxels > 0) {
        voxel_crs = cbind(voxel_crs, 1); # turn coords into homogeneous repr.
        surface_ras = matrix(rep(0, length(rendered_voxels)*3), ncol=3);
        # The indices from 'which(..., arr.ind = TRUE)' are 1-based R array indices, so the
        # 'index2ras_tkr' matrix is used instead of the 'vox2ras_tkr' one (which expects CRS).
        vox2surface_ras_matrix = index2ras_tkr();
        for(idx in seq(length(rendered_voxels))) {
            row_idx = rendered_voxels[idx];
            surface_ras[idx,] = (vox2surface_ras_matrix %*% voxel_crs[row_idx,])[1:3];
        }
        #rgl::spheres3d(surface_ras, r = 0.5, ...);
        return(invisible(rglvoxels(surface_ras, r = 1.0, voxelcol=voxelcol, ...)));
    } else {
        warning("No foreground voxels in volume, nothing to visualize.");
        return(invisible(NULL));
    }
}


#' @title Retain only the outer hull voxels of the foreground.
#'
#' @description Filters the *foreground* voxel in the volume by keeping only an outer border of voxels, and setting the inner core voxels to `NA`. This is a utility function for voxel-based visualization. The goal is to remove the inner voxels, which will not be visible anyways, and thus to dramatically reduce the number of triangles that will need to be computed for the mesh.
#'
#' @param volume numeric 3d array, must contain foreground voxel and background voxels. The latter must have value `NA`. This function assumes that a solid foreground object surrounded by background exists in the volume.
#'
#' @param thickness integer, the width of the border in voxels, i.e., how many of the voxels in each upright column to keep at the top and at the bottom.
#'
#' @param axes integer vector, the axes to use. Valid values in the vector are 1L, 2L and 3L. You will have to use all 3 axes if you do not want any holes in the object. (Obvisouly, having noise around the object can still lead to holes.)
#'
#' @return numeric 3d array, a filtered version of the input. It contains at least as many `NA` voxels as the input. If the function had any effect, it contains a lot more `NA` values. The other values and the volume dimensions are left unchanged.
#' @export
vol.hull <- function(volume, thickness=1L, axes=c(2L)) {
    vd = dim(volume);
    if(length(vd) != 3L) {
        stop("Volume must have exactly 3 dimensions.");
    }

    if(length(axes) > 3L) {
        stop("Length of vector in parameter 'axes' must not exceed 3.");
    }
    axes = as.integer(axes);

    hull = array(rep(NA, prod(vd)), vd);

    for(axis in axes) {
        hull = hull.retain.along.axis(volume, hull, dim_check = axis, upwards = TRUE, thickness = thickness);
        hull = hull.retain.along.axis(volume, hull, dim_check = axis, upwards = FALSE, thickness = thickness);
    }

    return(hull);
}


#' @title Copy the first *n* foreground voxel values.
#'
#' @description Copy the first *n* foreground voxel values along the axis and direction from the volume to the hull, thus adding foreground voxels to the hull.
#'
#' @param volume numeric 3d array, the full source volume.
#'
#' @param hull numeric 3d array, the input hull volume.
#'
#' @param dim_check integer, the array dimension to use. Must be 1L, 2L or 3L.
#'
#' @param upwards logical, whether to use upwards direction (increasing indices) in the array dimension
#'
#' @param thickness integer, the width of the border in voxels, i.e., how many of the foreground voxels to keep
#'
#' @return numeric 3d array, the updated hull volume.
#'
#' @keywords internal
hull.retain.along.axis <- function(volume, hull, dim_check=2L, upwards=TRUE, thickness=1L) {
    vd = dim(volume);
    row_length = vd[dim_check];
    start_idx = ifelse(upwards, 1L, row_length);
    end_idx = ifelse(upwards, row_length, 1L);

    if(dim_check == 2L) {
        for(v_c in seq_len(vd[1])) {
            for(v_s in seq_len(vd[3])) {
                num_retained_this_row = 0L;
                for(v_r in seq.int(start_idx, end_idx)) {
                    if(num_retained_this_row >= thickness) {
                        break;
                    }
                    voxel_value = volume[v_c, v_r, v_s];
                    if(!is.na(voxel_value)) {
                        hull[v_c, v_r, v_s] = voxel_value;
                        num_retained_this_row = num_retained_this_row + 1L;
                    }
                }
            }
        }

    } else if(dim_check == 1L) {
        for(v_c in seq_len(vd[2])) {
            for(v_s in seq_len(vd[3])) {
                num_retained_this_row = 0L;
                for(v_r in seq.int(start_idx, end_idx)) {
                    if(num_retained_this_row >= thickness) {
                        break;
                    }
                    voxel_value = volume[v_r, v_c, v_s];
                    if(!is.na(voxel_value)) {
                        hull[v_r, v_c, v_s] = voxel_value;
                        num_retained_this_row = num_retained_this_row + 1L;
                    }
                }
            }
        }
    } else if(dim_check == 3L) {
        for(v_c in seq_len(vd[1])) {
            for(v_s in seq_len(vd[2])) {
                num_retained_this_row = 0L;
                for(v_r in seq.int(start_idx, end_idx)) {
                    if(num_retained_this_row >= thickness) {
                        break;
                    }
                    voxel_value = volume[v_c, v_s, v_r];
                    if(!is.na(voxel_value)) {
                        hull[v_c, v_s, v_r] = voxel_value;
                        num_retained_this_row = num_retained_this_row + 1L;
                    }
                }
            }
        }
    } else {
        stop("Invalid 'dim_check' parameter.");
    }
    return(hull);
}


#' @title Visualize contour of a volume.
#'
#' @description Compute a smoothed surface from the voxel intensities in the given volume and render it. Requires the `misc3d` package to be installed, which is an optional dependency.
#'
#' @param volume a 3D brain volume
#'
#' @param level numeric, intensity threshold for the data. Voxels with intensity value smaller than `level` will be ignored when creating the contour surface.
#'
#' @param show logical, whether to display the triangles. Defaults to `TRUE`.
#'
#' @param frame integer, the frame to show in case of a 4D input volume. Can also be the character string 'all' to draw the contents of all frames at once. Useful to plot white matter tracts from DTI data, where each tract is stored in a different frame.
#'
#' @param color the color to use when plotting. Can be a vector of colors when plotting all frames of a 4D image (one color per frame).
#'
#' @return the rendered triangles (a `Triangles3D` instance) with coordinates in surface RAS space if any, `NULL` otherwise. This will be a list if you pass a 4D volume and select 'all' frames.
#'
#' @examples
#' \dontrun{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    brain = subject.volume(subjects_dir, 'subject1', 'brain');
#'    # Plot all voxels of the brain:
#'    volvis.contour(brain);
#' }
#'
#' @export
volvis.contour <- function(volume, level=80, show=TRUE, frame=1L, color='white') {
    if (requireNamespace("misc3d", quietly = TRUE)) {

        if(freesurferformats::is.fs.volume(volume)) {
            volume = volume$data;
        }

        ndim = length(dim(volume));
        if(ndim == 4L) {
            if(frame == "all") {
                num_frames = dim(volume)[4];

                if(length(color) == 1L) {
                    color = rep(color, num_frames);
                }
                if(length(color) != num_frames) {
                    stop("Length of color parameter must be 1 or exactly the number of frames (4th dim) in the image.");
                }

                all_tris = list();
                for(frame_index in seq.int(num_frames)) {
                    all_tris[[frame_index]] = misc3d::contour3d(volume[,,,frame_index], level=level, draw=FALSE);
                    all_tris[[frame_index]]$color = color[[frame_index]];
                }
                names(all_tris) = NULL;

                if(show) {
                    vis.coloredmeshes(all_tris);
                }
                return(invisible(all_tris));
            } else {
                volume = volume[,,,frame]; # select requested single frame
                surface_tris = misc3d::contour3d(volume, level=level, draw=FALSE);
            }
        } else if(ndim == 3L) {
            surface_tris = misc3d::contour3d(volume, level=level, draw=FALSE);
        } else {
            stop("Input volume must have 3 or 4 dimensions.");
        }

        if(length(color) != 1L) {
            stop("Color must have length 1 for a 3D volume.");
        }
        surface_tris$color = color;
        if(show) {
            vis.coloredmeshes(list(surface_tris));
        }
        return(invisible(surface_tris));
    } else {
        warning("The 'misc3d' package must be installed to use this functionality.");
        return(invisible(NULL));
    }
}


#' Apply affine transformation to input.
#'
#' @description Apply an affine transformation, like a *vox2ras_tkr* transformation, to input. This is just matrix multiplication for different input objects. Supported input types are coordinate vectors, coordinate matrices, `fs.surface` meshes (the vertex coordinates are transformed, the face indices stay the same), renderable objects like `fs.coloredmesh`, `fs.coloredvoxels`, `fs.coloredpaths` or *misc3d* `Triangles3D`, rgl `mesh3d`/`tmesh3d` instances (including their normals, if any), and (hemi-)lists of such objects (which are transformed element-wise).
#'
#' @param object numerical vector/matrix, `fs.surface`, `fs.coloredmesh`, `fs.coloredvoxels`, `fs.coloredpaths`, `Triangles3D`, `mesh3d`/`tmesh3d` instance, or a list (e.g., a hemilist) of such objects, the coordinates or objects to transform.
#'
#' @param matrix_fun a 4x4 affine matrix or a function returning such a matrix. If `NULL`, the input is returned as-is. In many cases you way want to use a matrix computed from the header of a volume file, e.g., the `vox2ras` matrix of the respective volume. See the `mghheader.*` functions in the *freesurferformats* package to obtain these matrices. Registration files can be read with `freesurferformats::read.fs.transform` and friends, but note that such files often describe a *voxel* to *surface RAS* mapping, so you may have to compose them with a `vox2ras` matrix to get a transformation between RAS coordinates.
#'
#' @return the input after application of the affine matrix (matrix multiplication)
#'
#' @note The affine matrix is applied in the standard way: the coordinates are interpreted as homogeneous *column* vectors, i.e., a vertex `v` is transformed as `v' = M %*% v`. Note that rgl, and fsbrain functions that are implemented on top of rgl (like the camera transforms used internally for views), use the transposed convention for their rotation matrices, see \code{\link[rgl]{rotationMatrix}}. For pure translations and scalings, both conventions are identical.
#'
#'   Meshes keep their orientation: if the linear part of the matrix has a negative determinant, the transformation mirrors the object (this is the case for the FreeSurfer `vox2ras_tkr` matrix, which flips and permutes axes), which would invert all surface normals and make the mesh render inside-out. In that case, the vertex order within each face is reversed to preserve the original orientation, and the stored normals (if any) are transformed with the linear part of the matrix so that they stay consistent with the faces.
#'
#' @examples
#' \dontrun{
#'    # Transform the vertex coordinates of a surface mesh:
#'    cube = freesurferformats::read.fs.surface(system.file("extdata", "cube.ply", package = "fsbrain"));
#'    translation = matrix(c(1,0,0,10, 0,1,0,20, 0,0,1,30, 0,0,0,1), nrow = 4L, byrow = TRUE);
#'    cube_moved = apply.transform(cube, translation);
#' }
#'
#' @export
apply.transform <- function(object, matrix_fun) {
    m = object;
    if(is.null(matrix_fun)) {
        return(m);
    }
    if(is.function(matrix_fun)) {
        affine_matrix = matrix_fun();
    } else if (is.matrix(matrix_fun)) {
        affine_matrix = matrix_fun;
    } else {
        stop("Parameter 'matrix_fun' must be a function or a matrix.");
    }

    if(! is.matrix(affine_matrix) || nrow(affine_matrix) != 4L || ncol(affine_matrix) != 4L) {
        stop(sprintf("Parameter 'matrix_fun' must be (or return) a 4x4 affine matrix, but the result is not: it has %d row(s) and %d column(s).\n", nrow(affine_matrix), ncol(affine_matrix)));
    }

    return(apply.transform.matrix(m, affine_matrix));
}


#' @title Apply an affine transformation matrix to an object.
#'
#' @description Internal workhorse of \code{\link[fsbrain]{apply.transform}}, assumes that the matrix has already been resolved from the 'matrix_fun' parameter.
#'
#' @param object the object to transform.
#'
#' @param affine_matrix a 4x4 affine matrix.
#'
#' @return the transformed object.
#'
#' @keywords internal
apply.transform.matrix <- function(object, affine_matrix) {

    # If the linear part of the matrix has a negative determinant, the transformation mirrors the
    # object, i.e., it turns a right-handed into a left-handed coordinate system. This happens for
    # the FreeSurfer 'vox2ras_tkr' matrix (which flips and permutes axes), so it is the common case
    # when transforming data from voxel space to surface RAS. Mirroring reverses the winding of
    # every face, which makes all geometric normals point inwards: the mesh is rendered inside-out
    # (dark, with wrong lighting), so we restore the original orientation after the transformation.
    flips_orientation = det(affine_matrix[1:3, 1:3, drop = FALSE]) < 0;

    # Vertex order which restores the original winding of a face: swap the 2nd and 3rd vertex and
    # keep the remaining ones (quads) in place.
    winding_restore_order = function(num_vertices) {
        order = c(1L, 3L, 2L);
        if(num_vertices > 3L) {
            order = c(order, seq.int(4L, num_vertices));
        }
        return(order);
    };

    if(freesurferformats::is.fs.surface(object)) {
        # Transform the vertex coordinates. The face indices themselves are unaffected, only their
        # order within each face may change (see below).
        object$vertices = apply.affine.to.coords(object$vertices, affine_matrix);
        if(flips_orientation && ncol(object$faces) >= 3L) {
            # Restore the original winding of the faces.
            object$faces = object$faces[, winding_restore_order(ncol(object$faces)), drop = FALSE];
        }
        return(object);
    }

    if(is.fs.coloredmesh(object)) {
        object$mesh = apply.transform.matrix(object$mesh, affine_matrix);
        # A coloredmesh stores the mesh it was created from in its metadata, keep it in sync.
        if(freesurferformats::is.fs.surface(object$metadata$fs_mesh)) {
            object$metadata$fs_mesh = apply.transform.matrix(object$metadata$fs_mesh, affine_matrix);
        }
        return(object);
    }

    if(is.fs.coloredvoxels(object)) {
        object$voxeltris = apply.transform.matrix(object$voxeltris, affine_matrix);
        return(object);
    }

    if(is.fs.coloredpaths(object)) {
        # Line segments: transform both endpoints. The winding of faces is irrelevant here.
        object$from = apply.affine.to.coords(object$from, affine_matrix);
        object$to = apply.affine.to.coords(object$to, affine_matrix);
        return(object);
    }

    if(is.Triangles3D(object)) {
        object$v1 = apply.affine.to.coords(object$v1, affine_matrix);
        object$v2 = apply.affine.to.coords(object$v2, affine_matrix);
        object$v3 = apply.affine.to.coords(object$v3, affine_matrix);
        if(flips_orientation) {
            # Swap the second and third vertex of every triangle to restore the original winding.
            tmp_v = object$v2;
            object$v2 = object$v3;
            object$v3 = tmp_v;
        }
        return(object);
    }

    if(inherits(object, "mesh3d") || (is.list(object) && ! is.null(object$vb))) {
        object$vb[1:3, ] = t(apply.affine.to.coords(t(object$vb[1:3, , drop = FALSE]), affine_matrix));
        if(! is.null(object$normals) && nrow(object$normals) >= 3L) {
            # Normals are transformed with the linear part of the matrix (translation does not apply).
            # Note that they must NOT be negated: the winding of the faces is restored above, so
            # the transformed normals stay consistent with the (outward) faces, see the tests.
            object$normals[1:3, ] = affine_matrix[1:3, 1:3, drop = FALSE] %*% object$normals[1:3, , drop = FALSE];
        }
        if(flips_orientation) {
            # Same as above: restore the winding of the faces, in 'it' (triangles) or 'ib' (quads).
            for(faces_key in c('it', 'ib')) {
                if(! is.null(object[[faces_key]]) && nrow(object[[faces_key]]) >= 3L) {
                    object[[faces_key]] = object[[faces_key]][winding_restore_order(nrow(object[[faces_key]])), , drop = FALSE];
                }
            }
        }
        return(object);
    }

    if(is.matrix(object)) {
        if(ncol(object) == 3L) {   # Nx3 vertex coordinates
            return(apply.affine.to.coords(object, affine_matrix));
        } else if(ncol(object) == 4L) {   # Nx4 coordinates in homogeneous representation
            return(t((affine_matrix %*% t(object)))[, 1:3, drop = FALSE]);
        } else {
            stop(sprintf("Matrix input must have 3 (x,y,z) or 4 (homogeneous x,y,z,w) columns, but has %d.\n", ncol(object)));
        }
    }

    if(is.numeric(object) && ! is.matrix(object)) {
        coords = object;
        if(length(coords) == 3) {
            coords = c(coords, 1L);
        }
        return((affine_matrix %*% coords)[1:3]);
    }

    if(is.list(object) && length(object) > 0L) {
        # A (e.g., hemi-)list of objects: transform all elements, provided that they are supported.
        supported = vapply(object, function(el) {
            freesurferformats::is.fs.surface(el) || is.fs.coloredmesh(el) || is.fs.coloredvoxels(el) || is.fs.coloredpaths(el) || is.Triangles3D(el) || inherits(el, "mesh3d") || is.matrix(el) || is.list(el);
        }, logical(1L));
        if(all(supported)) {
            return(lapply(object, apply.transform.matrix, affine_matrix = affine_matrix));
        }
    }

    stop("Input type of parameter 'object' not supported. Must be a numerical vector/matrix, an fs.surface, an fs.coloredmesh, fs.coloredvoxels, fs.coloredpaths, Triangles3D or mesh3d instance, or a list of such objects.");
}


#' @title Apply a 4x4 affine matrix to vertex coordinates.
#'
#' @description Internal helper, applies the matrix to homogeneous column vectors, i.e., `v' = M %*% v`.
#'
#' @param coords Nx3 matrix of vertex coordinates (or a vector of length 3).
#'
#' @param affine_matrix a 4x4 affine matrix.
#'
#' @return Nx3 matrix of transformed coordinates.
#'
#' @keywords internal
apply.affine.to.coords <- function(coords, affine_matrix) {
    if(! is.matrix(coords)) {
        coords = matrix(coords, ncol = 3L);
    }
    if(ncol(coords) != 3L) {
        stop(sprintf("Coordinate input must have 3 (x,y,z) columns, but has %d.\n", ncol(coords)));
    }
    if(nrow(coords) == 0L) {
        return(coords);
    }
    return((cbind(coords, 1) %*% t(affine_matrix))[, 1:3, drop = FALSE]);
}


#' @title Draw 3D boxes at locations using rgl.
#'
#' @description Draw 3D boxes at all given coordinates using rgl, analogous to \code{\link[rgl]{spheres3d}}. Constructs the coordinates for triangles making up the boxes, then uses \code{\link[rgl]{triangles3d}} to render them.
#'
#' @param centers numerical matrix with 3 columns. Each column represents the x, y, z coordinates of a center at which to create a cube.
#'
#' @param r numerical vector or scalar, the cube edge length. This is the length of the axis-parallel edges of the cube. The vector must have length 1 (same edge length for all cubes), or the length must be identical to the number of rows in parameter `centers`.
#'
#' @param voxelcol vector of rgb color strings for the individual voxels. Its length must be identical to \code{nrow(centers)} if given.
#'
#' @param ... material properties, passed to \code{\link[rgl]{triangles3d}}. Example: \code{color = "#0000ff", lit=FALSE}.
#'
#' @param do_show logical, whether to visualize the result in the current rgl scene
#'
#' @return list of `fs.coloredvoxels` instances, invisible. The function is called for the side effect of visualizing the data, and usually you can ignore the return value.
#'
#'
#' @examples
#' \dontrun{
#'    # Plot a 3D cloud of 500 red voxels:
#'    centers = matrix(rnorm(500*3)*100, ncol=3);
#'    rglvoxels(centers, voxelcol="red");
#' }
#' @export
rglvoxels <- function(centers, r=1.0, voxelcol=NULL, do_show = TRUE, ...) {
    coloredvoxels = list();
    if(is.null(voxelcol)) {
        coloredvox = list("voxeltris"=cubes3D.tris(centers, edge_length = r), "color"="#000000");
        class(coloredvox) = c("fs.coloredvoxels", class(coloredvox));
        rgl::triangles3d(coloredvox$voxeltris, color = coloredvox$color, ...);
        coloredvoxels = append(coloredvoxels, list(coloredvox));
    } else {
        if(is.character(voxelcol) & length(voxelcol) == 1) {
            voxelcol = rep(voxelcol, nrow(centers));
        }
        if(length(voxelcol) != nrow(centers)) {
            stop(sprintf("Mismatch between voxel centers (%d rows) and voxel colors (length %d), sizes must match.\n", nrow(centers), length(voxelcol)));
        }
        for(rgbcol in unique(voxelcol)) {
            voxel_indices_this_color = which(voxelcol==rgbcol);
            #message(sprintf("Rendering %d voxels with color '%s'.\n", length(voxel_indices_this_color), rgbcol));
            coloredvox = list("voxeltris"=cubes3D.tris(centers[voxel_indices_this_color,], edge_length = r), "color"=rgbcol);
            class(coloredvox) = c("fs.coloredvoxels", class(coloredvox));
            coloredvoxels = append(coloredvoxels, list(coloredvox));
            if(do_show) {
                rgl::triangles3d(coloredvox$voxeltris, color = coloredvox$color, ...);
            }
        }
    }
    return(invisible(coloredvoxels));
}


#' @title Check whether object is an fs.coloredvoxels instance (S3)
#'
#' @param x any `R` object
#'
#' @return TRUE if its argument is a fs.coloredvoxels instance (that is, has "fs.coloredvoxels" among its classes) and FALSE otherwise.
#'
#' @export
is.fs.coloredvoxels <- function(x) inherits(x, "fs.coloredvoxels")



#' @title Print description of fs.coloredvoxels (S3).
#'
#' @param x brain voxel tris with class `fs.coloredvoxels`.
#'
#' @param ... further arguments passed to or from other methods
#'
#' @export
print.fs.coloredvoxels <- function(x, ...) {
    cat(sprintf("Brain coloredvoxels with %d triangles.\n", nrow(x$voxeltris)/3L));     # nocov start
    if(is.null(x$color)) {
        cat(sprintf("No color information.\n"));
    } else {
        if(length(x$color == 1L)) {
            cat(sprintf("Voxel color is '%s'.\n", x$color));
        } else {
            cat(sprintf("Voxel color with %d entries.\n", length(x$color)));
        }
    }                                                                                   # nocov end
}


# ══════════════════════════════════════════════════════════════════════════════════════
# Combined surface + volume visualization
# ══════════════════════════════════════════════════════════════════════════════════════

#' @title Visualize a brain volume overlaid on a cortical surface in 3D.
#'
#' @description Render a brain volume (as an isosurface or as voxels) together with a cortical surface mesh colored by morphometry data in the same interactive 3D scene. The volume coordinates are transformed to surface RAS space using the FreeSurfer \code{\link[fsbrain]{vox2ras_tkr}} matrix (more precisely \code{\link[fsbrain]{index2ras_tkr}}, see there for the R-index versus CRS distinction), ensuring proper spatial alignment of volume and surface.
#'
#' @param subjects_dir character string, the FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id character string, the subject identifier.
#'
#' @param volume numeric 3D array or character string. Either a 3D volume array with voxel intensities, or the name of a volume file to load from the subject's `mri/` directory (e.g., \code{"brain"}, \code{"aseg"}, \code{"aparc+aseg"}). Background voxels should have value \code{NA} or 0.
#'
#' @param volume_mode character string, one of \code{"contour"} or \code{"voxels"}. The rendering mode for the volume: \code{"contour"} creates a smooth isosurface using the `misc3d` package (requires the optional dependency `misc3d`), \code{"voxels"} renders individual foreground voxels as small cubes. Defaults to \code{"contour"}.
#'
#' @param volume_level numeric scalar, the intensity threshold for contour mode. Only voxels with intensity values >= this threshold contribute to the isosurface. Ignored in voxel mode. Defaults to 80 (suitable for a raw brain.mgz with values in range 0-255).
#'
#' @param volume_color character string, the color for the volume rendering. Defaults to \code{"#666666"} (medium gray).
#'
#' @param volume_alpha numeric in range 0..1, the transparency (alpha) of the volume overlay. Lower values make the volume more transparent, revealing the surface beneath. Defaults to 0.3.
#'
#' @param measure character string or NULL, the morphometry data to use for coloring the surface. E.g., \code{"thickness"}, \code{"sulc"}, \code{"area"}, or \code{"curv"}. Pass \code{NULL} to render the surface in a single plain color without morphometry overlay. Defaults to \code{"thickness"}.
#'
#' @param surface character string, the display surface. E.g., \code{"white"}, \code{"pial"}, or \code{"inflated"}. Defaults to \code{"white"}.
#'
#' @param hemi character string, one of \code{'lh'}, \code{'rh'}, or \code{'both'}. The hemisphere to display. Defaults to \code{"both"}.
#'
#' @param views list of strings, the view configuration. For single interactive view use \code{c("si")}. For 4-angle tiled view use \code{c("t4")}. For 9-angle tiled view use \code{c("t9")}. Defaults to \code{c("t4")}.
#'
#' @param surface_style character string, a rendering style for the cortical surface, e.g., \code{'default'}, \code{'shiny'} or \code{'semitransparent'}. Use \code{'semitransparent'} to make the surface partially see-through, which works well for volume overlays. Defaults to \code{"semitransparent"}.
#'
#' @param rgloptions named list, parameters passed to \code{\link[rgl]{par3d}}. Example: \code{rgloptions = list("windowRect"=c(50,50,800,800))}. Defaults to the result of \code{\link[fsbrain]{rglo}}.
#'
#' @param makecmap_options named list of parameters to pass to \code{\link[squash]{makecmap}}. Should include at least a colormap function as name \code{'colFn'}. Defaults to the result of \code{\link[fsbrain]{mkco.seq}}.
#'
#' @param cortex_only logical, whether to mask the medial wall (i.e., only render cortical vertices). Defaults to \code{FALSE}.
#'
#' @param render_every integer, for voxel mode only: render every Nth foreground voxel. Higher values improve performance but reduce density. Set to 1 to render all voxels (may be slow). Defaults to 20.
#'
#' @param ... extra parameters passed to the volume rendering backend (e.g., \code{lit=FALSE} for unlit voxels).
#'
#' @return invisible named list with entries: \code{"surface"} (the coloredmeshes from the surface rendering, a hemilist of \code{coloredmesh} instances) and \code{"volume"} (the volume rendering result: a `Triangles3D` object for contour mode, or a list of \code{coloredvoxels} for voxel mode).
#'
#' @examples
#' \dontrun{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    # Contour overlay of brain volume on thickness-colored surface:
#'    vis.volume.on.surface(subjects_dir, 'subject1', 'brain',
#'       volume_mode='contour', volume_level=60, measure='thickness',
#'       views=c('si'), surface_style='semitransparent');
#'    # Voxel overlay of aseg segmentation ventricles:
#'    aseg = subject.volume(subjects_dir, 'subject1', 'aseg');
#'    ventricle_mask = vol.mask.from.segmentation(aseg, c(4,14,15,43));
#'    vis.volume.on.surface(subjects_dir, 'subject1', ventricle_mask,
#'       volume_mode='voxels', measure=NULL, views=c('si'),
#'       volume_color='red');
#' }
#'
#' @family visualization functions
#' @family volume visualization
#'
#' @export
vis.volume.on.surface <- function(subjects_dir, subject_id,
    volume = "brain", volume_mode = "contour", volume_level = 80,
    volume_color = "#666666", volume_alpha = 0.3,
    measure = "thickness", surface = "white",
    hemi = "both", views = c("t4"),
    surface_style = "semitransparent",
    rgloptions = rglo(),
    makecmap_options = mkco.seq(),
    cortex_only = FALSE,
    render_every = 20L,
    ...
) {
    # ── Validate parameters ──────────────────────────────────────────────────
    if(!(volume_mode %in% c("contour", "voxels"))) {
        stop("Parameter 'volume_mode' must be one of 'contour' or 'voxels'.");
    }

    if(volume_mode == "contour") {
        if (!requireNamespace("misc3d", quietly = TRUE)) {
            stop("The 'misc3d' package is required for contour mode. Please install it.");
        }
    }

    # ── Load volume data if a name was given ─────────────────────────────────
    if(is.character(volume) && length(volume) == 1L && !startsWith(volume, "#")) {
        vol_data = subject.volume(subjects_dir, subject_id, volume);
    } else {
        vol_data = volume;
    }

    if(!(is.array(vol_data) && length(dim(vol_data)) == 3L)) {
        stop("Parameter 'volume' must be a 3D numeric array or the name of a volume file in the subject's mri/ directory.");
    }

    # ── Render the cortical surface ──────────────────────────────────────────
    if(is.null(measure)) {
        measure = "thickness";
    }

    coloredmeshes = vis.subject.morph.native(
        subjects_dir = subjects_dir,
        subject_id = subject_id,
        measure = measure,
        hemi = hemi,
        surface = surface,
        views = views,
        rgloptions = rgloptions,
        style = surface_style,
        makecmap_options = makecmap_options,
        cortex_only = cortex_only
    );

    # ── Render the volume overlay ────────────────────────────────────────────
    vol_result = NULL;

    if(volume_mode == "contour") {
        # Create isosurface in R array index space, transform to surface RAS, draw into scene
        vol_tris = misc3d::contour3d(vol_data, level = volume_level,
            color = volume_color, alpha = volume_alpha, draw = FALSE);
        vol_tris = apply.transform(vol_tris, index2ras_tkr());
        misc3d::drawScene.rgl(vol_tris, add = TRUE);
        vol_result = vol_tris;

    } else if(volume_mode == "voxels") {
        # volvis.voxels handles the index2ras_tkr transform internally
        # and renders directly into the current rgl scene
        vol_data_bg = vol_data;
        vol_data_bg[vol_data_bg == 0] = NA;
        vol_result = volvis.voxels(vol_data_bg,
            render_every = render_every,
            voxelcol = volume_color, ...);
    }

    return(invisible(list("surface" = coloredmeshes, "volume" = vol_result)));
}


