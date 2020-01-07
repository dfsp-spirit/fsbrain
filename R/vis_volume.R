# Functions for volume manipulation and rendering.
# The 3D imaging functions are designed to work on gray-scale (single channel) images.


#' @title Extract and visualize a slice of a 3D image stack.
#'
#' @description Extracts one or more 2D slices from a 3D image (or a frame of a 4D image). Optionally, visualizes the first of the selected slices.
#'
#' @param volume a 3D or 4D image volume. Note that empty dimensions will be dropped before any processing, and the remaining volume must have 3 or 4 dimensions.
#'
#' @param slice_index positive integer or vector of positive integers, the index into the slices (for the axis). If NULL, the slice in the middle of the volume is used.
#'
#' @param frame positive integer, optional. The frame (time point) to use, only relevant for 4D volumes. The last dimension is assumed to be the time dimension in that case.
#'
#' @param axis positive integer, the axis to use when indexing the slices. Defaults to 1.
#'
#' @param rotation integer, rotation in degrees. Defaults to 0 (no ratation). Must be a multiple of 90 if given. Currently only supported if slice_index is scalar.
#'
#' @param flip logical, whether to flip the slice. Currently only supported if slice_index is scalar.
#'
#' @param show logical, whether to display the slice. Will display the first slice only if `slice_index` is a vector and print information on the slice to stdout. Defaults to `FALSE`.
#'
#' @return slice data. If `slice_index` is a scalar, a numerical 2D matrix (a 2D image from the stack). Otherwise, a numerical 3D array that contains the selected 2D images.
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics image
#' @export
vol.slice <- function(volume, slice_index=NULL, frame=1L, axis=1L, rotation=0L, flip=FALSE, show=FALSE) {
    if(axis < 1 | axis > 3) {
        stop(sprintf("Axis must be integer with value 1, 2 or 3 but is %d.\n", axis));
    }
    volume = drop(volume); # drop empty dimensions
    if(length(dim(volume))==4) {
        vol3d = volume[,,,frame];
    } else if(length(dim(volume))==3) {
        vol3d = volume;
    } else {
        stop("Data passed as parameter 'volume' must have 3 or 4 (non-empty) dimensions.");
    }

    if(is.null(slice_index)) {
        # Select a middle slice, the first one is often (almost) empty.
        slice_index = as.integer(round(dim(vol3d)[axis] / 2));
    }

    # Limit the index to the range 1..axis dim
    slice_index = ifelse(slice_index < 1L, 1L, slice_index);
    slice_index = ifelse(slice_index > dim(vol3d)[axis], dim(vol3d)[axis], slice_index);

    # Select requested axis
    if(axis == 1L) {
        slice = vol3d[slice_index,,];
    } else if(axis == 2L) {
        slice = vol3d[,slice_index,];
    } else {
        slice = vol3d[,,slice_index];
    }

    if(rotation != 0L) {
        if(length(slice_index) == 1) {
            slice = rotate2D(slice, rotation);
        } else {
            warning("Rotation request ignored for multi-slice index.");
        }
    }

    if(flip) {
        if(length(slice_index) == 1) {
            slice = flip2D(slice);
        } else {
            warning("Flip request ignored for multi-slice index.");
        }
    }

    if(show) {
        if(length(slice_index) > 1) {
            shown_slice_index = slice_index[1];    # shown_slice_index is the index of the slice in the volume (not in the vector of requested slices)
            shown_slice = slice[1,,];              # select the first slice from the matrix of selected slices
        } else {
            shown_slice_index = slice_index;
            shown_slice = slice;
        }

        message(sprintf("Slice %d/%d at axis %d, with dimensions '%s'.\n", shown_slice_index, dim(vol3d)[axis], axis, paste(dim(shown_slice), collapse="x")));
        image(shown_slice, col=grDevices::gray.colors(n=255, 0.0, 1.0), useRaster = TRUE);
    }
    return(slice);
}

#' @title Flip a 2D matrix.
#'
#' @param slice a 2D matrix
#'
#' @return 2D matrix, the flipped matrix
#'
#' @export
flip2D <- function(slice) {
    if(length(dim(slice)) != 2L) {
        stop("Slice must be a 2D matrix.");
    }
    return(as.matrix(rev(as.data.frame(rotate2D(slice, degrees=180L)))));
}

#' @title Rotate a 2D matrix in 90 degree steps.
#'
#' @param slice a 2D matrix
#'
#' @param degrees integer, must be a (positive or negative) multiple of 90
#'
#' @return 2D matrix, the rotated matrix
#'
#' @export
rotate2D <- function(slice, degrees=90) {
    if(length(dim(slice)) != 2L) {
        stop("Slice must be a 2D matrix.");
    }
    degrees = as.integer(degrees %% 360L);
    if(!degrees %in% as.integer(c(0, 90, 180, 270))) {
        stop("Parameter 'degrees' must be a multiple of 90 (it can be negative).");
    }
    if(degrees == 0L) {
        return(slice);
    } else if(degrees == 270) {
        return(rotate90(slice, times=1L, clockwise=FALSE));
    } else if(degrees == 180) {
        return(rotate90(slice, times=2L));
    } else {  # 90
        return(rotate90(slice));
    }
}


#' @title Rotate 2D matrix clockwise in 90 degree steps.
#' @keywords internal
rotate90 <- function(mtx, times=1L, clockwise=TRUE) {
    for(i in seq_len(times)) {
        if(clockwise) {
            mtx = t(apply(mtx, 2, rev));
        } else {
            mtx = apply(t(mtx), 2, rev);
        }
    }
    return(mtx);
}


#' @title Compute foreground pixels over the whole 3D imagestack.
#'
#' @description Compute, over all images in a stack along an axis, the foreground and background pixels as a binary mask. A pixel is a `foreground` pixel iff its value is greater than the `threshold` parameter in at least one of the slices. A pixel is a `background` pixel iff its value is below or euqal to the `threshold` in all slices.
#'
#' @param volume a 3D image volume
#'
#' @param plane integer vector of length 2 or something that will be turned into one by \code{\link[fsbrain]{vol.plane.axes}}.
#'
#' @param threshold numerical, the threshold intensity used to separate background and foreground. All voxels with intensity values greater than this value will be considered `foreground` voxels.
#'
#' @return integer 2D matrix with dimensions of a slice of the volume. Positions set to 1 are `foreground` pixels and positions set to 0 are `background` pixels (see `Details` section).
#'
#'
#' @export
vol.boundary.mask <- function(volume, plane=1L, threshold=0L) {
    axes = vol.plane.axes(plane);
    if(length(dim(volume)) != 3) {
        stop(sprintf("Volume must have exactly 3 dimensions but has %d.\n", length(dim(volume))));
    }

    height = dim(volume)[axes[1]];
    width = dim(volume)[axes[2]];

    if(is.null(threshold)) {
        return(matrix(rep(1L, width*height), nrow=height));
    }

    maximg = apply(volume, axes, max);
    foreground_indices = which(maximg > threshold, arr.ind=TRUE);

    mask = matrix(rep(0L, width*height), nrow=height);
    mask[foreground_indices] = 1L;
    #cat(sprintf(" Using axes '%s', dimension of mask is '%s'.\n", paste(axes, collapse=", "), paste(dim(mask), collapse=", ")));
    return(mask);
}


#' @title Compute 3D bounding box of a volume.
#'
#' @description Compute the axis-aligned foreground bounding box of a 3D volume, i.e. the inner foreground area that must be retained if you want to remove all background from the corners of the volume. The foreground is determined by thresholding, such that all values greater than 0 are considered foreground. See \code{\link[fsbrain]{vol.boundary.mask}} for details.
#'
#' @param volume a 3D image volume
#'
#' @param threshold numerical, the threshold intensity used to separate background and foreground. All voxels with intensity values greater than this value will be considered `foreground` voxels.
#'
#' @return named list with 2 entries: `from` is an integer vector of length 3, defining the minimal (x,y,z) foreground indices. `to` is an integer vector of length 3, defining the maximal (x,y,z) foreground indices.
#'
#' @export
vol.boundary.box <- function(volume, threshold=0L) {
    if(length(dim(volume)) != 3) {
        stop(sprintf("Volume must have exactly 3 dimensions but has %d.\n", length(dim(volume))));
    }
    min_index_per_axis = rep(-1L, 3);
    max_index_per_axis = rep(-1L, 3);
    for(axis in c(1L, 2L, 3L)) {
        mmask = vol.boundary.mask(volume, axis, threshold=threshold);
        colmax = apply(mmask, 1, max);
        min_index_per_axis[axis] = Position(function(x) x >= 1L, colmax, right=FALSE);
        max_index_per_axis[axis] = Position(function(x) x >= 1L, colmax, right=TRUE);
        #cat(sprintf("At axis %d, determined min=%d, max=%d (used columns of the %d rows and %d colums).\n", axis, min_index_per_axis[axis], max_index_per_axis[axis], nrow(mmask), ncol(mmask)));
    }
    return(list("from"=min_index_per_axis, "to"=max_index_per_axis));
}



#' @title Get indices of the axes defining the given plane.
#'
#' @description This function assumes that the volume is in the standard FreeSurfer orientation, as returned by reading a volume with functions like \code{\link[fsbrain]{subject.volume}}.
#'
#' @param plane integer or string. If a string, one of "axial", "coronal", or "sagittal". If this is an integer vector of length 2 already, it is returned as given. If it is a single integer, it is interpreted as an axis index, and the plane orthogonal to the axis is returned. A warning on using the plane names: these only make sense if the volume is in the expected orientation, no checking whatsoever on this is performed.
#'
#' @return integer vector of length 2, the axes indices.
#'
#' @keywords internal
vol.plane.axes <- function(plane) {
    if(is.double(plane)) {
        plane = as.integer(plane);
    }
    if(is.integer(plane)) {
        if(length(plane) == 2L) {   # Already done. We do not check whether the contents makes sense in this case.
            return(plane);
        } else if(length(plane) == 1L) {
            # Treat it as an axis, and return the plane that is orthogonal to the axis
            if(plane == 1L) {
                return(c(1L, 2L));
            } else if(plane == 2L) {
                return(c(2L, 3L));
            } else if(plane == 3L) {
                return(c(3L, 1L));
            } else {
                stop("If plane is an integer (vector), the values must be in range 1..3");
            }
        } else {
            stop("If plane is an integer vector, it must have length 1 or 2.");
        }
    }
    if(!(plane %in% c("axial", "coronal", "sagittal"))) {
        stop(sprintf("Parameter 'plane' must be on of c('axial', 'coronal', 'sagittal') but is '%s'.\n", plane));
    }
    if(plane == "sagittal") {
        return(c(2L, 3L));
    } else if(plane == "coronal") {
        return(c(1L, 2L))
    } else { # axial
        return(c(3L, 1L));
    }
}


#' @title Translate names and indices of planes.
#'
#' @description Translate names and indices of 3D image planes. The names only make sense if the volume is in the default FreeSurfer orientation.
#'
#' @param plane NULL, a plane index, or a plane name.
#'
#' @return if `plane` is NULL, all available planes and their indices as a named list. If `plane` is an integer (a plane index), its name. If `plane` is an characters string (a plane name), its index.
#' @export
vol.planes <- function(plane=NULL) {
    planes = list("coronal"=1, "sagittal"=2, "axial"=3);
    if(is.null(plane)) {
        return(planes);
    } else {
        if(is.character(plane)) {
            return(planes[[plane]]);
        } else {
            return(names(planes)[plane]);
        }
    }
}


#' @title Turn volume into an ImageMagick image stack.
#'
#' @description Create an image from each slice along the axis, then stack those into an ImageMagick image stack.
#'
#' @param volume a 3D image volume
#'
#' @param axis positive integer in range 1L..3L or an axis name, the axis to use.
#'
#' @param intensity_scale integer, value by which to scale the intensities in the volume to the range `[0, 1]`. Set to NULL for no scaling. Defaults to 255, which is suitable for 8 bit image data.
#'
#' @return a vectorized ImageMagick image, containing one subimage per slice. This can be interpreted as an animation or whatever.
#'
#' @export
vol.imagestack <- function(volume, axis=1L, intensity_scale=255) {
    if(is.character(axis)) {
        axis = vol.planes(axis);
    }
    axis = as.integer(axis);
    if(axis < 1L | axis > 3L) {
        stop(sprintf("Axis must be integer with value 1, 2 or 3 but is %d.\n", axis));
    }
    if(length(dim(volume)) != 3) {
        stop(sprintf("Volume must have exactly 3 dimensions but has %d.\n", length(dim(volume))));
    }
    if(is.null(intensity_scale)) {
        image_list = apply(volume, axis, function(x){magick::image_read(x)});
    } else {
        image_list = apply(volume, axis, function(x){magick::image_read(grDevices::as.raster(x / intensity_scale))});
    }
    image_stack = Reduce(c, image_list);
    return(image_stack);
}


#' @title Generate colors for a 3D volume, based on the activation data and a colormap.
#'
#' @description Applies the colormap function to the data, then sets the alpha value (transparency) to full in all areas without any activation. Feel free to clip data or whatever before passing it, so that all your no-activation data has the same value.
#'
#' @param volume a 3D array, the activation data (or p-values, effect sizes, or whatever)
#'
#' @param colormap_fn function, a colormap function
#'
#' @param no_act_source_value numerical scalar, the value from the data in 'volume' that means no activation. The output colors for this value will be set to `NA`.
#'
#' @return a 3D matrix of color strings, with the same dimensions as the input volume
#'
#' @importFrom squash makecmap blueorange cmap
#' @importFrom grDevices adjustcolor
#' @export
vol.overlay.colors.from.activation <- function(volume, colormap_fn=squash::blueorange, no_act_source_value=0) {
    col = squash::cmap(volume, map = squash::makecmap(volume, colFn = colormap_fn));
    no_act_indices = which(volume == no_act_source_value);
    #no_act_colors = col[no_act_indices];
    #no_act_colors = grDevices::adjustcolor(no_act_colors, 1.0);
    col[no_act_indices] = NA;
    return(col);
}


#' @title Draw a lightbox from a volume.
#'
#' @description If overlay_colors are given, the volume will be used as the background, and it will only be visible where overlay_colors has transparency.
#' @export
vol.lightbox <- function(volume, axis=1L, slice_arrange=c(5,5)) {

    return(volume);
}


#' @title Merge background volume and overlay to new colors.
#'
#' @param volume 3D array, can be numeric (gray-scale intensity values) or color strings. If numeric, the intensity values must be in range `[0, 1]`.
#'
#' @param overlay_colors 3D array of color strings, values which are not part of the overlay (and should display background in the result) must have `NA` instead of a color string. Must have same dimensions as the `volume`.
#'
#' @param bbox_threshold numerical, the threshold intensity used to separate background and foreground. All voxels with intensity values greater than this value in the background `volume` will be considered `foreground` voxels. Background-only slices at the borders of the volume will be discarded. Pass `NULL` to use the full image without applying any bounding box.
#'
#' @return 3D array of color strings, the merged colors
#'
#' @importFrom grDevices rgb
#' @export
vol.merge <- function(volume, overlay_colors, bbox_threshold=0L) {
    if(!(all.equal(dim(volume), dim(overlay_colors)))) {
        stop("If 'overlay_colors' are given, they must have the same dimension as the 'volume'. Hint: use RGB color strings.");
    }
    bbox = vol.boundary.box(volume, threshold=bbox_threshold);
    volume = volume[bbox$from[1]:bbox$to[1], bbox$from[2]:bbox$to[2], bbox$from[3]:bbox$to[3]];

    overlay_colors = overlay_colors[bbox$from[1]:bbox$to[1], bbox$from[2]:bbox$to[2], bbox$from[3]:bbox$to[3]];

    # Compute background volume color strings from intensity values if needed.
    if(is.numeric(volume)) {
        message("Converting volume from intensities to color strings.");
        volume = array(grDevices::rgb(volume, volume, volume), dim(volume)); # try magick::image_read(vol.slice(volume))
    }

    # Same for the overlay
    if(is.numeric(overlay_colors)) {
        message("Converting overlay_colors from intensities to color strings.");
        overlay_colors = array(grDevices::rgb(overlay_colors, overlay_colors, overlay_colors), dim(overlay_colors));
    }

    # Copy background colors into NA voxels of the activation.
    no_act_indices = which(is.na(overlay_colors), arr.ind=F);
    overlay_colors[no_act_indices] = volume[no_act_indices];
    return(overlay_colors);
}


