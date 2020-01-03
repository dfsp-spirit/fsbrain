# Functions for volume rendering. Totally WIP.


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
#' @param show logical, whether to display the slice. Will display the first slice only if `slice_index` is a vector and print information on the slice to stdout. Defaults to `FALSE`.
#'
#' @return slice data. If `slice_index` is a scalar, a numerical 2D matrix (a 2D image from the stack). Otherwise, a numerical 3D array that contains the selected 2D images.
#'
# @keywords internal
#' @importFrom grDevices gray.colors
#' @importFrom graphics image
#' @export
vol.slice <- function(volume, slice_index=NULL, frame=1L, axis=1L, show=FALSE) {
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


#' @title Compute foreground pixels over the whole 3D imagestack.
#'
#' @description Compute, over all images in a stack along an axis, the foreground and background pixels as a binary mask. A pixel is a `foreground` pixel iff its value is greater than 0 in at least one of the slices. A pixel is a `background` pixel iff its value is exactly 0 in all slices.
#'
#' @param volume a 3D image volume
#'
#' @param plane integer vector of length 2 or something that will be turned into one by \code{\link[fsbrain]{vol.plane.axes}}.
#'
#' @return integer 2D matrix with dimensions of a slice of the volume. Positions set to 1 are `foreground` pixels and positions set to 0 are `background` pixels (see `Details` section).
#'
#'
#' @export
vol.boundary.mask <- function(volume, plane=1L) {
    axes = vol.plane.axes(plane);
    if(length(dim(volume)) != 3) {
        stop(sprintf("Volume must have exactly 3 dimensions but has %d.\n", length(dim(volume))));
    }
    maximg = apply(volume, axes, max);
    foreground_indices = which(maximg > 0, arr.ind=TRUE);

    height = dim(volume)[axes[1]];
    width = dim(volume)[axes[2]];

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
#' @return named list with 2 entries: `from` is an integer vector of length 3, defining the minimal (x,y,z) foreground indices. `to` is an integer vector of length 3, defining the maximal (x,y,z) foreground indices.
#'
#' @export
vol.boundary.box <- function(volume) {
    if(length(dim(volume)) != 3) {
        stop(sprintf("Volume must have exactly 3 dimensions but has %d.\n", length(dim(volume))));
    }
    min_index_per_axis = rep(-1L, 3);
    max_index_per_axis = rep(-1L, 3);
    for(axis in c(1L, 2L, 3L)) {
        mmask = vol.boundary.mask(volume, axis);
        colmax = apply(mmask, 1, max);
        min_index_per_axis[axis] = Position(function(x) x >= 1L, colmax, right=FALSE);
        max_index_per_axis[axis] = Position(function(x) x >= 1L, colmax, right=TRUE);
        #cat(sprintf("At axis %d, determined min=%d, max=%d (used columns of the %d rows and %d colums).\n", axis, min_index_per_axis[axis], max_index_per_axis[axis], nrow(mmask), ncol(mmask)));
        #print(colmax);
    }
    return(list("from"=min_index_per_axis, "to"=max_index_per_axis));
}



#' @title Get indices of the axes defining the given plane.
#'
#' @description This function assumes that the volume is in the standard FreeSurfer orientation, as returned by reading a volume with functions like \code{\link[fsbrain]{subject.volume}}.
#'
#' @param plane_name string, one of "axial", "coronal", or "sagittal". If this is an integer vector of length 2 already, it is returned as given. If it is a single integer, it is interpreted as an axis index, and the plane orthogonal to the axis is returned.
#'
#' @return integer vector of length 2, the axes indices.
#'
#' @keywords internal
vol.plane.axes <- function(plane_name) {
    if(is.double(plane_name)) {
        plane_name = as.integer(plane_name);
    }
    if(is.integer(plane_name)) {
        if(length(plane_name) == 2L) {   # Already done. We do not check whether the contents makes sense in this case.
            return(plane_name);
        } else if(length(plane_name) == 1L) {
            # Treat it as an axis, and return the plane that is orthogonal to the axis
            if(plane_name == 1L) {
                return(c(1L, 2L));
            } else if(plane_name == 2L) {
                return(c(2L, 3L));
            } else if(plane_name == 3L) {
                return(c(3L, 1L));
            } else {
                stop("If plane is an integer (vector), the values must be in range 1..3");
            }
        } else {
            stop("If plane is an integer vector, it must have length 1 or 2.");
        }
    }
    if(!(plane_name %in% c("axial", "coronal", "sagittal"))) {
        stop(sprintf("Parameter 'plane_name' must be on of c('axial', 'coronal', 'sagittal') but is '%s'.\n", plane_name));
    }
    if(plane_name == "sagittal") {
        return(c(2L, 3L));
    } else if(plane_name == "coronal") {
        return(c(1L, 2L))
    } else { # axial
        return(c(3L, 1L));
    }
}


#' @title Turn volume into an ImageMagick image stack.
#'
#' @description Create an image from each slice along the axis, then stack those into an ImageMagick image stack.
#'
#' @param volume a 3D image volume
#'
#' @param axis positive integer in range 1L..3L, the axis to use.
#'
#' @param intensity_scale integer, value by which to scale the intensities in the volume to reach range [0, 1]. Set to 1 for no scaling. Defaults to 255, which is suitable for 8 bit image data.
#'
#' @return a vectorized ImageMagick image, containing one subimage per slice. This can be interpreted as an animation or whatever.
#'
#' @export
vol.imagestack <- function(volume, axis=1L, intensity_scale=255) {
    if(axis < 1 | axis > 3) {
        stop(sprintf("Axis must be integer with value 1, 2 or 3 but is %d.\n", axis));
    }
    if(length(dim(volume)) != 3) {
        stop(sprintf("Volume must have exactly 3 dimensions but has %d.\n", length(dim(volume))));
    }
    image_list = apply(volume, 1, function(x){magick::image_read(grDevices::as.raster(x / intensity_scale))});
    image_stack = Reduce(c, image_list);
    return(image_stack);
}

