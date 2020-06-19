# Functions for volume manipulation and rendering.
# The 3D imaging functions are designed to work on gray-scale (single channel) images.


#' @title Extract a slice of a 3D image stack.
#'
#' @description Extracts one or more 2D slices from a 3D image (or a frame of a 4D image). To display the result, you can use \code{\link[fsbrain]{volvis.lightbox}}.
#'
#' @param volume a 3D or 4D image volume. Note that empty dimensions will be dropped before any processing, and the remaining volume must have 3 or 4 dimensions.
#'
#' @param slice_index positive integer or vector of positive integers, the index into the slices (for the axis). A *slice* in the sense of this function is any 2D image plane extracted from the 3D volume (no matter the axis). If NULL, the slice in the middle of the volume is used. One can pass the magic character string 'all' to use all slice indices along the axis.
#'
#' @param frame positive integer, optional. The frame (time point) to use, only relevant for 4D volumes. The last (i.e. 4th) dimension is assumed to be the time dimension in that case.
#'
#' @param axis positive integer, the axis to use when indexing the slices. Defaults to 1.
#'
#' @param rotation integer, rotation in degrees. Defaults to 0 (no ratation). Must be a multiple of 90L if given.
#'
#' @param flip NULL or one of the character strings 'vertically' or 'horizontally'. Note that flipping *horizontally* means that the image will be mirrored along the central *vertical* axis. If `NULL` is passed, nothing is flipped. Flipping occurs after rotation.
#'
#' @return slice data. If `slice_index` is a scalar, a numerical 2D matrix (a 2D image from the stack). Otherwise, a numerical 3D array that contains the selected 2D images.
#'
#' @family volume utility
#'
#' @export
vol.slice <- function(volume, slice_index=NULL, frame=1L, axis=1L, rotation=0L, flip=NULL) {
    if(axis < 1 | axis > 3) {
        stop(sprintf("Axis must be integer with value 1, 2 or 3 but is %d.\n", axis));
    }

    if(length(dim(volume))==4) {
        vol3d = volume[,,,frame];
    } else if(length(dim(volume))==3) {
        vol3d = volume;
    } else {
        stop("Data passed as parameter 'volume' must have 3 or 4 dimensions.");
    }

    if(is.null(slice_index)) {
        # Select a middle slice, the first one is often (almost) empty.
        slice_index = as.integer(round(dim(vol3d)[axis] / 2));
    }

    if(is.character(slice_index)) {
        if(slice_index == 'all') {
            slice_index = seq_len(dim(vol3d)[axis]);
        }
    }

    if(!is.numeric(slice_index)) {
        stop("Could not determine a valid slice index.");
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
            slice = freesurferformats::rotate2D(slice, rotation);
        } else {
            slice = freesurferformats::rotate3D(slice, axis=axis, degrees=rotation);
        }
    }

    if(!is.null(flip)) {
        if(length(slice_index) == 1) {
            slice = freesurferformats::flip2D(slice, how = flip);
        } else {
            slice = freesurferformats::flip3D(slice, axis = axis, how = flip);
        }
    }

    return(slice);
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
#' @keywords internal
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
#' @param apply logical, whether to directly apply the bounding box and return the resulting volume instead.
#'
#' @return named list with 2 entries: `from` is an integer vector of length 3, defining the minimal (x,y,z) foreground indices. `to` is an integer vector of length 3, defining the maximal (x,y,z) foreground indices.
#'
#' @family volume utility
#'
#' @export
vol.boundary.box <- function(volume, threshold=0L, apply=FALSE) {
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
    }
    coords = boxcoords.from.bbox(min_index_per_axis, max_index_per_axis);
    bbox = list("from"=min_index_per_axis, "to"=max_index_per_axis, "edge_coords"=coords);
    if(apply) {
        return(vol.boundary.box.apply(volume, bbox));
    } else {
        return(bbox);
    }
}


#' @title Apply a boundary box to a volume, returning the inner volume part
#'
#' @param volume a 3D image volume
#'
#' @param bbox the boundary box
#'
#' @return a 3D image volume, the inner volume part, resulting from the application of the boundary box
#'
#' @export
vol.boundary.box.apply <- function(volume, bbox) {
    return(volume[bbox$from[1]:bbox$to[1], bbox$from[2]:bbox$to[2], bbox$from[3]:bbox$to[3]]);
}


#' @title Compute the coordinates of the 8 corners of a 3D box.
#'
#' @description Given the extreme values (min and max) along the 3 axes, compute the coordinates of the 8 corners of a 3D box.
#'
#' @param axes_min numerical vector of length 3, the min values of the 3 axes
#'
#' @param axes_max numerical vector of length 3, the max values of the 3 axes
#'
#' @return numerical matrix with 3 columns and 8 rows, the edge coordinates
#'
#' @keywords internal
boxcoords.from.bbox <- function(axes_min, axes_max) {
    xlen = axes_max[1] - axes_min[1];
    ylen = axes_max[2] - axes_min[2];
    zlen = axes_max[3] - axes_min[3];

    coords = matrix(rep(0.0, 8*3), ncol=3);
    coords[1,] = axes_min;
    coords[2,] = c(axes_min[1] + xlen, axes_min[2], axes_min[3]);
    coords[3,] = c(axes_min[1], axes_min[2] + ylen, axes_min[3]);
    coords[4,] = c(axes_min[1] + xlen, axes_min[2] + ylen, axes_min[3]);

    coords[5,] = c(axes_min[1], axes_min[2], axes_max[3]);
    coords[6,] = c(axes_min[1] + xlen, axes_min[2], axes_max[3]);
    coords[7,] = c(axes_min[1], axes_min[2] + ylen, axes_max[3]);
    coords[8,] = axes_max;
    return(coords);
}


#' @title Get indices of the axes defining the given plane.
#'
#' @description When using plane names, this function assumes that the volume is in the standard FreeSurfer orientation, as returned by reading a conformed volume with functions like \code{\link[fsbrain]{subject.volume}}.
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

    # The plane must be a character string when we hit this.
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
#' @description Translate names and indices of 3D image planes. The names only make sense if the data in the volume is in the default FreeSurfer conformed orientation.
#'
#' @param plane NULL, a plane index, or a plane name.
#'
#' @return if `plane` is NULL, all available planes and their indices as a named list. If `plane` is an integer (a plane index), its name. If `plane` is an characters string (a plane name), its index.
#'
#' @family volume utility
#'
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
#' @param volume a 3D image volume. Can be numeric, or something that can be read directly by \code{magick::image_read} in 2D matrices (slices along the axis), e.g., a 3D array of color strings. If a 2D matrix is passed, the resulting stack will contain a single image.
#'
#' @param axis positive integer in range 1L..3L or an axis name, the axis to use.
#'
#' @param intensity_scale integer, value by which to scale the intensities in the volume to the range `[0, 1]`. Only used for numeric volumes. Set to NULL for data that can be read directly by \code{magick::image_read}, and to 1 for intensity data that requires no scaling. Defaults to 255, which is suitable for 8 bit image data.
#'
#' @return a vectorized ImageMagick image, containing one subimage per slice. This can be interpreted as an animation or whatever.
#'
#' @family volume utility
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

    if(length(dim(volume)) == 2) {
        if(is.null(intensity_scale) | !is.numeric(volume)) {
            return(magick::image_read(volume));
        } else {
            return(magick::image_read(grDevices::as.raster(volume / intensity_scale)));
        }
    } else if(length(dim(volume)) == 3) {
        if(is.null(intensity_scale) | !is.numeric(volume)) {
            image_list = apply(volume, axis, function(x){magick::image_read(x)});
        } else {
            image_list = apply(volume, axis, function(x){magick::image_read(grDevices::as.raster(x / intensity_scale))});
        }
        image_stack = Reduce(c, image_list);
        return(image_stack);
    } else{
        stop("The image must have exactly 2 or 3 dimensions.");
    }
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
#'
#' @family volume utility
#'
#' @export
vol.overlay.colors.from.activation <- function(volume, colormap_fn=squash::blueorange, no_act_source_value=0) {
    col = squash::cmap(volume, map = squash::makecmap(volume, colFn = colormap_fn));
    no_act_indices = which(volume == no_act_source_value, arr.ind = TRUE);
    col[no_act_indices] = NA;
    return(col);
}


#' @title Draw a lightbox view from volume slices.
#'
#' @description A lightbox is a single image that holds a set of subimages, arranged in a grid. The images can have a small border or spacing between them. Consecutive subimages will be appear the same row of the grid.
#'
#' @param volume 3D array, can be numeric (gray-scale intensity values) or color strings. If numeric, the intensity values must be in range `[0, 1]`.
#'
#' @param slices slice index definition. If a vector of integers, interpreted as slice indices. If a single negative interger `-n`, interpreted as every `nth` slice, starting at slice 1. The character string 'all' or the value `NULL` will be interpreted as *all slices*.
#'
#' @param axis positive integer in range 1L..3L, the axis to use.
#'
#' @param per_row positive integer, the number of subimages per row in the output image. If `NULL`, automatically computed from the number of slices and the `per_col` parameter.
#'
#' @param per_col positive integer, the number of subimages per column in the output image. If `NULL`, automatically computed from the number of slices and the `per_row` parameter.
#'
#' @param border_geometry string, a geometry string passed to \code{magick::image_border} to define the borders to add to each image tile. The default value adds 5 pixels, both horizontally and vertically.
#'
#' @param background_color string, a valid ImageMagick color string such as "white" or "#000080". The color to use when extending images (e.g., when creating the border). Defaults to black.
#'
#' @param arrange_single_image logical, whether to apply the given arrangement (from parameters `per_row` and `per_column`) even if a single slice (a 2D image) is passed as `volume`. Defaults to FALSE, which prevents that background tiles are added to fill the row up to `per_row` images. This also prevents the border from getting added to a single image, so all you see is the raw image. Set to `TRUE` if you want to arrange even a single image in a row with a border.
#'
#' @description If overlay_colors are given, the volume will be used as the background, and it will only be visible where overlay_colors has transparency.
#'
#' @family volume visualization
#'
#' @export
volvis.lightbox <- function(volume, slices=-5, axis=1L, per_row=5L, per_col=NULL, border_geometry="5x5", background_color = "#000000", arrange_single_image=FALSE) {

    skip_border = FALSE;
    if(length(dim(volume)) == 2) {
        just_a_slice = volume;
        message("Inflating single 2D slice to volume, new axis added at position 1.");
        volume = array(just_a_slice, dim = c(1, dim(just_a_slice)[1], dim(just_a_slice)[2]));
        if(!arrange_single_image) {
            per_row=1L;
            skip_border = TRUE;
        }
    }

    if(is.character(axis)) {
        axis = vol.planes(axis);
    }
    axis = as.integer(axis);
    if(axis < 1L | axis > 3L) {
        stop(sprintf("Axis must be integer with value 1, 2 or 3 but is %d.\n", axis));
    }

    if(is.numeric(volume)) {
        volume = vol.intensity.to.color(volume);
    }

    if(length(dim(volume)) != 3) {
        stop("Volume must have exactly 3 dimensions.");
    }

    # Compute the slice indices from the slice definition
    slice_indices = get.slice.indices(dim(volume), axis, slices);

    # Get the subset of requested slices as a 3D image
    img_slices = vol.slice(volume, slice_index=slice_indices, axis=axis);

    # Transform the slices into an ImageMagick stack of 2D images
    images = vol.imagestack(img_slices, axis=axis);

    # Add tiny border
    if(!skip_border) {
        if(!((is.null(border_geometry) | is.null(background_color)))) {
            #message("Applying border to individual images.");
            images = magick::image_border(images, background_color, border_geometry);
        }
    }

    # Arrange the stack of images
    #merged_img = magick::image_append(images);
    merged_img = magick.grid(images, per_row=per_row, per_col=per_col, background_color = background_color);

    return(merged_img);
}


#' @title Arrange a multi-frame ImageMagick image into a grid.
#'
#' @description Arrange all subimages of the given ImageMagick image into a single 2D image, that contains the subimages arranged in a grid-like structure. Consecutive subimages will be appear the same row.
#'
#' @param magickimage an ImageMagick image
#'
#' @param per_row positive integer, the number of subimages per row in the output image. If `NULL`, automatically computed from the number of slices and the `per_col` parameter.
#'
#' @param per_col positive integer, the number of subimages per column in the output image. If `NULL`, automatically computed from the number of slices and the `per_row` parameter.
#'
#' @param background_color string, a valid ImageMagick color string such as "white" or "#000080". The color to use when extending images (e.g., when creating the border). Defaults to black.
#'
#' @keywords internal
magick.grid <- function(magickimage, per_row=5L, per_col=NULL, background_color = "#000000") {
    images = magickimage;
    num_subimages = length(images);
    if(is.null(per_row) & is.null(per_col)) {
        # If both are none, just return one horizontal strip of images.
        return(magick::image_append(images));
    }

    if(!(is.null(per_row) | is.null(per_col))) {
        # Both are given, let's see whether they make sense.
        expected_per_row = ceiling(num_subimages / as.double(per_col));
        if(per_row != expected_per_row) {
            warning(sprintf("Changing 'per_row' value from %d to %d based on %d subimages and %d per column.\n", per_row, expected_per_row, num_subimages, per_col));
            per_row = expected_per_row;
        }
    } else {
        if(is.null(per_row)) {
            per_row = ceiling(num_subimages / as.double(per_col));
        } else {
            per_col = ceiling(num_subimages / as.double(per_row));
        }
    }

    num_rows = per_col;
    num_columns = per_row;

    #message(sprintf("Distributing %d images over %d rows and %d colums.\n", num_subimages, num_rows, num_columns));

    img_start_indices_each_row = seq.int(from = 1L, to = num_subimages, by=per_row);
    image_data = NULL;
    for(row_idx in seq_len(length(img_start_indices_each_row))) {
        row_start_index = img_start_indices_each_row[[row_idx]];
        row_end_index = row_start_index + (per_row -1);
        num_this_row = per_row;
        if(row_end_index > num_subimages) {  # Can only happen in last row.
            out_of_bounds_row_end_index = row_end_index;
            row_end_index = row_start_index + (num_subimages %% per_row) -1;
            num_this_row = (row_end_index - row_start_index) + 1;
            #message(sprintf("At row %d of %d, handling the %d subimages %d to %d of %d.\n", row_idx, num_rows, num_this_row, row_start_index, row_end_index, num_subimages));

            if(!is.null(background_color)) {
                # Fill the rest of the background with the background color. Otherwise, we will get a white background for the remainder of the last row.
                num_missing = out_of_bounds_row_end_index - row_end_index;
                background_tile = magick::image_blank(magick::image_info(images[1])$width, magick::image_info(images[1])$height, background_color);
                for(tile_idx in 1:num_missing) {
                    images = c(images, background_tile);
                }
                #message(sprintf("-At row %d of %d: row had %d images, missed %d background tiles to a full row of %d images, added them.\n", row_idx, num_rows, num_this_row, num_missing, per_row));
                #message(sprintf("-After adding the  %d missing background tiles, changes row_end_index from %d to %d.\n", num_missing, row_end_index, out_of_bounds_row_end_index));
                row_end_index = out_of_bounds_row_end_index; # It's not out of bounds anymore since we added the background tiles.

            }
        }

        if(is.null(image_data)) {
            image_data = magick::image_append(images[row_start_index:row_end_index], stack=FALSE);
        } else {
            image_row = magick::image_append(images[row_start_index:row_end_index], stack=FALSE);
            image_data = magick::image_append(c(image_data, image_row), stack = TRUE);
        }
    }
    return(image_data);

}


#' @title Compute slice indices from slice definition.
#'
#' @param voldim integer vector, the dimension of the volume
#'
#' @param axis integer, the axis
#'
#' @param slices slice index definition. If a vector of integers, interpreted as slice indices. If a single negative interger `-n`, interpreted as every `nth` slice, starting at slice 1. The character string 'all' or the value `NULL` will be interpreted as *all slices*.
#'
#' @return integer vector, the computed slice indices. They are guaranteed to be valid indices into the volume.
#'
#' @keywords internal
get.slice.indices <- function(voldim, axis, slices) {
    if(is.character(axis)) {
        axis = vol.planes(axis);
    }
    axis = as.integer(axis);
    num_slices_in_volume = voldim[axis];    # along the requested axis
    if(is.numeric(slices)) {
        if(length(slices) == 1 & slices < 0L) {
            # every nth slice
            return(seq.int(from=1L, to=num_slices_in_volume, by=abs(slices)))
        } else {
            if(any(slices > num_slices_in_volume)) {
                stop(sprintf("The %d requested slice indices include %d which are out of bounds, volume has %d slices along axis %d.\n", length(slices), length(which(slices > num_slices_in_volume)), num_slices_in_volume, axis));
            }
            if(any(slices < 0)) {
                stop(sprintf("The %d requested slice indices include %d negative ones which are invalid.\n", length(slices), length(which(slices < 0))));
            }
            return(slices);
        }
    } else if(is.character(slices)) {
        if(slices == "all") {
            return(seq_len(num_slices_in_volume));
        } else {
            stop(sprintf("Slice definition character string '%s' not recognized.", slices));
        }
    } else if (is.null(slices)) {
        return(seq_len(num_slices_in_volume));
    } else {
        stop("Invalid slice definition.");
    }
}


#' @title Extract subset from a volume by value.
#'
#' @description Extract subset from a volume by value, set all other voxel values to `NA`. Typically used to extract a brain structure (like corpus callosum) from a volume segmentation (like the `mri/aseg.mgz` file of a subject). You should consider passing the volume and the include values as integers.
#'
#' @param volume numeric 3D array
#'
#' @param include_values numerical vector, the intensity values which qualify a voxel to be part of the result (without being set to NA)
#'
#' @return numerical array with same dimensions as the input volume. All values which are not part of `include_values` replaced with `NA`.
#'
#' @export
vol.mask.from.segmentation <- function(volume, include_values) {
    if(length(dim(volume)) < 3) {
        stop("Volume must have 3 or more dimensions.");
    }

    mask = array(rep(NA, prod(dim(volume))), dim(volume));

    for(inc_val in include_values) {
        act_indices = which(volume == inc_val, arr.ind = TRUE);
        mask[act_indices] = volume[act_indices];
    }
    return(mask);
}


#' @title Merge background volume and overlay to new colors.
#'
#' @param volume 3D array, can be numeric (gray-scale intensity values) or color strings. If numeric, the intensity values must be in range `[0, 1]`.
#'
#' @param overlay_colors 3D array of color strings, values which are not part of the overlay (and should display background in the result) must have `NA` instead of a color string. Must have same dimensions as the `volume`.
#'
#' @param bbox_threshold numerical, the threshold intensity used to separate background and foreground. All voxels with intensity values greater than this value in the background `volume` will be considered `foreground` voxels. Background-only slices at the borders of the volume will be discarded. Pass `NULL` to use the full image without applying any bounding box.
#'
#' @param forced_overlay_color NULL or an rgb color string, like '#FF0000' for red. If NULL, the activation colors will be used as foreground colors. Otherwise, the given color will be for all foreground vertices.
#'
#' @return 3D array of color strings, the merged colors
#'
#' @family volume utility
#'
#' @importFrom grDevices rgb
#' @export
vol.merge <- function(volume, overlay_colors, bbox_threshold=0L, forced_overlay_color=NULL) {
    if(length(dim(volume)) != 3) {
        stop("Volume must have exactly 3 dimensions.");
    }

    if(!(all.equal(dim(volume), dim(overlay_colors)))) {
        stop("If 'overlay_colors' are given, they must have the same dimension as the 'volume'. Hint: use RGB color strings.");
    }

    if(!is.null(bbox_threshold)) {
        bbox = vol.boundary.box(volume, threshold=bbox_threshold);
        volume = volume[bbox$from[1]:bbox$to[1], bbox$from[2]:bbox$to[2], bbox$from[3]:bbox$to[3]];
        overlay_colors = overlay_colors[bbox$from[1]:bbox$to[1], bbox$from[2]:bbox$to[2], bbox$from[3]:bbox$to[3]];
    }

    # Compute background volume color strings from intensity values if needed.
    if(is.numeric(volume)) {
        merged = vol.intensity.to.color(volume);
    } else {
        merged = volume;
    }

    # Same for the overlay
    if(is.numeric(overlay_colors)) {
        warning("Overlay is numerical and will be gray-scale. It may be hard to discern from the background volume.");
        overlay_colors = vol.intensity.to.color(overlay_colors);
    }

    if(!(all.equal(dim(volume), dim(overlay_colors)))) {
        stop("Bug: dimensions of 'volume' do not match 'overlay_colors' anymore");
    }

    # Copy background colors into NA voxels of the activation.
    overlay_idc = which(!is.na(overlay_colors), arr.ind = TRUE);
    #message(sprintf("Setting %d activated vertices to colors.\n", nrow(overlay_idc)));

    if(is.null(forced_overlay_color)) {
        merged[overlay_idc] = overlay_colors[overlay_idc];
    } else {
        if(!is.character(forced_overlay_color)) {
            stop("Parameter 'forced_overlay_color' must be a color string (like '#FF0000') or NULL.");
        } else {
            merged[overlay_idc] = forced_overlay_color;
        }
    }

    return(merged);
}


#' @title Convert integer intensity image to RGB color string form.
#'
#' @description Convert a gray-scale image defined by intensity values in range `[0, 1]` to an image with identical dimensions that contains an R color string (like `#222222`) at each position. The color strings are computed from the intensities, by taking the intensity value as the value for all three RGB channels. I.e., the output is still gray-scale, but defined in RGB space. To make it clear, this function does **not** apply a colormap. It only changes the representation of the data, not the resulting colors.
#'
#' @param volume numeric array, typically a 3D image with intensities in range `[0, 1]`. This function now also supports numeric matrices (2D images, slices) and numeric vectors (1D).
#'
#' @param scale numeric or character string, a scaling to apply to the values. Defaults to NULL, which means *no scaling* and requires the values in `volume` to be in rage `[0, 1]`. You can pass a number like 255 or the string 'normalize' to scale based on the data. You can pass the string 'normalize_if_needed' to scale only if the data is *outside* the range `[0, 1]`, so that data in range `[0.3, 0.5]` would **not** be rescaled to `[0, 1]`.
#'
#' @return array (or matrix, or vector) of RGB color strings. All of them will represent gray values.
#'
#' @examples
#'    vol.intensity.to.color(c(0.0, 0.5, 1.0));
#'    # output: "#000000" "#808080" "#FFFFFF"
#'    vol.intensity.to.color(c(20, 186, 240), scale="normalize");
#'    vol.intensity.to.color(c(20, 186, 240), scale=255);
#'    vol.intensity.to.color(c(0.0, 0.5, 0.8), scale="normalize");
#'    vol.intensity.to.color(c(0.0, 0.5, 0.8), scale="normalize_if_needed");
#'
#' @importFrom grDevices rgb
#' @export
vol.intensity.to.color <- function(volume, scale=NULL) {
    if(is.numeric(volume)) {

        if(!is.null(scale)) {
            rng = range(volume);
            if(scale == 'normalize') {
                volume = normalize(volume);
            } else if(scale == 'normalize_if_needed') {
                if(rng[1] < 0.0 | rng[2] > 1.0) {
                    volume = normalize(volume);
                }
            } else {
                if(is.numeric(scale) & length(scale) == 1) {
                    volume = volume / scale;
                } else {
                    stop("Parameter 'scale' must be exactly the character string 'normalize', 'normalize_if_needed', or a numeric scalar.");
                }
            }
        }

        rng = range(volume);
        if(rng[1] < 0.0 | rng[2] > 1.0) {
            warning(sprintf("Intensity values of volume are in range range [%.2f, %.2f], please scale the intensity values to range [0, 1] before passing them to this function or use 'scale' parameter.\n", rng[1], rng[2]));
        }

        num_dims = length(dim(volume));
        if(num_dims == 3L) {
            return(array(grDevices::rgb(volume, volume, volume), dim(volume))); # try magick::image_read(vol.slice(return_value)) or volvis.lightbox(return_value)
        } else if (num_dims == 2L) {
            return(matrix(grDevices::rgb(as.vector(volume), as.vector(volume), as.vector(volume)), nrow=nrow(volume)));
        } else if (is.vector(volume)) {
            return(grDevices::rgb(volume, volume, volume));
        } else {
            stop("Parameter 'volume' must have 1, 2, or 3 dimensions.");
        }
    } else {
        stop("Parameter 'volume' must be numeric.");
    }
}


#' @title Normalize data.
#'
#' @description Scales data to the range `[0, 1]` based on min and max values.
#'
#' @param x the data
#'
#' @return the scaled data
#'
#' @keywords internal
normalize <- function(x) {
    return((x- min(x)) /(max(x)-min(x)));
}


#' @title Return triangles for a 3D cube or cuboid.
#'
#' @description Each row of the returned matrix encodes a point (the x, y, and z coordinates), and 3 consecutive rows encode a triangle. Obvisouly, a point will occur several times (as part of several triangles). The result can be passed to \code{\link{triangles3d}} to render a 3D box. The defaults for the parameters will create a cube with edge length 1 centered at (0, 0, 0).
#'
#' @param xmin numeric, minimal x coordinate
#'
#' @param xmax numeric, maximal x coordinate
#'
#' @param ymin numeric, minimal y coordinate
#'
#' @param ymax numeric, maximal y coordinate
#'
#' @param zmin numeric, minimal z coordinate
#'
#' @param zmax numeric, maximal z coordinate
#'
#' @param center numeric vector of length 3 or NULL, coordinates where to center a cube with the edge length defined in parameter `edge_length`. If this is not `NULL`, the parameters `xmin`, `xmax`, ... will be ignored, and their values will be computed for a cube based on the `center` and `edge_length`. Note that you can only create cubes using `center` and `edge_length`, while the min/max methods allows the construction of cuboids.
#'
#' @param edge_length numeric, the edge length of the cube. Only used if parameter `center` is used, ignored otherwise.
#'
#' @return numerical matrix with 36 rows and 3 columns, the 3D coordinates. Each row encodes a point (the x, y, and z coordinates), and 3 consecutive rows encode a triangle.
#'
#' @examples
#'    # Create a cube with edge length 2, centered at (3,4,5):
#'    cube_coords = cube3D.tris(center=c(3,4,5), edge_length=2.0);
#'    # Create the same cube using the min/max method:
#'    cube_coords = cube3D.tris(xmin=2, xmax=4, ymin=3, ymax=5, zmin=4, zmax=6);
#'    # Create a cuboid:
#'    cuboid_coords = cube3D.tris(xmin=2, xmax=4, ymin=3, ymax=9, zmin=4, zmax=5);
#'    # To render the cuboid:
#'    #rgl::triangles3d(cuboid_coords, col="red");
#'
#' @export
cube3D.tris <- function(xmin=-0.5, xmax=0.5, ymin=-0.5, ymax=0.5, zmin=-0.5, zmax=0.5, center=NULL, edge_length=1.0) {

    if(!is.null(center)) {
        if(is.numeric(center) & length(center) == 3) {
            hel = edge_length / 2.0;   # half edge length
            xmin = center[1] - hel;
            xmax = center[1] + hel;
            ymin = center[2] - hel;
            ymax = center[2] + hel;
            zmin = center[3] - hel;
            zmax = center[3] + hel;

        } else {
            stop("If given, 'center' must be a numeric vector of length 3: the x, y, and z coordinates of the cube center.");
        }
    }

    tris_cube = matrix(c(xmin, ymin, zmin, # the 2 front tris start
                         xmin, ymin, zmax,
                         xmax, ymin, zmin,
                         # tris 2
                         xmax, ymin, zmin,
                         xmin, ymin, zmax,
                         xmax, ymin, zmax,
                         # tris 3: the back starts (shifted by +1.0 on y axis compared to front)
                         xmin, ymax, zmin,
                         xmin, ymax, zmax,
                         xmax, ymax, zmin,
                         # tris 4
                         xmax, ymax, zmin,
                         xmin, ymax, zmax,
                         xmax, ymax, zmax,
                         # tris 5: the left side starts
                         xmin, ymax, zmin,
                         xmin, ymax, zmax,
                         xmin, ymin, zmin,
                         # tris 6
                         xmin, ymin, zmin,
                         xmin, ymax, zmax,
                         xmin, ymin, zmax,
                         # tris 7: the right side starts (shifted by +1.0 on x axis compared to left side)
                         xmax, ymax, zmin,
                         xmax, ymax, zmax,
                         xmax, ymin, zmin,
                         # tris 8
                         xmax, ymin, zmin,
                         xmax, ymax, zmax,
                         xmax, ymin, zmax,
                         # tris 9: the bottom starts
                         xmin, ymin, zmin,
                         xmin, ymax, zmin,
                         xmax, ymin, zmin,
                         # tris 10
                         xmax, ymin, zmin,
                         xmin, ymax, zmin,
                         xmax, ymax, zmin,
                         # tris 11: the top starts (shifted by +1.0 on z axis compared to the bottom)
                         xmin, ymin, zmax,
                         xmin, ymax, zmax,
                         xmax, ymin, zmax,
                         # tris 12
                         xmax, ymin, zmax,
                         xmin, ymax, zmax,
                         xmax, ymax, zmax
    ), ncol=3, byrow = TRUE);
    return(tris_cube);
}



#' @title Vectorized version of cube3D.tris
#'
#' @param centers numerical matrix with 3 columns. Each column represents the x, y, z coordinates of a center at which to create a cube.
#'
#' @param edge_length numerical vector or scalar, the edge length. Must have length 1 (same edge length for all cubes), or the length must be identical to the number of rows in parameter `centers`.
#'
#' @return matrix of triangle coordinates, see \code{\link[fsbrain]{cube3D.tris}}.
#'
#' @examples
#'    # Plot a 3D cloud of 20000 voxels:
#'    centers = matrix(rnorm(20000*3)*100, ncol=3);
#'    rgl::triangles3d(cubes3D.tris(centers));
#'
#' @export
cubes3D.tris <- function(centers, edge_length=1) {
    if(is.vector(centers)) {
        centers = matrix(centers, ncol = 3);
    }

    num_tris = nrow(centers);

    if(length(edge_length) == 1) {
        edge_length = rep(edge_length, num_tris);
    }

    points_per_rect = 36L;    # Each 3D rectangle consists of 12 triangles, each of which has 3 points.

    tris = matrix(rep(NA, num_tris*points_per_rect*3), ncol=3);
    for(row_idx in seq_len(nrow(centers))) {
        start_idx = (row_idx - 1) * points_per_rect + 1;
        end_idx = start_idx + points_per_rect - 1;
        tris[start_idx:end_idx,] = cube3D.tris(center=centers[row_idx,], edge_length = edge_length[row_idx]);
    }
    return(tris);
}


#' @title Compute voxel colors based on colortable.
#'
#' @description Use the intensity values of the voxels in volume and lookup the respective colors in a colortable.
#'
#' @param volume numeric 3D array, the values should be integers present in the `struct_index` column of the colortable. All other values will be assigned `NA` as a color.
#'
#' @param colortable a colortable, as returned by \code{\link[freesurferformats]{read.fs.colortable}}.
#'
#' @param ignored_struct_indices integer vector, `struct_index` entries in the colortable that should be ignored
#'
#' @param ignored_struct_names vector of character strings, `struct_name` entries in the colortable that should be ignored. Can be combined with `ignored_struct_indices`.
#'
#' @return character string 3D array, the colors. Voxels in the volume which were not matched by the colortable are set to `NA` in it.
#'
#' @export
vol.overlay.colors.from.colortable <- function(volume, colortable, ignored_struct_indices=c(), ignored_struct_names=c('unknown', 'Unknown')) {
    if(length(dim(volume)) != 3) {
        stop("Volume must have exactly 3 dimensions.");
    }

    overlay_colors = array(rep(NA, length(volume)), dim(volume));

    for(row_idx in seq_len(nrow(colortable))) {
        ct_structure = colortable[row_idx, ];
        if(ct_structure$struct_index %in% ignored_struct_indices | ct_structure$struct_name %in% ignored_struct_names) {
            #cat(sprintf("Skipping colortable entry with struct_index %d, named '%s'.\n", ct_structure$struct_index, ct_structure$struct_name));
            next;
        }
        struct_voxels = which(volume==ct_structure$struct_index, arr.ind = TRUE);
        voxel_color = grDevices::rgb(ct_structure$r/255., ct_structure$g/255., ct_structure$b/255.);
        if(nrow(struct_voxels) > 0) {
            #cat(sprintf("Assigning %d voxels to color %s from colortable entry with struct_index %d, named '%s'.\n", nrow(struct_voxels), voxel_color, ct_structure$struct_index, ct_structure$struct_name));
            overlay_colors[struct_voxels] = voxel_color;
        }
    }
    return(overlay_colors);
}


