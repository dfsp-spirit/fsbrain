# Visualization of a volume as a set of nested, semi-transparent iso-surface shells.
#
# Each shell is one iso-level of the volume, extracted with the optional 'Rvcg' package (fast,
# vertex-welded meshes with normals) or with the optional 'misc3d' package (unwelded meshes, slower).
# The shells are rendered as one coloredmesh per level, each with its own alpha value, which the
# renderers apply per mesh (neither rgl nor scimesh support per-vertex alpha reliably).


#' @title Visualize a volume as nested, semi-transparent iso-surface shells.
#'
#' @description Visualize the iso-surfaces (*shells*) of a volume at several levels as nested, semi-transparent meshes, like the nested contour lines of a topographic map. This is useful to show the shape and the internal structure of a volume at the same time, e.g., for a statistical map: the outer shells (low levels) are rendered more transparently than the inner ones (high levels), so that the inner structure remains visible. The volume is extracted in voxel space and transformed to surface RAS with the FreeSurfer \code{\link[fsbrain]{vox2ras_tkr}} matrix, so the result is spatially aligned with surface renderings of the same subject (see \code{\link[fsbrain]{vis.subject.morph.native}}). To combine the shells with a surface rendering, use the `rglactions` parameter of the surface function (key 'no_vis') to keep the rendering window open, or call this function with `views = NULL` and render the returned meshes yourself.
#'
#' @param volume a 3D numerical array (or an `fs.volume` instance), the volume to visualize. Values 0 and `NA` are treated as background when the levels are computed automatically, and the shells are the iso-surfaces at the computed (or given) levels.
#'
#' @param levels numerical vector or `NULL`. The iso-levels at which to extract shells, in the units of the volume data. Must be ascending (the first level is the outermost shell). If `NULL` (the default), the levels are computed automatically, see `num_levels` and `level_type`.
#'
#' @param num_levels positive integer, the number of shells to create. Ignored if `levels` is given. Defaults to 5.
#'
#' @param level_type character string, how to compute the levels if `levels` is `NULL`. One of 'quantile' (the default) or 'linear'. For 'quantile', each shell contains the same fraction of the foreground voxels, for 'linear', the levels are equally spaced within the range given by `level_range`. In both cases only voxels with finite, non-zero values are considered.
#'
#' @param level_range numerical vector of length 2, the range from which the automatic levels are taken. For `level_type = 'quantile'`, this is interpreted as quantiles (values between 0 and 1) of the foreground values, for `level_type = 'linear'` as fractions of the range of the foreground values. Defaults to `c(0.2, 0.95)`, i.e., the innermost shell is at the 95th percentile of the voxel values, so that a few extreme voxels do not dominate the visualization.
#'
#' @param frame positive integer, the frame (4th dimension) to use for a 4D volume. Defaults to 1.
#'
#' @param colors character vector of colors, one per shell (outermost shell first). Defaults to `NULL`, in which case the colors are taken from the `palette`.
#'
#' @param alphas numerical vector of alpha values, one per shell (outermost shell first), values between 0 and 1. Defaults to `NULL`, in which case the alpha values are taken from the `palette`.
#'
#' @param palette character string, the color and transparency recipe used for the shells if `colors` and/or `alphas` are not given, one of 'grey_context' (the default), 'sequential' or 'viridis'. See \code{\link[fsbrain]{shell.palette}} for the details.
#'
#' @param alpha_range numerical vector of length 2 or `NULL`, the alpha value of the outermost and of the innermost shell. If `NULL` (the default), the range defined by the `palette` is used. Ignored if `alphas` is given.
#'
#' @param smoothing non-negative integer, the number of 3x3x3 box blur passes applied to the volume before the shells are extracted. This removes the staircase artifacts of the marching cubes algorithm and leads to much nicer surfaces. Defaults to 1. Set to 0 to disable and extract the shells from the raw volume.
#'
#' @param downsample positive integer, a factor by which the volume is subsampled before the shells are extracted. Values larger than 1 reduce the number of triangles (and thus the rendering time) considerably, at the cost of a less detailed surface. The coordinates of the subsampled shells are corrected, so the result is still aligned with the original volume. Defaults to 1. Note that the volume is smoothed before subsampling if `smoothing` is larger than 0, otherwise the shells are prone to aliasing artifacts.
#'
#' @param cut_away character string or `NULL`. If not `NULL`, the volume is cut open along one of the six anatomical directions, so that one can look inside: the part of the volume in the given direction is removed. Valid values are 'left', 'right', 'posterior', 'anterior', 'inferior' and 'superior' (in FreeSurfer RAS convention, i.e., +x is right, +y is anterior, +z is superior). The caps of the cut are not closed, so the shells are open at the cut plane. Defaults to `NULL`, i.e., no cut.
#'
#' @param cut_fraction numerical value between 0 and 1, the position of the cut along the axis given by `cut_away`, as a fraction of the bounding box of the shells. Defaults to 0.5, i.e., the volume is cut in the middle.
#'
#' @param backend character string, the backend used to extract the iso-surfaces. One of 'auto' (the default: use the `Rvcg` package if it is installed, and fall back to `misc3d` otherwise), 'Rvcg' or 'misc3d'. `Rvcg` is the recommended backend: it is faster and returns vertex-welded meshes (i.e., meshes that need much less memory) with normals. Note that at least one of the two optional packages must be installed.
#'
#' @param views list of character strings, the views to visualize. Available views are 'sd_<angle>' (a static view, see \code{\link[fsbrain]{vis.subject.morph.native}}), 't4'/'t9' (a 2x2 or 3x3 lightbox of views) and 'si'/'sr' (single interactive / rotating, requires the rgl renderer backend). Defaults to the 4 standard lateral and medial views. Pass `NULL` to skip the rendering entirely and only compute the meshes (which are also returned).
#'
#' @param rgloptions option list passed to \code{\link[rgl]{par3d}}. Defaults to the package default, see \code{\link[fsbrain]{rglo}}.
#'
#' @param rglactions named list, passed to the visualization functions, see \code{\link[fsbrain]{rglactions}}. The key 'no_vis' is useful to prepare the scene and add other data to it later. Note that saving the rendered scene to an image file is done with \code{\link[fsbrain]{export}}, which takes the return value of this function and supports merging several views into one image.
#'
#' @param silent logical, whether to suppress the console messages which report the levels and the backend used. Defaults to `FALSE`.
#'
#' @return the list of `fs.coloredmesh` instances, one per shell (from the outermost to the innermost), invisibly. Note that the colorbar of the rendering functions is not used here, the color of a shell encodes its level, which is reported in the console unless `silent` is `TRUE`.
#'
#' @note Both renderers are used with the mesh's own style (see \code{\link[fsbrain]{get.rglstyle}}), which sets the alpha value of the shell. Note that backfaces are rendered for the shells (`back = 'filled'`): if they were culled, the rgl renderer would composite the nested transparent shells in the wrong order (the innermost shell would end up on top of the outer ones).
#'
#' @seealso \code{\link[fsbrain]{volvis.contour}} for a single iso-surface, \code{\link[fsbrain]{volvis.voxels}} for a voxel-based rendering of a volume, and \code{\link[fsbrain]{vis.volume.on.surface}} to combine a volume with a surface rendering.
#'
#' @examples
#' \dontrun{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    brain = subject.volume(subjects_dir, 'subject1', 'brain');
#'    # Show the 5 inner shells of the brain, cut open from the right:
#'    shells = volvis.shells(brain, num_levels = 5, downsample = 2, cut_away = 'right', views = NULL);
#'    # Combine the 4 standard views into a single image:
#'    fsbrain::export(shells, view_angles = c('sd_lateral_lh', 'sd_medial_lh', 'sd_lateral_rh', 'sd_medial_rh'),
#'        output_img = 'shells.png');
#' }
#'
#' @export
volvis.shells <- function(volume, levels = NULL, num_levels = 4L, level_type = "quantile",
        level_range = c(0.2, 0.95), frame = 1L, colors = NULL, alphas = NULL,
        palette = "grey_context", alpha_range = NULL, smoothing = 1L, downsample = 1L, cut_away = NULL,
        cut_fraction = 0.5, backend = "auto",
        views = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"),
        rgloptions = rglo(), rglactions = list(), silent = FALSE) {

    volume = shell.volume.data(volume, frame = frame);
    levels = shell.levels(volume, levels = levels, num_levels = num_levels, level_type = level_type, level_range = level_range);
    backend = shell.backend(backend);

    if(! (is.numeric(smoothing) && length(smoothing) == 1L && smoothing >= 0L)) {
        stop("Parameter 'smoothing' must be a non-negative integer, the number of box blur passes applied to the volume before extracting the shells.\n");
    }
    if(! (is.numeric(downsample) && length(downsample) == 1L && downsample >= 1L)) {
        stop("Parameter 'downsample' must be a positive integer, the factor by which the volume is subsampled.\n");
    }
    if(! (is.numeric(cut_fraction) && length(cut_fraction) == 1L && cut_fraction >= 0.0 && cut_fraction <= 1.0)) {
        stop("Parameter 'cut_fraction' must be a single value between 0 and 1.\n");
    }
    if(downsample > 1L && smoothing == 0L) {
        warning(sprintf("The volume is subsampled (downsample = %d) without smoothing (smoothing = 0), the shells will show aliasing artifacts. Consider using smoothing = 1 or larger.\n", as.integer(downsample)));
    }

    # Extract the shells. The volume is smoothed and/or subsampled first if requested, which reduces
    # the number of triangles a lot (the combination of both is much cheaper than decimating the
    # resulting meshes afterwards).
    work_volume = volume;
    if(smoothing > 0L) {
        work_volume = volume.boxblur(work_volume, passes = as.integer(smoothing));
    }
    if(downsample > 1L) {
        work_volume = volume.subsample(work_volume, factor = as.integer(downsample));
    }
    if(! silent) {
        cat(sprintf("Visualizing volume as %d nested iso-surface shell(s), using the '%s' backend.\n", length(levels), backend));
        cat(sprintf(" * volume: %s voxels, levels: %s\n", paste(dim(work_volume), collapse = "x"), paste(round(levels, 3), collapse = ", ")));
    }

    meshes = list();
    for(level_idx in seq_along(levels)) {
        mesh = shell.extract.mesh(work_volume, level = levels[level_idx], backend = backend);
        if(is.null(mesh)) {
            stop(sprintf("The iso-level %g is not within the range of the volume data, no shell was found. Use 'level_range' or the 'levels' parameter to stay within the data range.\n", levels[level_idx]));
        }
        # Transform from voxel space (of the possibly subsampled volume) to surface RAS.
        mesh = apply.transform(mesh, vox2ras_tkr() %*% volume.subsample.matrix(downsample));
        if(! is.null(cut_away)) {
            mesh = shell.cut.away(mesh, cut_away = cut_away, cut_fraction = cut_fraction);
        }
        meshes[[level_idx]] = mesh;
    }

    coloredmeshes = shell.coloredmeshes(meshes, levels = levels, colors = colors, alphas = alphas,
        palette = palette, alpha_range = alpha_range);

    if(! rglactions.has.key(rglactions, 'no_vis')) {
        # The style of each mesh is used, it carries the alpha value of the shell.
        brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, style = "from_mesh");
    }

    return(invisible(coloredmeshes));
}


#' @title Extract the 3D data array of a volume.
#'
#' @description Accepts a 3D array or an `fs.volume` instance, and selects the requested frame of a 4D volume.
#'
#' @param volume a 3D numerical array or an `fs.volume` instance.
#'
#' @param frame positive integer, the frame to use for a 4D volume.
#'
#' @return a 3D numerical array.
#'
#' @keywords internal
shell.volume.data <- function(volume, frame = 1L) {
    if(freesurferformats::is.fs.volume(volume)) {
        volume = volume$data;
    }
    if(! is.array(volume) || ! is.numeric(volume)) {
        stop("Parameter 'volume' must be a numeric array or an 'fs.volume' instance.\n");
    }
    if(length(dim(volume)) == 4L) {
        if(frame == "all") {
            stop("Parameter 'frame' must be a single positive integer for 'volvis.shells'.\n");
        }
        if(! (is.numeric(frame) && length(frame) == 1L && frame >= 1L && frame <= dim(volume)[4])) {
            stop(sprintf("Parameter 'frame' must be a value between 1 and %d for this 4D volume.\n", dim(volume)[4]));
        }
        volume = volume[, , , as.integer(frame)];
    }
    if(length(dim(volume)) != 3L) {
        stop("Parameter 'volume' must have exactly 3 dimensions (or 4 with a valid 'frame').\n");
    }
    return(volume);
}


#' @title Compute the iso-levels of the shells.
#'
#' @param volume a 3D numerical array.
#'
#' @param levels numerical vector or `NULL`, the levels given by the user.
#'
#' @param num_levels positive integer, the number of shells to compute if `levels` is `NULL`.
#'
#' @param level_type character string, 'quantile' or 'linear'.
#'
#' @param level_range numerical vector of length 2, the range from which the levels are taken.
#'
#' @return ascending numerical vector of levels, the outermost shell first.
#'
#' @keywords internal
shell.levels <- function(volume, levels = NULL, num_levels = 5L, level_type = "quantile", level_range = c(0.2, 0.95)) {
    if(! is.null(levels)) {
        if(! (is.numeric(levels) && length(levels) >= 1L)) {
            stop("Parameter 'levels' must be a numerical vector of iso-levels, or NULL to compute them automatically.\n");
        }
        if(any(! is.finite(levels))) {
            stop("Parameter 'levels' must not contain NA or infinite values.\n");
        }
        return(sort(unique(levels)));
    }

    if(! (is.character(level_type) && length(level_type) == 1L && level_type %in% c("quantile", "linear"))) {
        stop("Parameter 'level_type' must be one of 'quantile' or 'linear'.\n");
    }
    if(! (is.numeric(num_levels) && length(num_levels) == 1L && num_levels >= 1L)) {
        stop("Parameter 'num_levels' must be a positive integer.\n");
    }
    if(! (is.numeric(level_range) && length(level_range) == 2L && all(is.finite(level_range)) && level_range[1L] < level_range[2L])) {
        stop("Parameter 'level_range' must be a numerical vector of length 2 with ascending values.\n");
    }
    if(level_type == "quantile" && (level_range[1L] < 0.0 || level_range[2L] > 1.0)) {
        stop("For 'level_type = 'quantile'', the values in 'level_range' must be quantiles, i.e., between 0 and 1.\n");
    }

    foreground = volume[is.finite(volume) & volume != 0];
    if(length(foreground) == 0L) {
        stop("The volume contains no foreground voxels (all values are 0, NA or infinite), so the iso-levels cannot be computed. Pass the levels explicitly via the 'levels' parameter to visualize such a volume.\n");
    }

    if(level_type == "quantile") {
        level_values = as.numeric(stats::quantile(foreground, probs = seq(level_range[1L], level_range[2L], length.out = as.integer(num_levels)), names = FALSE));
    } else {
        value_range = range(foreground);
        level_values = seq(value_range[1L] + level_range[1L] * diff(value_range), value_range[1L] + level_range[2L] * diff(value_range), length.out = as.integer(num_levels));
    }

    level_values = sort(unique(round(level_values, digits = 6L)));
    if(length(level_values) < as.integer(num_levels)) {
        warning(sprintf("Only %d distinct iso-level(s) computed, the volume data range may be too small for %d shells.\n", length(level_values), as.integer(num_levels)));
    }
    return(level_values);
}


#' @title Smooth a volume with a 3x3x3 box blur.
#'
#' @description Applies a separable 3x3x3 box blur to a 3D array, once per requested pass. Voxels at the border of the volume are treated as if the volume were padded with its edge values.
#'
#' @param volume a 3D numerical array.
#'
#' @param passes positive integer, the number of blur passes.
#'
#' @return a 3D numerical array with identical dimensions.
#'
#' @keywords internal
volume.boxblur <- function(volume, passes = 1L) {
    for(pass_idx in seq_len(as.integer(passes))) {
        volume = volume.boxblur.axis(volume, axis = 1L);
        volume = volume.boxblur.axis(volume, axis = 2L);
        volume = volume.boxblur.axis(volume, axis = 3L);
    }
    return(volume);
}


#' @title Apply a 1D 3-element box blur along one axis of a 3D array.
#'
#' @param volume a 3D numerical array.
#'
#' @param axis integer, the axis along which to blur (1, 2 or 3).
#'
#' @return a 3D numerical array with identical dimensions.
#'
#' @keywords internal
volume.boxblur.axis <- function(volume, axis = 1L) {
    axis_dims = dim(volume);
    num = axis_dims[axis];
    lower = pmax(seq_len(num) - 1L, 1L);   # index of the previous voxel, clamped at the border
    upper = pmin(seq_len(num) + 1L, num);  # index of the next voxel, clamped at the border
    sel.index = function(indices) {
        args = lapply(seq_along(axis_dims), function(dim_idx) { if(dim_idx == axis) indices else seq_len(axis_dims[dim_idx]); });
        return(array(volume[args[[1L]], args[[2L]], args[[3L]]], dim = axis_dims));
    };
    return((sel.index(lower) + volume + sel.index(upper)) / 3.0);
}


#' @title Subsample a volume.
#'
#' @param volume a 3D numerical array.
#'
#' @param factor positive integer, the subsampling factor.
#'
#' @return a 3D numerical array, smaller than the input if `factor` is larger than 1.
#'
#' @keywords internal
volume.subsample <- function(volume, factor = 1L) {
    factor = as.integer(factor);
    if(factor <= 1L) {
        return(volume);
    }
    axis_dims = dim(volume);
    return(volume[seq(1L, axis_dims[1L], by = factor), seq(1L, axis_dims[2L], by = factor), seq(1L, axis_dims[3L], by = factor)]);
}


#' @title Compute the transform between the voxel space of a subsampled volume and the original voxel space.
#'
#' @description Subsampling a volume with a factor `f` keeps voxels 1, 1+f, 1+2f, ... of the original volume, so the coordinate `i` of the subsampled volume corresponds to the original voxel coordinate `i*f + (1-f)/2` (e.g., for `f = 2`: 1, 3, 5, ... stay at 1.5, 3.5, 5.5 of the original volume). This function returns the 4x4 affine matrix which implements this mapping, it can be composed with the `vox2ras_tkr` matrix to transform subsampled shells to surface RAS.
#'
#' @param factor positive integer, the subsampling factor.
#'
#' @return a 4x4 numerical matrix.
#'
#' @keywords internal
volume.subsample.matrix <- function(factor = 1L) {
    factor = as.integer(factor);
    shift = (1.0 - factor) / 2.0;
    return(matrix(c(factor, 0, 0, shift,
                    0, factor, 0, shift,
                    0, 0, factor, shift,
                    0, 0, 0, 1), nrow = 4L, byrow = TRUE));
}


#' @title Determine the backend used for iso-surface extraction.
#'
#' @param backend character string, one of 'auto', 'Rvcg' or 'misc3d'.
#'
#' @return character string, either 'Rvcg' or 'misc3d'.
#'
#' @keywords internal
shell.backend <- function(backend = "auto") {
    if(! (is.character(backend) && length(backend) == 1L && backend %in% c("auto", "Rvcg", "misc3d"))) {
        stop("Parameter 'backend' must be one of 'auto', 'Rvcg' or 'misc3d'.\n");
    }
    if(backend == "auto") {
        if(requireNamespace("Rvcg", quietly = TRUE)) {
            return("Rvcg");
        }
        if(requireNamespace("misc3d", quietly = TRUE)) {
            return("misc3d");
        }
        stop("Extracting iso-surfaces requires at least one of the optional packages 'Rvcg' and 'misc3d', but neither is installed.\n");
    }
    if(! requireNamespace(backend, quietly = TRUE)) {
        stop(sprintf("The package '%s' is required for 'backend = \"%s\"', but it is not installed.\n", backend, backend));
    }
    return(backend);
}


#' @title Extract the iso-surface mesh of a volume at one level.
#'
#' @description The mesh is returned in the voxel space of the volume, using 1-based voxel indices (the convention used by the `vox2ras_tkr` matrix and by `misc3d`), no matter which backend is used.
#'
#' @param volume a 3D numerical array, in voxel space.
#'
#' @param level numerical, the iso-level.
#'
#' @param backend character string, 'Rvcg' or 'misc3d'.
#'
#' @return a `mesh3d` instance with normals, in the voxel space of the volume, or `NULL` if the level is not within the range of the data.
#'
#' @keywords internal
shell.extract.mesh <- function(volume, level, backend) {
    if(level < min(volume) || level > max(volume)) {
        return(NULL);
    }
    if(backend == "Rvcg") {
        # VCGLib marching cubes: fast, and it returns a vertex-welded mesh with normals. Note that
        # 'Rvcg' flips the first two axes by default (its 'IJK2RAS' argument) and returns 0-based
        # voxel indices, so we ask for unmodified coordinates and shift them to 1-based indices
        # afterwards. This way, both backends produce meshes in the same coordinate system, and the
        # meshes can be transformed to surface RAS with the vox2ras_tkr matrix.
        mesh = Rvcg::vcgIsosurface(volume, threshold = level, IJK2RAS = diag(4L));
        mesh$vb[1:3, ] = mesh$vb[1:3, , drop = FALSE] + 1.0;
        return(mesh);
    } else {
        # misc3d: returns a 'Triangles3D' instance, which is converted to a mesh. Note that the
        # resulting mesh is not welded, it has one vertex per triangle corner.
        tris = misc3d::contour3d(volume, level = level, draw = FALSE);
        return(Triangles3D.to.coloredmesh(tris, hemi = NULL, add_normals = TRUE)$mesh);
    }
}


#' @title Cut away part of a mesh.
#'
#' @description Removes all faces of a mesh which are (partially) located in the given anatomical direction of a cut plane, so that one can look inside the object. The mesh is not closed at the cut plane.
#'
#' @param mesh a `mesh3d` instance, in surface RAS coordinates.
#'
#' @param cut_away character string, one of 'left', 'right', 'posterior', 'anterior', 'inferior' or 'superior'.
#'
#' @param cut_fraction numerical, the position of the cut plane along the respective axis, as a fraction of the bounding box of the mesh.
#'
#' @return a `mesh3d` instance with the respective faces removed.
#'
#' @keywords internal
shell.cut.away <- function(mesh, cut_away = "right", cut_fraction = 0.5) {
    cut_spec = list("left" = c(1L, -1.0), "right" = c(1L, 1.0), "posterior" = c(2L, -1.0), "anterior" = c(2L, 1.0),
                    "inferior" = c(3L, -1.0), "superior" = c(3L, 1.0));
    if(! (is.character(cut_away) && length(cut_away) == 1L && cut_away %in% names(cut_spec))) {
        stop(sprintf("Parameter 'cut_away' must be NULL or one of: %s.\n", paste(names(cut_spec), collapse = ", ")));
    }

    axis = cut_spec[[cut_away]][1L];
    direction = cut_spec[[cut_away]][2L];

    vertices = t(mesh$vb[1:3, , drop = FALSE]);
    coords = vertices[, axis];
    cut_position = min(coords) + cut_fraction * diff(range(coords));

    # Keep the vertices which are not in the direction that is cut away.
    keep_vertex = (direction * coords) <= (direction * cut_position);
    faces = if(! is.null(mesh$it)) mesh$it else NULL;
    if(is.null(faces) || ncol(faces) == 0L) {
        return(mesh);
    }

    # Remove all faces which use a vertex from the cut away part, then drop the vertices that are no
    # longer used by any face and renumber the face indices.
    keep_face = keep_vertex[faces[1L, ]] & keep_vertex[faces[2L, ]] & keep_vertex[faces[3L, ]];
    faces = faces[, keep_face, drop = FALSE];
    if(ncol(faces) == 0L) {
        stop(sprintf("Cutting away the '%s' part of the mesh at fraction %g removed all faces, there is nothing left to render. Use a smaller 'cut_fraction'.\n", cut_away, cut_fraction));
    }

    used_vertices = sort(unique(as.vector(faces)));
    mesh$vb = mesh$vb[, used_vertices, drop = FALSE];
    if(! is.null(mesh$normals) && ncol(mesh$normals) == length(keep_vertex)) {
        mesh$normals = mesh$normals[, used_vertices, drop = FALSE];
    }
    mesh$it = matrix(match(as.vector(faces), used_vertices), nrow = nrow(faces));
    return(mesh);
}


#' @title Compute the colors and alpha values of the shells.
#'
#' @description The palette defines how the shells look: their colors and how transparent they are, from the outermost to the innermost shell. Three palettes are available:
#' \itemize{
#'   \item 'grey_context' (the default): all shells but the innermost one are grey (like the semi-transparent cortex in \code{\link[fsbrain]{vis.subcortical.region.values}} is), and the innermost shell is rendered in a warm color and fully opaque. This gives the impression of looking at a structure inside a translucent head, and it makes a single feature stand out, at the price of hiding the level of the inner shells.
#'   \item 'sequential': a single hue, getting lighter and more opaque towards the innermost shell. Useful to emphasize the nested structure, and to avoid the colorful look of 'viridis'.
#'   \item 'viridis': the full viridis ramp, with the outer shells dark and very transparent and the inner ones bright. This is the most colorful option, and it encodes the iso-level in the color.
#' }
#'
#' @param palette character string, the name of the palette: one of 'grey_context', 'sequential' or 'viridis'.
#'
#' @param num_shells positive integer, the number of shells.
#'
#' @param alpha_range numerical vector of length 2 or `NULL`. The alpha value of the outermost and of the innermost (non-opaque) shell. If `NULL`, the default range of the palette is used.
#'
#' @return a named list with the entries 'colors' (character vector, one color per shell) and 'alphas' (numerical vector, one alpha value per shell).
#'
#' @keywords internal
shell.palette <- function(palette = "grey_context", num_shells = 4L, alpha_range = NULL) {
    if(! (is.character(palette) && length(palette) == 1L && palette %in% c("grey_context", "sequential", "viridis"))) {
        stop(sprintf("Parameter 'palette' must be one of: %s.\n", paste(sQuote(c("grey_context", "sequential", "viridis")), collapse = ", ")));
    }
    if(! (is.numeric(num_shells) && length(num_shells) == 1L && num_shells >= 1L)) {
        stop("Parameter 'num_shells' must be a positive integer.\n");
    }
    if(! is.null(alpha_range) && ! (is.numeric(alpha_range) && length(alpha_range) == 2L && all(is.finite(alpha_range)) && alpha_range[1L] <= alpha_range[2L] && alpha_range[1L] >= 0.0 && alpha_range[2L] <= 1.0)) {
        stop("Parameter 'alpha_range' must be NULL or a numerical vector of length 2 with ascending values between 0 and 1.\n");
    }
    num_shells = as.integer(num_shells);

    if(palette == "viridis") {
        colors = viridis::viridis(num_shells);
        bounds = if(is.null(alpha_range)) c(0.1, 0.8) else alpha_range;
        alphas = seq(bounds[1L], bounds[2L], length.out = num_shells);
    } else if(palette == "sequential") {
        colors = grDevices::colorRampPalette(c("#08519C", "#9ECAE1"))(num_shells);
        bounds = if(is.null(alpha_range)) c(0.06, 0.95) else alpha_range;
        alphas = seq(bounds[1L], bounds[2L], length.out = num_shells);
    } else {   # 'grey_context'
        colors = c(rep("#9E9E9E", max(num_shells - 1L, 0L)), "#D94801");
        bounds = if(is.null(alpha_range)) c(0.05, 0.30) else alpha_range;
        # The innermost shell is the actual structure of interest, so it is fully opaque.
        alphas = c(seq(bounds[1L], bounds[2L], length.out = max(num_shells - 1L, 0L)), 1.0);
    }

    return(list("colors" = colors, "alphas" = alphas));
}


#' @title Create the coloredmeshes of the shells.
#'
#' @description Creates one `fs.coloredmesh` per shell, each with its own color and alpha value stored in the mesh style.
#'
#' @param meshes list of `mesh3d` instances, one per shell.
#'
#' @param levels numerical vector of the iso-levels, ascending.
#'
#' @param colors character vector of colors or `NULL` to use the colors of the `palette`.
#'
#' @param alphas numerical vector of alpha values or `NULL` to use the alphas of the `palette`.
#'
#' @param palette character string, the name of the palette, see \code{\link[fsbrain]{shell.palette}}.
#'
#' @param alpha_range numerical vector of length 2 or `NULL`, passed to \code{\link[fsbrain]{shell.palette}}.
#'
#' @return list of `fs.coloredmesh` instances, one per shell.
#'
#' @keywords internal
shell.coloredmeshes <- function(meshes, levels, colors = NULL, alphas = NULL, palette = "grey_context", alpha_range = NULL) {
    num_shells = length(meshes);
    palette_definition = shell.palette(palette, num_shells = num_shells, alpha_range = alpha_range);
    if(is.null(colors)) {
        colors = palette_definition$colors;
    }
    if(length(colors) != num_shells) {
        stop(sprintf("Parameter 'colors' must contain exactly one color per shell (%d), but %d colors were given.\n", num_shells, length(colors)));
    }
    if(is.null(alphas)) {
        alphas = palette_definition$alphas;
    }
    if(length(alphas) != num_shells || any(alphas < 0.0 | alphas > 1.0)) {
        stop(sprintf("Parameter 'alphas' must contain exactly one alpha value between 0 and 1 per shell (%d).\n", num_shells));
    }

    coloredmeshes = list();
    for(shell_idx in seq_len(num_shells)) {
        cmesh = fs.coloredmesh(meshes[[shell_idx]], colors[shell_idx], hemi = NULL, render = TRUE, add_normals = FALSE);
        # Backfaces are not culled on purpose: with culled backfaces, the rgl renderer composites
        # nested transparent meshes in the wrong order (the innermost shell on top of the others).
        # The material is matte (no specular highlights), which keeps the rendering readable.
        cmesh$style = list("alpha" = alphas[shell_idx], "lit" = TRUE, "back" = "filled",
                           "shininess" = 0, "specular" = "black");
        coloredmeshes[[shell_idx]] = cmesh;
    }
    return(coloredmeshes);
}
