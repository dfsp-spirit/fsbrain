# Functions to project volume data onto brain surfaces ('vol2surf').


# ══════════════════════════════════════════════════════════════════════════════
# Internal helpers: volume loading
# ══════════════════════════════════════════════════════════════════════════════


#' @title Compute the filepath of a volume file.
#'
#' @description Takes a volume specification (a filepath, a name without extension, or an in-memory volume) and computes the filepath of the volume file on disk.
#'
#' @param volume character string or named list. If a character string and the file exists, it is interpreted as the volume filepath. If it does not exist, it is interpreted as the volume name without extension, resolved against `default_dir` (and the current working directory) by trying the extensions `.mgz`, `.mgh`, `.nii.gz`, and `.nii` (in that order). If a named list, it is interpreted as an in-memory volume (see \code{\link[fsbrain]{subject.vol2surf}}) and `NULL` is returned.
#'
#' @param default_dir character string or NULL, the directory to resolve volume names against, typically the `mri` directory of a subject.
#'
#' @return character string, the volume filepath, or `NULL` if `volume` is an in-memory volume.
#'
#' @keywords internal
vol.find.file <- function(volume, default_dir = NULL) {
    if(is.list(volume)) {
        return(NULL);
    }
    if(! is.character(volume) || length(volume) != 1L) {
        stop("Parameter 'volume' must be a character string or a named list with entries 'data' and 'affine'.\n");
    }

    if(file.exists(volume)) {
        return(volume);
    }

    base = volume;
    if(! is.null(default_dir)) {
        base = file.path(default_dir, volume);
    }

    for(suffix in c("", ".mgz", ".mgh", ".nii.gz", ".nii")) {
        candidate = paste0(base, suffix);
        if(file.exists(candidate)) {
            return(candidate);
        }
    }

    stop(sprintf("Cannot find the volume file for '%s'%s. Tried the extensions '.mgz', '.mgh', '.nii.gz' and '.nii'.\n", volume,
        if(is.null(default_dir)) "" else sprintf(" in directory '%s'", default_dir)));
}


#' @title Load a volume file and return the data along with the affine transformation.
#'
#' @description Supports MGH/MGZ volumes (read with \code{\link[freesurferformats]{read.fs.mgh}}) and NIfTI v1 volumes, including oblique ones (read with \code{\link[freesurferformats]{read.fs.volume.nii}}, which requires the 'oro.nifti' package). The data array is returned in the storage order of the file, i.e., it is *not* reoriented, and the returned affine maps **0-based** voxel indices (column, row, slice) to world coordinates. This is the convention used by the NIfTI `sform` and by \code{\link[freesurferformats]{mghheader.vox2ras}}.
#'
#' @param filepath character string, the path to an `.mgz`, `.mgh`, `.nii` or `.nii.gz` file.
#'
#' @return named list with entries: 'data' (numeric array, 3D or 4D), 'affine' (4x4 numeric matrix), 'filepath' (character string), 'file_format' (character string, either 'mgh' or 'nifti'), and 'frames' (integer, the number of volumes in a 4D file).
#'
#' @note If the header of an MGH/MGZ file does not contain a valid `vox2ras` matrix (`ras_good_flag` is 0), the surface RAS convention \code{\link[fsbrain]{vox2ras_tkr}} is used for conformed volumes (dimension 256x256x256) and a warning is emitted. For NIfTI files without a valid sform/qform, an error is raised, as the coordinate space is unknown in that case.
#'
#' @keywords internal
vol.read.file.with.affine <- function(filepath) {
    filepath_lower = tolower(filepath);

    if(endsWith(filepath_lower, ".nii") || endsWith(filepath_lower, ".nii.gz")) {
        if(! requireNamespace("oro.nifti", quietly = TRUE)) {
            stop("Reading NIfTI files requires the 'oro.nifti' package, please install it.\n");
        }
        # 'reorient = FALSE' is required: the data must be returned in the storage order of the file, matching the affine.
        vol = freesurferformats::read.fs.volume.nii(filepath, with_header = TRUE, reorient = FALSE);
        affine = NULL;
        if(! is.null(vol$header$ras_good_flag) && vol$header$ras_good_flag >= 1L) {
            affine = freesurferformats::mghheader.vox2ras(vol$header);
        }
        if(is.null(affine)) {
            stop(sprintf("The NIfTI file '%s' does not contain a valid sform or qform, so its coordinate space is unknown. Cannot project it onto a surface.\n", filepath));
        }
        return(list("data" = vol$data, "affine" = affine, "filepath" = filepath, "file_format" = "nifti"));
    }

    if(endsWith(filepath_lower, ".mgz") || endsWith(filepath_lower, ".mgh")) {
        vol = freesurferformats::read.fs.mgh(filepath, with_header = TRUE, drop_empty_dims = FALSE);
        affine = vol$header$vox2ras_matrix;
        if(is.null(affine)) {
            if(all(dim(vol$data)[1:3] == 256L)) {
                warning(sprintf("The MGH/MGZ file '%s' does not contain a valid vox2ras matrix (ras_good_flag is 0). Assuming the standard FreeSurfer surface RAS convention for conformed volumes (vox2ras_tkr).\n", filepath));
                affine = vox2ras_tkr();
            } else {
                stop(sprintf("The MGH/MGZ file '%s' does not contain a valid vox2ras matrix (ras_good_flag is 0) and does not have the dimensions of a conformed volume (256x256x256), so its coordinate space is unknown. Cannot project it onto a surface.\n", filepath));
            }
        }
        return(list("data" = vol$data, "affine" = affine, "filepath" = filepath, "file_format" = "mgh"));
    }

    stop(sprintf("Unsupported volume file format for file '%s'. Supported formats are MGH, MGZ, NIfTI1 ('.nii') and gzipped NIfTI1 ('.nii.gz').\n", filepath));
}


#' @title Transform a volume affine matrix into the FreeSurfer tkregister convention.
#'
#' @description FreeSurfer stores brain surfaces (and all surface-based data) in the *surface RAS* coordinate system, which is also called the *tkregister* or *tkreg* space: the RAS coordinate (0, 0, 0) is at the center of the field of view of the volume. The transformation from voxel indices to this space is **not** the transformation stored in the header of the volume: a volume can have a center-of-RAS offset (`cras`), which is preserved by FreeSurfer in the header but ignored by the surface-based tools (this is the difference between the `--vox2ras` and `--vox2ras-tkr` options of `mri_vol2surf`).
#'
#'   This function takes the header transformation matrix of a volume and returns the corresponding tkregister matrix, i.e., the same direction cosines and voxel sizes, but with the translation replaced so that the center of the volume is at RAS (0, 0, 0). For a conformed FreeSurfer volume (256x256x256 voxels of 1 mm), the result is identical to \code{\link[fsbrain]{vox2ras_tkr}}.
#'
#' @param affine numeric 4x4 matrix, the transformation matrix from the volume header (maps 0-based voxel indices to world coordinates).
#'
#' @param dims integer vector of length 3 or more, the dimensions of the volume (`dim(volume_data)`).
#'
#' @return numeric 4x4 matrix, the tkregister transformation matrix.
#'
#' @keywords internal
vol.tkreg.affine <- function(affine, dims) {
    if(! is.matrix(affine) || nrow(affine) != 4L || ncol(affine) != 4L) {
        stop("Parameter 'affine' must be a 4x4 numeric matrix.\n");
    }
    if(length(dims) < 3L) {
        stop("Parameter 'dims' must have at least 3 entries.\n");
    }
    tkreg = affine;
    # The center of the volume in 0-based CRS indices:
    crs_center = as.numeric(dims[1:3]) / 2.0;
    tkreg[1:3, 4] = -1.0 * (affine[1:3, 1:3, drop = FALSE] %*% crs_center);
    return(tkreg);
}


#' @title Load a volume (from file or memory) and return the data along with the affine transformation.
#'
#' @param volume character string or named list. If a string, either a volume filepath or the name of a volume file without extension. If a named list, an in-memory volume with entries 'data' (numeric 3D or 4D array) and 'affine' (4x4 numeric matrix mapping 0-based voxel indices to world coordinates).
#'
#' @param default_dir character string or NULL, the directory used to resolve volumes specified as names without extension.
#'
#' @param affine numeric 4x4 matrix or `NULL`. If given, it is used instead of the transformation matrix from the volume header. This is rarely needed, but can help if the header of a volume is missing or wrong. See `vox2ras` for the interpretation of a volume whose header does not match the surfaces.
#'
#' @param vox2ras character string, how to compute the transformation from voxel indices to world coordinates for the volume: 'auto' (the default) uses \code{\link[fsbrain]{vol.tkreg.affine}} (the FreeSurfer tkregister convention, in which the surfaces are defined) for MGH/MGZ volumes, and the transformation matrix from the file header for NIfTI volumes. 'tkr' forces the tkregister convention, 'header' forces the matrix from the volume header. This corresponds to the `--vox2ras-tkr` and `--vox2ras` options of `mri_vol2surf`. Ignored if `affine` is given.
#'
#' @return named list with entries 'data', 'affine' and 'filepath' (see \code{\link[fsbrain]{vol.read.file.with.affine}}).
#'
#' @keywords internal
vol.load <- function(volume, default_dir = NULL, affine = NULL, vox2ras = "auto") {
    if(! is.null(affine)) {
        if(! is.matrix(affine) || nrow(affine) != 4L || ncol(affine) != 4L || ! is.numeric(affine)) {
            stop("Parameter 'affine' must be NULL or a numeric 4x4 matrix.\n");
        }
    }
    if(! (vox2ras %in% c("auto", "tkr", "header"))) {
        stop(sprintf("Parameter 'vox2ras' must be one of 'auto', 'tkr' or 'header', but is '%s'.\n", vox2ras));
    }

    if(is.list(volume)) {
        vol = volume;
        if(is.null(vol$data)) {
            stop("An in-memory volume must be a named list with entries 'data' and 'affine'.\n");
        }
        if(is.null(vol$affine) && is.null(affine)) {
            stop("An in-memory volume must be a named list with entries 'data' and 'affine'.\n");
        }
        if(! is.null(affine)) {
            vol$affine = affine;
        }
        return(list("data" = vol$data, "affine" = vol$affine, "filepath" = NULL, "file_format" = "memory"));
    }

    filepath = vol.find.file(volume, default_dir = default_dir);
    vol = vol.read.file.with.affine(filepath);

    if(! is.null(affine)) {
        vol$affine = affine;
    } else if(vox2ras == "tkr" || (vox2ras == "auto" && vol$file_format == "mgh")) {
        # FreeSurfer brain surfaces are defined in the surface RAS (= tkregister) space of the volume,
        # which differs from the header matrix if the volume has a center-of-RAS offset.
        vol$affine = vol.tkreg.affine(vol$affine, dim(vol$data));
    }
    return(vol);
}


#' @title Select a single frame (3D volume) from a volume array.
#'
#' @param volume_data numeric array, 3D or 4D.
#'
#' @param frame integer scalar, the frame index (1-based).
#'
#' @return numeric 3D array.
#'
#' @keywords internal
vol.select.frame <- function(volume_data, frame = 1L) {
    dims = dim(volume_data);
    if(length(dims) < 3L) {
        stop(sprintf("The volume data must have at least 3 dimensions, but it has %d.\n", length(dims)));
    }
    num_frames = if(length(dims) < 4L) 1L else dims[4];
    if(! is.numeric(frame) || length(frame) != 1L || frame < 1L || frame > num_frames) {
        stop(sprintf("Parameter 'frame' must be a single integer in range 1..%d, but is '%s'.\n", num_frames, paste(frame, collapse = ", ")));
    }
    if(length(dims) == 3L) {
        return(volume_data);
    }
    return(volume_data[, , , frame, drop = TRUE]);
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal helpers: volume sampling
# ══════════════════════════════════════════════════════════════════════════════


#' @title Sample a 3D volume at the given voxel coordinates using trilinear interpolation.
#'
#' @param volume_data numeric 3D array, the volume data.
#'
#' @param coords numeric matrix with 3 columns and one row per query point, the **0-based** voxel coordinates (column, row, slice). Coordinates must be within the volume (see \code{\link[fsbrain]{vol.clamp.coords}}), or an error will be raised.
#'
#' @return numeric vector, one interpolated value per query point.
#'
#' @note Coordinates which are exactly on the last voxel result in the value of that voxel: the interpolation weights of the out-of-range upper neighbors become 0, and their index is clamped (which is safe because their weight is 0).
#'
#' @keywords internal
vol.sample.trilinear <- function(volume_data, coords) {
    dims = dim(volume_data);
    num_points = nrow(coords);

    idx_low = floor(coords);
    frac = coords - idx_low;
    idx_low = idx_low + 1L;    # the data array uses 1-based R indices.
    idx_high = cbind(pmin(idx_low[, 1] + 1L, dims[1]), pmin(idx_low[, 2] + 1L, dims[2]), pmin(idx_low[, 3] + 1L, dims[3]));

    result = numeric(num_points);
    for(ax in 0:1) {
        ii = if(ax == 0L) idx_low[, 1] else idx_high[, 1];
        wi = if(ax == 0L) 1.0 - frac[, 1] else frac[, 1];
        for(ay in 0:1) {
            jj = if(ay == 0L) idx_low[, 2] else idx_high[, 2];
            wj = if(ay == 0L) 1.0 - frac[, 2] else frac[, 2];
            for(az in 0:1) {
                kk = if(az == 0L) idx_low[, 3] else idx_high[, 3];
                wk = if(az == 0L) 1.0 - frac[, 3] else frac[, 3];
                result = result + (wi * wj * wk) * volume_data[cbind(ii, jj, kk)];
            }
        }
    }
    return(result);
}


#' @title Sample a 3D volume at the given voxel coordinates using nearest neighbor interpolation.
#'
#' @param volume_data numeric 3D array, the volume data.
#'
#' @param coords numeric matrix with 3 columns and one row per query point, the **0-based** voxel coordinates. Coordinates must be within the volume.
#'
#' @return numeric vector, one value per query point.
#'
#' @note A coordinate which is exactly between two voxels is resolved towards the larger voxel index, i.e., `floor(coord + 0.5)` is used. This matches the behavior of `scipy.ndimage.map_coordinates(..., order = 0)`. Note that R's `round` uses banker's rounding and would give different results for such coordinates.
#'
#' @keywords internal
vol.sample.nearest <- function(volume_data, coords) {
    dims = dim(volume_data);
    idx = floor(coords + 0.5);
    idx[, 1] = pmin(pmax(idx[, 1], 0), dims[1] - 1);
    idx[, 2] = pmin(pmax(idx[, 2], 0), dims[2] - 1);
    idx[, 3] = pmin(pmax(idx[, 3], 0), dims[3] - 1);
    return(volume_data[cbind(idx[, 1] + 1L, idx[, 2] + 1L, idx[, 3] + 1L)]);
}


#' @title Sample a 3D volume at world coordinates.
#'
#' @description This is the core of the volume-to-surface projection: the world coordinates (e.g., surface vertex positions) are transformed into voxel coordinates using the inverse of the affine matrix, and the volume is then sampled at those coordinates.
#'
#' @param volume_data numeric 3D array, the volume data.
#'
#' @param affine numeric 4x4 matrix, maps 0-based voxel indices (column, row, slice) to world coordinates.
#'
#' @param coords numeric matrix with 3 columns and one row per query point, the world coordinates at which to sample the volume.
#'
#' @param interpolation character string, one of 'trilinear' (or its alias 'linear') or 'nearest'.
#'
#' @param clamp logical, whether to clamp query points which are outside the volume to the closest voxel at the volume border (edge replication). If FALSE, such points get the value NA.
#'
#' @param check_outside logical, whether to determine which of the query points are outside the volume. Setting this to FALSE saves the (small) cost of the comparison, but the caller cannot report or mask such points.
#'
#' @return named list with entries 'values' (numeric vector) and 'outside' (logical vector or NULL if `check_outside` is FALSE).
#'
#' @keywords internal
vol.sample.at.coords <- function(volume_data, affine, coords, interpolation = "trilinear", clamp = FALSE, check_outside = TRUE) {
    if(length(dim(volume_data)) != 3L) {
        stop(sprintf("Parameter 'volume_data' must be a 3D array, but it has %d dimensions. Use 'vol.select.frame' to select a single frame of a 4D volume.\n", length(dim(volume_data))));
    }
    if(! is.matrix(affine) || nrow(affine) != 4L || ncol(affine) != 4L) {
        stop("Parameter 'affine' must be a 4x4 numeric matrix.\n");
    }
    if(is.null(dim(coords))) {
        coords = matrix(coords, ncol = 3L);
    }
    if(ncol(coords) != 3L) {
        stop(sprintf("Parameter 'coords' must have 3 columns (x, y, z), but it has %d.\n", ncol(coords)));
    }

    dims = dim(volume_data);
    inv_affine = solve(affine);
    coords_vox = t(inv_affine %*% t(cbind(coords, 1.0)))[, 1:3, drop = FALSE];

    outside = NULL;
    if(check_outside) {
        tol = 1e-9;
        outside = (coords_vox[, 1] < -tol) | (coords_vox[, 1] > (dims[1] - 1L) + tol) |
                  (coords_vox[, 2] < -tol) | (coords_vox[, 2] > (dims[2] - 1L) + tol) |
                  (coords_vox[, 3] < -tol) | (coords_vox[, 3] > (dims[3] - 1L) + tol);
    }

    # Clamp for the array access, so that we never index outside the array. If 'clamp' is FALSE, the
    # values of the clamped points are set to NA below.
    coords_vox[, 1] = pmin(pmax(coords_vox[, 1], 0.0), dims[1] - 1L);
    coords_vox[, 2] = pmin(pmax(coords_vox[, 2], 0.0), dims[2] - 1L);
    coords_vox[, 3] = pmin(pmax(coords_vox[, 3], 0.0), dims[3] - 1L);

    if(interpolation == "nearest") {
        values = vol.sample.nearest(volume_data, coords_vox);
    } else {
        values = vol.sample.trilinear(volume_data, coords_vox);
    }

    if(! is.null(outside) && ! clamp) {
        values[outside] = NA_real_;
    }

    return(list("values" = values, "outside" = outside));
}


#' @title Compute the vertex positions at a given fraction between two surfaces.
#'
#' @description Computes a new mesh whose vertex positions lie on the line between the corresponding vertices of two meshes with identical vertex counts, e.g., between the white and the pial surface. This is what the FreeSurfer command line option `--surf-frac` does.
#'
#' @param surface_a an `fs.surface` instance, the surface at fraction 0.
#'
#' @param surface_b an `fs.surface` instance, the surface at fraction 1. Must have the same number of vertices as `surface_a`.
#'
#' @param frac numeric scalar, the fraction, in range 0..1.
#'
#' @return numeric matrix of vertex coordinates (see `surface_a$vertices`).
#'
#' @keywords internal
surface.interpolate.frac <- function(surface_a, surface_b, frac) {
    if(nrow(surface_a$vertices) != nrow(surface_b$vertices)) {
        stop(sprintf("Cannot interpolate between surfaces with different vertex counts (%d vs %d).\n", nrow(surface_a$vertices), nrow(surface_b$vertices)));
    }
    return(surface_a$vertices + frac * (surface_b$vertices - surface_a$vertices));
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal helpers: shared logic of the vol2surf functions
# ══════════════════════════════════════════════════════════════════════════════


#' @title Check and normalize the interpolation parameter.
#'
#' @param interpolation character string, the interpolation method.
#'
#' @return character string, one of 'trilinear' or 'nearest'.
#'
#' @keywords internal
vol.check.interpolation <- function(interpolation = "trilinear") {
    interpolation = tolower(interpolation);
    if(interpolation == "linear") {
        interpolation = "trilinear";    # alias, for people coming from yabplot/nilearn.
    }
    if(! (interpolation %in% c("trilinear", "nearest"))) {
        stop(sprintf("Parameter 'interpolation' must be one of 'trilinear', 'linear' or 'nearest', but is '%s'.\n", interpolation));
    }
    return(interpolation);
}


#' @title Check and normalize the surface_frac parameters.
#'
#' @param surface_frac numeric scalar or NULL, the fraction between two surfaces.
#'
#' @keywords internal
vol.check.surface.frac <- function(surface_frac) {
    if(! is.null(surface_frac)) {
        if(! is.numeric(surface_frac) || length(surface_frac) != 1L || is.na(surface_frac) || surface_frac < 0.0 || surface_frac > 1.0) {
            stop(sprintf("Parameter 'surface_frac' must be a single number in range 0..1 or NULL, but is '%s'.\n", paste(surface_frac, collapse = ", ")));
        }
    }
}


#' @title Compute the vertex coordinates of a (possibly interpolated) surface for one hemisphere.
#'
#' @param surface an `fs.surface` instance, the surface to sample, or `NULL` if `surface_frac` is given.
#'
#' @param frac_surface an `fs.surface` instance or `NULL`, the second surface for the interpolation.
#'
#' @param surface_frac numeric scalar or `NULL`, the fraction from `surface` to `frac_surface`.
#'
#' @return numeric matrix, the vertex coordinates.
#'
#' @keywords internal
vol.frac.vertices <- function(surface, frac_surface, surface_frac) {
    if(is.null(surface_frac)) {
        return(surface$vertices);
    }
    return(surface.interpolate.frac(surface, frac_surface, surface_frac));
}


#' @title Warn about surface vertices which are outside the volume.
#'
#' @param outside named list of logical vectors, per hemisphere (or NULL entries if the check was disabled).
#'
#' @param num_verts named list of integer scalars, the number of vertices per hemisphere.
#'
#' @param clamp logical, whether outside vertices were clamped to the volume border instead of being set to NA.
#'
#' @return NULL, called for the side effect of emitting a warning.
#'
#' @keywords internal
vol.warn.outside <- function(outside, num_verts, clamp) {
    msgs = character(0);
    for(hemi in names(outside)) {
        if(! is.null(outside[[hemi]]) && any(outside[[hemi]])) {
            num_outside = sum(outside[[hemi]]);
            action = if(clamp) "set to the value of the nearest voxel at the volume border" else "set to NA";
            msgs = c(msgs, sprintf("* %d of %d '%s' vertices (%.2f%%) are outside the field of view of the volume. Their values have been %s.",
                num_outside, num_verts[[hemi]], hemi, 100.0 * num_outside / num_verts[[hemi]], action));
        }
    }
    if(length(msgs) > 0L) {
        warning(sprintf("Some surface vertices are outside the volume:\n%s\nThis can indicate that the volume and the surface are not defined in the same coordinate space, e.g., when projecting an MNI152 volume onto an MNI305 template (fsaverage). Please check that the volume and the surface belong to the same space (see the 'template' parameter).\n",
            paste(msgs, collapse = "\n")));
    }
    return(invisible(NULL));
}


#' @title Project a volume onto the vertices of the given surfaces (internal workhorse).
#'
#' @param surfaces named list of vertex coordinate matrices, per hemisphere.
#'
#' @param volume the volume, as accepted by \code{\link[fsbrain]{vol.load}}.
#'
#' @param default_dir character string or NULL, the directory used to resolve volume names without extension.
#'
#' @param interpolation character string, see \code{\link[fsbrain]{subject.vol2surf}}.
#'
#' @param frame integer scalar, see \code{\link[fsbrain]{subject.vol2surf}}.
#'
#' @param clamp logical, whether to clamp query points outside the volume to the volume border.
#'
#' @param check_fov logical, whether to check (and warn about) vertices outside the volume.
#'
#' @param affine numeric 4x4 matrix or `NULL`, used instead of the affine matrix from the volume header if given.
#'
#' @param vox2ras character string, see \code{\link[fsbrain]{subject.vol2surf}}.
#'
#' @return named list of numeric vectors (one per hemisphere), with attribute 'outside' (named list of logical vectors or NULLs).
#'
#' @keywords internal
vol.vol2surf.hemilist <- function(surfaces, volume, default_dir, interpolation, frame, clamp, check_fov, affine = NULL, vox2ras = "auto") {
    interpolation = vol.check.interpolation(interpolation);
    vol_data = vol.load(volume, default_dir = default_dir, affine = affine, vox2ras = vox2ras);
    volume_data = vol.select.frame(vol_data$data, frame = frame);

    ret = list();
    outside = list();
    for(hemi in names(surfaces)) {
        res = vol.sample.at.coords(volume_data, vol_data$affine, surfaces[[hemi]], interpolation = interpolation,
            clamp = clamp, check_outside = check_fov || ! clamp);
        ret[[hemi]] = res$values;
        outside[[hemi]] = res$outside;
    }
    attr(ret, "outside") = outside;
    attr(ret, "volume_filepath") = vol_data$filepath;
    return(ret);
}


#' @title Apply the cortex mask to projected data.
#'
#' @param data named list of numeric vectors, per hemisphere.
#'
#' @param subjects_dir character string, the subjects dir holding the label files.
#'
#' @param subject_id character string, the subject/template identifier.
#'
#' @param cortex_label character string, the name of the cortex label file (or the loaded label data).
#'
#' @return named list of numeric vectors, with medial wall values set to NA.
#'
#' @keywords internal
vol.apply.cortex.mask <- function(data, subjects_dir, subject_id, cortex_label = "cortex") {
    for(hemi in names(data)) {
        data[[hemi]] = apply.label.to.morphdata(data[[hemi]], subjects_dir, subject_id, hemi, label = cortex_label, masked_data_value = NA_real_);
    }
    return(data);
}


#' @title Resolve the subjects dir which contains a template subject.
#'
#' @description Searches for a directory which contains the surface files of the given template subject, first in the user-supplied directory (if any), then in the fsbrain package data cache, and finally in the FreeSurfer installation. Stops with an error explaining how to obtain the missing data if the template cannot be found.
#'
#' @param template character string, the template subject identifier, e.g., 'fsaverage' or 'fs_LR_32'.
#'
#' @param subjects_dir character string or NULL, a user-supplied subjects dir which is checked first.
#'
#' @return character string, the subjects dir which contains the template subject.
#'
#' @keywords internal
resolve.template.subjects.dir <- function(template, subjects_dir = NULL) {

    if(! is.null(subjects_dir)) {
        if(! dir.exists(file.path(subjects_dir, template, "surf"))) {
            stop(sprintf("The directory '%s' does not contain the template subject '%s': no directory '%s' found.\n", subjects_dir, template, file.path(subjects_dir, template, "surf")));
        }
        return(subjects_dir);
    }

    cache_subjects_dir = get_optional_data_filepath("subjects_dir", mustWork = FALSE);
    if(nchar(cache_subjects_dir) > 0L && dir.exists(file.path(cache_subjects_dir, template, "surf"))) {
        return(cache_subjects_dir);
    }

    search_res = find.subjectsdir.of(subject_id = template, mustWork = FALSE);
    if(isTRUE(search_res$found)) {
        return(search_res$found_at);
    }

    if(template == "fs_LR_32") {
        hint = "You can download the fs_LR_32 meshes with fsbrain::download_fs_LR_32_meshes().";
    } else if(template == "fsaverage") {
        hint = "The fsaverage subject is part of a FreeSurfer installation. Alternatively, you can download it with fsbrain::download_fsaverage().";
    } else {
        hint = "Please pass the 'subjects_dir' which contains the template subject.";
    }
    stop(sprintf("Cannot find the surface files for template subject '%s'. %s\n", template, hint));
}


# ══════════════════════════════════════════════════════════════════════════════
# Exported functions
# ══════════════════════════════════════════════════════════════════════════════


#' @title Project a volume onto the cortical surface of a subject using its own (native) space.
#'
#' @description Samples a brain volume at the positions of the cortical surface vertices of a subject and returns the resulting per-vertex values. This is the equivalent of the FreeSurfer command line tool \code{mri_vol2surf} (or of the `project_vol2surf` function of the Python package `yabplot`), implemented in R. The volume and the surface must be defined in the same coordinate space, which is the case for the volumes in the `mri` directory of a subject (e.g., `brain.mgz`) and the surfaces in its `surf` directory (e.g., `lh.white`) -- both are in the subject's native space.
#'
#'   The volume is sampled at the vertex positions using trilinear interpolation (default) or nearest neighbor interpolation. Vertices which fall outside the volume (e.g., medial wall vertices at the bottom of a `brainmask` volume, or vertices outside a truncated field of view) get the value NA by default, and a warning is emitted. Use `clamp = TRUE` to instead use the value of the closest voxel at the volume border, and `check_fov = FALSE` to suppress the check.
#'
#'   To project a group-level statistical map (e.g., in MNI space) onto a template like `fsaverage`, see \code{\link[fsbrain]{template.vol2surf}}.
#'
#' @param subjects_dir character string. The FreeSurfer `SUBJECTS_DIR`, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id character string. The subject identifier.
#'
#' @param volume character string or named list. A volume filepath, the name of a volume file in the `mri` directory of the subject without file extension (e.g., `brain` or `aseg`), or an in-memory volume given as a named list with entries 'data' (numeric 3D or 4D array) and 'affine' (4x4 numeric matrix mapping **0-based** voxel indices to world coordinates). Examples for the latter are \code{\link[freesurferformats]{read.fs.mgh}} (use `with_header = TRUE` and pass `list(data = vol$data, affine = vol$header$vox2ras_matrix)`) and `nibabel` in Python.
#'
#' @param surface character string, the name of the surface to sample. Examples: 'white', 'pial', 'midthickness'. The corresponding `surf/?h.<surface>` files must exist for the subject.
#'
#' @param hemi character string, one of 'lh', 'rh' or 'both'. The hemisphere to project to.
#'
#' @param surface_frac numeric scalar in range 0..1, or `NULL` (the default). If given, the volume is not sampled at the vertices of `surface`, but at the vertices of the surface which lies at the given fraction on the line between the corresponding vertices of `surface` (fraction 0) and `frac_surface` (fraction 1). E.g., with `surface = "white"`, `frac_surface = "pial"` and `surface_frac = 0.5`, the mid-cortical surface is used. This is what the `--surf-frac` option of `mri_vol2surf` does. Note that no surface normals are required, the interpolation is purely geometric.
#'
#' @param frac_surface character string, the second surface for `surface_frac`. Ignored if `surface_frac` is `NULL`. Defaults to 'pial'.
#'
#' @param interpolation character string, one of 'trilinear' (the default, an alias 'linear' is accepted) or 'nearest'. Trilinear interpolation is suitable for continuous data like thickness or t-statistics, while nearest neighbor is what you want for discrete data like atlas labels, segmentation indices or p-values (it never averages the values of neighboring voxels).
#'
#' @param frame positive integer scalar, the index of the volume ('frame') to use if `volume` contains more than one. Defaults to 1. Ignored for 3D volumes.
#'
#' @param cortex_only logical, whether to set the values of all vertices which are *not* part of the cortex (as defined by the label file `label/?h.cortex.label`) to NA. This masks the medial wall. Defaults to FALSE.
#'
#' @param clamp logical. How to handle surface vertices which are outside the volume: if FALSE (the default), their value is set to NA; if TRUE, the value of the closest voxel at the volume border is used instead (edge replication, like the `mode = 'nearest'` parameter of `scipy.ndimage.map_coordinates`). See also `check_fov`.
#'
#' @param check_fov logical, whether to check for surface vertices which are outside the volume and emit a warning listing them. Defaults to TRUE. The check is very cheap (a few comparisons per vertex), the parameter exists mainly so that you can silence the warning when you know that a large part of the surface is outside the volume on purpose.
#'
#' @param affine numeric 4x4 matrix or `NULL` (the default). The matrix that maps **0-based** voxel indices to world coordinates, i.e., the coordinate space in which the surface vertices are defined. By default this is derived from the volume (see `vox2ras`). Use this parameter only if you want to override the transformation completely.
#'
#' @param vox2ras character string, how the transformation from voxel indices to the coordinate space of the surface is obtained. One of 'auto' (the default), 'tkr' or 'header'. For MGH/MGZ volumes, 'auto' uses the FreeSurfer *tkregister* convention, in which the brain surfaces are defined: the direction cosines and voxel sizes from the volume header, but with the origin moved to the center of the volume (see \code{\link[fsbrain]{vox2ras_tkr}} and \code{\link[fsbrain]{vol.tkreg.affine}}). This matches what the FreeSurfer tools do (it is the difference between the `--vox2ras-tkr` and the `--vox2ras` option of `mri_vol2surf`), and it matters for volumes whose header carries a non-zero center-of-RAS offset (`cras`) -- such volumes are common, e.g., when the data was imported from DICOM. For NIfTI volumes, 'auto' uses the transformation matrix from the file header (`sform`/`qform`), as there is no tkregister convention for NIfTI files. Use 'header' or 'tkr' to force one of the two interpretations. Ignored if `affine` is given.
#'
#' @return a numerical vector of per-vertex values, one value per surface vertex, or a hemilist (named list with entries `lh` and `rh`) of such vectors if `hemi` is 'both'. Note that the values are in the order of the vertices of the *sampled* surface, which is the order used by the surface file, so they can be used directly with functions like \code{\link[fsbrain]{vis.subject.morph.native}}.
#'
#' @note Trilinear interpolation is used by default, while the FreeSurfer tool `mri_vol2surf` defaults to nearest neighbor interpolation: pass `interpolation = "nearest"` to reproduce its output exactly. Note also that the affine matrix of a NIfTI file is converted into an MGH-style header by the reader, which stores the direction cosines with limited precision: for volumes with an oblique (rotated) orientation the transformation can differ from the value in the file by about 1e-07, and sheared transformations are not supported (the reader warns about this).
#'
#' @family volume to surface projection functions
#'
#' @examples
#' \dontrun{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    # Project the brain volume of subject1 onto the white surface:
#'    brain_morph = subject.vol2surf(subjects_dir, "subject1", volume = "brain", surface = "white");
#'    # Project the same volume onto the mid-cortical surface (between white and pial):
#'    mid_morph = subject.vol2surf(subjects_dir, "subject1", volume = "brain",
#'        surface = "white", frac_surface = "pial", surface_frac = 0.5);
#'    # Visualize the result on the surface:
#'    vis.subject.morph.native(subjects_dir, "subject1", morph_data = brain_morph);
#' }
#'
#' @export
subject.vol2surf <- function(subjects_dir, subject_id, volume, surface = "white", hemi = "both",
        surface_frac = NULL, frac_surface = "pial", interpolation = "trilinear", frame = 1L,
        cortex_only = FALSE, clamp = FALSE, check_fov = TRUE, affine = NULL, vox2ras = "auto") {

    if(! (hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }
    vol.check.surface.frac(surface_frac);
    interpolation = vol.check.interpolation(interpolation);

    hemis = if(hemi == "both") c("lh", "rh") else hemi;

    surfaces = list();
    for(h in hemis) {
        sf = subject.surface(subjects_dir, subject_id, surface, h);
        if(is.null(surface_frac)) {
            surfaces[[h]] = sf$vertices;
        } else {
            sf_frac = subject.surface(subjects_dir, subject_id, frac_surface, h);
            surfaces[[h]] = surface.interpolate.frac(sf, sf_frac, surface_frac);
        }
    }

    result = vol.vol2surf.hemilist(surfaces, volume, default_dir = file.path(subjects_dir, subject_id, "mri"),
        interpolation = interpolation, frame = frame, clamp = clamp, check_fov = check_fov, affine = affine, vox2ras = vox2ras);

    if(check_fov) {
        num_verts = list();
        for(h in hemis) {
            num_verts[[h]] = nrow(surfaces[[h]]);
        }
        vol.warn.outside(attr(result, "outside"), num_verts, clamp = clamp);
    }

    if(isTRUE(cortex_only)) {
        result = vol.apply.cortex.mask(result, subjects_dir, subject_id);
    }

    attr(result, "outside") = NULL;
    attr(result, "volume_filepath") = NULL;
    if(hemi == "both") {
        return(result);
    }
    return(result[[hemi]]);
}


#' @title Project a volume onto a template surface like fsaverage or fs_LR_32.
#'
#' @description Samples a volume at the positions of the vertices of a template surface, e.g., for group-level statistical maps. The volume and the template surface must be defined in the same coordinate space: `fsaverage` is the FreeSurfer template and its surfaces are in MNI305 (surface RAS) space, while the `fs_LR_32` template (the HCP-style 32k surface space) is in MNI152 space. Projecting an MNI152 volume onto fsaverage (or vice versa) will produce wrong results, but this function will usually detect it and warn (see `check_fov`).
#'
#'   This is the equivalent of the `project_vol2surf` function of the Python package `yabplot` for the `fs_LR_32` template, and of \code{mri_vol2surf} for `fsaverage`. See \code{\link[fsbrain]{subject.vol2surf}} for the per-subject variant, which uses the native space of a subject.
#'
#' @param volume character string or named list, see \code{\link[fsbrain]{subject.vol2surf}}. For template projections, this is typically the filepath of a NIfTI file containing a group-level map, e.g., in MNI152 space.
#'
#' @param template character string, the template identifier. One of 'fsaverage' (the FreeSurfer template, in MNI305 space) or 'fs_LR_32' (the HCP-style 32k template, in MNI152 space). Additional templates can be used by passing the respective `subjects_dir` and a template identifier that matches a directory in it.
#'
#' @param surface character string, the name of the surface to sample. Examples: 'white', 'pial', 'midthickness', 'inflated'. Defaults to 'white' for 'fsaverage' and to 'midthickness' for 'fs_LR_32' if `surface` is `NULL`.
#'
#' @param hemi character string, see \code{\link[fsbrain]{subject.vol2surf}}.
#'
#' @param subjects_dir character string or `NULL`, the subjects dir which contains the template subject. If `NULL` (the default), the fsbrain data cache (see \code{\link[fsbrain]{get_optional_data_filepath}}) is searched first, then the FreeSurfer installation. If the required files are missing, the respective download function is suggested in the error message.
#'
#' @param surface_frac numeric scalar or `NULL`, see \code{\link[fsbrain]{subject.vol2surf}}.
#'
#' @param frac_surface character string, see \code{\link[fsbrain]{subject.vol2surf}}.
#'
#' @param interpolation character string, see \code{\link[fsbrain]{subject.vol2surf}}.
#'
#' @param frame positive integer scalar, see \code{\link[fsbrain]{subject.vol2surf}}.
#'
#' @param cortex_only logical, see \code{\link[fsbrain]{subject.vol2surf}}. For 'fsaverage', the required `label/?h.cortex.label` file is part of the FreeSurfer installation, for 'fs_LR_32' it can be downloaded with \code{\link[fsbrain]{download_fs_LR_32_labels}}.
#'
#' @param clamp logical, see \code{\link[fsbrain]{subject.vol2surf}}.
#'
#' @param check_fov logical, see \code{\link[fsbrain]{subject.vol2surf}}.
#'
#' @param affine numeric 4x4 matrix or `NULL`, see \code{\link[fsbrain]{subject.vol2surf}}.
#'
#' @param vox2ras character string, see \code{\link[fsbrain]{subject.vol2surf}}.
#'
#' @return a numerical vector of per-vertex values, or a hemilist of such vectors if `hemi` is 'both'. See \code{\link[fsbrain]{subject.vol2surf}}.
#'
#' @note The `fs_LR_32` template meshes can be downloaded with \code{\link[fsbrain]{download_fs_LR_32_meshes}}. Note that the surface files of the templates are not part of the fsbrain package, and that the `fsaverage` data is subject to the FreeSurfer license.
#'
#' @family volume to surface projection functions
#'
#' @examples
#' \dontrun{
#'    # Project a group-level stat map (e.g., in MNI152 space) onto the HCP-style fs_LR_32 template:
#'    fsbrain::download_fs_LR_32_meshes();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    stat_map = template.vol2surf("/path/to/your_statmap.nii.gz", template = "fs_LR_32",
#'        surface = "midthickness", subjects_dir = subjects_dir);
#'    # Project an MNI305 map onto fsaverage:
#'    stat_map = template.vol2surf("/path/to/your_mni305_map.nii.gz", template = "fsaverage",
#'        surface = "white");
#' }
#'
#' @export
template.vol2surf <- function(volume, template = "fsaverage", surface = NULL, hemi = "both",
        subjects_dir = NULL, surface_frac = NULL, frac_surface = "pial", interpolation = "trilinear",
        frame = 1L, cortex_only = FALSE, clamp = FALSE, check_fov = TRUE, affine = NULL, vox2ras = "auto") {

    if(! (hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }
    if(is.null(surface)) {
        surface = if(template == "fs_LR_32") "midthickness" else "white";
    }
    vol.check.surface.frac(surface_frac);
    interpolation = vol.check.interpolation(interpolation);

    subjects_dir = resolve.template.subjects.dir(template, subjects_dir = subjects_dir);

    hemis = if(hemi == "both") c("lh", "rh") else hemi;

    surfaces = list();
    for(h in hemis) {
        sf = subject.surface(subjects_dir, template, surface, h);
        if(is.null(surface_frac)) {
            surfaces[[h]] = sf$vertices;
        } else {
            sf_frac = subject.surface(subjects_dir, template, frac_surface, h);
            surfaces[[h]] = surface.interpolate.frac(sf, sf_frac, surface_frac);
        }
    }

    result = vol.vol2surf.hemilist(surfaces, volume, default_dir = file.path(subjects_dir, template, "mri"),
        interpolation = interpolation, frame = frame, clamp = clamp, check_fov = check_fov, affine = affine, vox2ras = vox2ras);

    if(check_fov) {
        num_verts = list();
        for(h in hemis) {
            num_verts[[h]] = nrow(surfaces[[h]]);
        }
        vol.warn.outside(attr(result, "outside"), num_verts, clamp = clamp);
    }

    if(isTRUE(cortex_only)) {
        result = vol.apply.cortex.mask(result, subjects_dir, template);
    }

    attr(result, "outside") = NULL;
    attr(result, "volume_filepath") = NULL;
    if(hemi == "both") {
        return(result);
    }
    return(result[[hemi]]);
}
