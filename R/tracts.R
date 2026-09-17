# White matter tract (streamline) visualization: bundles drawn as lines inside
# (or without) a semi-transparent brain shell.
#
# The streamlines are read with the readers of the 'freesurferformats' package
# (read.dti.trk() and read.dti.tck()), which support the two established
# streamline formats (TrackVis TRK and MRtrix TCK), optionally gzip-compressed
# for TCK, and can read subsets of huge files. The drawing itself reuses the
# line renderable of the connectome code ('fs.coloredpaths'), so the result is a
# regular fsbrain renderable list: it works with both renderer backends (rgl and
# scimesh) and can be passed to export() for publication quality figures.
#
# Terminology: a 'tract' or 'streamline' is a single polyline (n points), a
# 'bundle' is a named set of streamlines (e.g. the left corticospinal tract), and
# a 'bundle atlas' is a set of files, one per bundle, like the XTRACT atlas.
# The tract atlas data is not part of this package, see download_xtract_tracts().


#' @title Compute the line segments of streamlines.
#'
#' @description Converts streamlines (a list of point sequences) into the pair of
#' point matrices that describes the line segments between their consecutive
#' points, which is the representation used by \code{\link[fsbrain]{fs.coloredpaths}}.
#' This is vectorized: the segments of all streamlines are computed in one go,
#' no matter how many there are.
#'
#' @param tracts an \code{fs.tracts} instance (as returned in the \code{tracks}
#'   entry of \code{freesurferformats::read.dti.trk} and
#'   \code{freesurferformats::read.dti.tck}), an (n, 3) matrix of points (a
#'   single streamline), or a list of (n, 3) matrices (one per streamline).
#'
#' @return named list with entries \code{from} (matrix of segment start points),
#'   \code{to} (matrix of segment end points) and \code{lengths} (integer vector,
#'   the number of segments of each streamline).
#'
#' @keywords internal
streamlines.to.segments <- function(tracts) {

    if(freesurferformats::is.fs.tracts(tracts)) {
        coords = freesurferformats::fs.tracts.coords(tracts);
        point_counts = freesurferformats::fs.tracts.lengths(tracts);
    } else if(is.matrix(tracts) && is.numeric(tracts)) {
        coords = tracts;
        point_counts = nrow(coords);
    } else if(is.list(tracts)) {
        if(length(tracts) < 1L) {
            stop("Parameter 'tracts' must contain at least one streamline.\n");
        }
        bad = which(! vapply(tracts, function(x) { is.matrix(x) && is.numeric(x) && ncol(x) == 3L }, logical(1L)));
        if(length(bad) > 0L) {
            stop(sprintf("Parameter 'tracts' must be a list of (n, 3) numeric matrices (one per streamline), but entry %d is not.\n", bad[1L]));
        }
        point_counts = vapply(tracts, nrow, integer(1L));
        coords = do.call(rbind, lapply(tracts, function(x) { unname(as.matrix(x)) }));
    } else {
        stop("Parameter 'tracts' must be an fs.tracts instance, an (n, 3) numeric matrix, or a list of such matrices.\n");
    }

    if(! (is.matrix(coords) && is.numeric(coords) && ncol(coords) == 3L)) {
        stop("The tract coordinates must be an (n, 3) numeric matrix.\n");
    }
    if(nrow(coords) != sum(point_counts)) {
        stop(sprintf("The tract data is inconsistent: %d coordinates rows for %d points.\n", nrow(coords), sum(point_counts)));
    }

    # The k-th segment of a streamline starts at its k-th point, so the start row
    # of segment k of tract i is start_row(i) + k - 1. Both 'rep.int' and
    # 'sequence' work on vectors, which gives all segments of all tracts without
    # any loop over the tracts.
    segment_counts = pmax(point_counts - 1L, 0L);
    start_rows = group.start.rows(point_counts);
    from_rows = rep.int(start_rows, segment_counts) + sequence(segment_counts) - 1L;

    return(list("from"=coords[from_rows, , drop = FALSE],
                "to"=coords[from_rows + 1L, , drop = FALSE],
                "lengths"=segment_counts));
}


#' @title Compute the first row of every group in a concatenated matrix.
#'
#' @param lengths integer vector, the number of rows of each group.
#'
#' @return integer vector with one entry per group.
#'
#' @keywords internal
group.start.rows <- function(lengths) {
    if(length(lengths) == 0L) {
        return(integer(0L));
    }
    return(cumsum(c(1L, lengths[-length(lengths)])));
}


#' @title Color line segments by their direction.
#'
#' @description Computes the classic DTI orientation colors, i.e., segments
#' running left-right, anterior-posterior and superior-inferior get different
#' colors (red, green and blue for the 'axis' mode).
#'
#' @param from matrix of segment start points, see \code{\link[fsbrain]{fs.coloredpaths}}.
#'
#' @param to matrix of segment end points, same size as \code{from}.
#'
#' @param mode character string, either 'axis' (every segment gets one fully
#'   saturated color, based on the dominant direction axis) or 'rgb' (the color
#'   channels are the absolute direction components, which gives smoother but
#'   paler colors).
#'
#' @return vector of hex color strings, one per segment.
#'
#' @keywords internal
segment.orientation.colors <- function(from, to, mode = c("axis", "rgb")) {
    mode = match.arg(mode);
    directions = to - from;
    lengths = sqrt(rowSums(directions * directions));
    lengths[lengths < .Machine$double.eps] = 1.0;   # a segment without length gets a color anyway
    directions = directions / lengths;

    if(mode == "axis") {
        channel_idx = max.col(abs(directions), ties.method = "first");
        channels = matrix(0L, nrow = nrow(directions), ncol = 3L);
        channels[cbind(seq_len(nrow(directions)), channel_idx)] = 255L;
    } else {
        channels = round(abs(directions) * 255.0);
    }

    return(grDevices::rgb(channels[, 1L], channels[, 2L], channels[, 3L], maxColorValue = 255.0));
}


#' @title Read tract (streamline) files into named bundles.
#'
#' @description Reads one or more tract files in TRK or TCK format (see the
#' details) and returns them as a named list of bundles, i.e., one entry per
#' file. This is the input format of \code{\link[fsbrain]{vis.tracts}}. A single
#' directory can be passed to read a whole tract atlas (one file per bundle, e.g.
#' the XTRACT atlas, see \code{\link[fsbrain]{download_xtract_tracts}}).
#'
#' @details The files are read with the readers of the 'freesurferformats'
#' package. Both the TrackVis TRK format (used by FSL, DSI Studio, AFQ, TractSeg
#' and the XTRACT atlas) and the MRtrix3 TCK format (used by MRtrix3, QSIRecon,
#' MRtrix3_connectome) are supported; TCK files may be gzip-compressed, TRK files
#' may not. The bundle names are the file names without the extension, e.g.
#' 'CST_L.trk' becomes the bundle 'CST_L'.
#'
#' Note that TRK files store a transformation matrix which is not necessarily the
#' identity: a TRK file can contain coordinates in a voxel grid (e.g. all files of
#' the XTRACT atlas do). With the default \code{coords = "ras"}, the coordinates
#' are returned in RAS+ mm space, which is what the template surfaces of
#' fsbrain are in, see the parameter documentation. Pass \code{coords = "native"}
#' to get the coordinates exactly as stored in the file.
#'
#' @param paths character string, either the path to a directory containing tract
#'   files, or a vector of paths to tract files.
#'
#' @param coords character string or NULL, the coordinate system of the returned
#'   coordinates for TRK files, see \code{\link[freesurferformats]{read.dti.trk}}.
#'   The default 'ras' returns RAS+ mm coordinates, which is required to combine
#'   the tracts with the fsbrain template surfaces: 'native' returns the
#'   coordinates as stored, which for TRK files is usually a voxel grid, so the
#'   result will be mirrored when plotted. Note that this parameter has no effect
#'   for TCK files, which do not store a transformation and are always returned as
#'   stored.
#'
#' @param transform_matrix a 4x4 numeric matrix or NULL. If given, it is applied
#'   to the coordinates of all files after reading them, e.g. to move a tract
#'   atlas from MNI space into the space of the surfaces, see the note in
#'   \code{\link[fsbrain]{vis.tracts}}.
#'
#' @param max_tracks numeric, the maximum number of streamlines to read per file.
#'   Use \code{Inf} (the default) to read all of them. Reading a subset is the way
#'   to handle whole-brain tractograms, which can contain millions of streamlines
#'   and cannot be plotted (or held in memory) as a whole.
#'
#' @param skip_tracks integer, the number of streamlines to skip per file, see
#'   \code{\link[freesurferformats]{read.dti.tck}}.
#'
#' @param bbox numeric vector of length 6 or NULL, a bounding box to select the
#'   streamlines that pass through a region, see
#'   \code{\link[freesurferformats]{read.dti.tck}}. The box is interpreted in the
#'   coordinate system given by \code{coords}.
#'
#' @param pattern character string, a regular expression to select the files in a
#'   directory. Ignored if \code{paths} is not a directory.
#'
#' @param silent logical, whether to suppress the progress messages.
#'
#' @return named list of \code{fs.tracts} instances, one per file (see
#'   \code{\link[freesurferformats]{fs.tracts}}). The names are the file names
#'   without the extension. The attribute 'files' contains the file paths, and the
#'   attribute 'coords_space' the coordinate system of the coordinates ('ras' or
#'   'native', see the parameter \code{coords}). An entry which was read from a
#'   TCK file is a TCK entry, i.e., its \code{[[i]]} entries are coordinate
#'   matrices, while the entries of a TRK file also contain the per-point scalars
#'   and per-track properties of the file.
#'
#' @family tracts functions
#'
#' @examples
#' \dontrun{
#'   # Read a whole tract atlas from a directory, one bundle per file:
#'   atlas_dir = file.path(get_optional_data_filepath("tracts"), "xtract_tiny");
#'   bundles = read.tract.bundles(atlas_dir);
#'   names(bundles);
#'   fs.tracts.lengths(bundles$CST_L);
#'
#'   # Read a single bundle:
#'   cst = read.tract.bundles(file.path(atlas_dir, "CST_L.trk"));
#' }
#'
#' @export
read.tract.bundles <- function(paths, coords = "ras", transform_matrix = NULL, max_tracks = Inf,
                               skip_tracks = 0L, bbox = NULL,
                               pattern = "\\.(trk|tck)(\\.gz)?$", silent = FALSE) {

    if(! is.character(paths) || length(paths) < 1L) {
        stop("Parameter 'paths' must be a character string (a directory or a file) or a vector of file paths.\n");
    }

    if(! is.null(coords)) {
        coords = match.arg(coords, c("native", "ras"));
    }
    if(! is.null(transform_matrix)) {
        transform_matrix = check.transform.matrix(transform_matrix);
    }
    if(! (is.numeric(max_tracks) && length(max_tracks) == 1L && max_tracks > 0.0)) {
        stop("Parameter 'max_tracks' must be a single positive number (or Inf).\n");
    }
    if(! (is.numeric(skip_tracks) && length(skip_tracks) == 1L && skip_tracks >= 0.0)) {
        stop("Parameter 'skip_tracks' must be a single non-negative number.\n");
    }

    if(length(paths) == 1L && dir.exists(paths)) {
        files = list.files(paths, pattern = pattern, full.names = TRUE);
        files = files[! startsWith(basename(files), ".")];
        files = files[! grepl("__MACOSX", files, fixed = TRUE)];
        files = sort(files);
        if(length(files) < 1L) {
            stop(sprintf("The directory '%s' does not contain any tract files matching the pattern '%s'.\n", paths, pattern));
        }
    } else {
        files = paths;
        missing_files = files[! file.exists(files)];
        if(length(missing_files) > 0L) {
            stop(sprintf("The following tract file(s) do not exist: %s\n", paste(missing_files, collapse = ", ")));
        }
    }

    if(! silent) {
        cat(sprintf("Reading %d tract file(s) with the %s package.\n", length(files), "freesurferformats"));
    }

    bundles = list();
    file_paths = character(0L);
    any_native = FALSE;

    for(file_idx in seq_along(files)) {
        filepath = files[file_idx];
        bundle_name = tract.bundle.name.from.file(filepath);
        is_gz = grepl("\\.gz$", filepath, ignore.case = TRUE);
        file_ext = tolower(tools::file_ext(sub("\\.gz$", "", filepath, ignore.case = TRUE)));

        if(file_ext == "trk") {
            if(is_gz) {
                stop(sprintf("Reading gzip-compressed TRK files is not supported (file '%s'): the TRK reader of the 'freesurferformats' package supports gzip for TCK files only. Please decompress the file first, e.g. with 'gunzip'.\n", filepath));
            }
            result = freesurferformats::read.dti.trk(filepath, coords = coords, max_tracks = max_tracks,
                                                     skip_tracks = skip_tracks, bbox = bbox);
            tracts = result$tracks;
            if(identical(result$header$coords_space, "native")) {
                any_native = TRUE;
            }
        } else if(file_ext == "tck") {
            result = freesurferformats::read.dti.tck(filepath, max_tracks = max_tracks,
                                                     skip_tracks = skip_tracks, bbox = bbox);
            tracts = result$tracks;
        } else {
            stop(sprintf("File '%s' is not in TRK or TCK format (unexpected file extension '%s').\n", filepath, file_ext));
        }

        if(! is.null(transform_matrix)) {
            tracts = apply.transform.to.tracts(tracts, transform_matrix);
        }

        if(! silent) {
            cat(sprintf("  %s: %d streamlines, %d points.\n", bundle_name, length(tracts), nrow(freesurferformats::fs.tracts.coords(tracts))));
        }

        bundles[[bundle_name]] = tracts;
        file_paths = c(file_paths, filepath);
    }

    if(length(names(bundles)) != length(unique(names(bundles)))) {
        stop(sprintf("The tract file names are not unique, the bundle names would be ambiguous: %s\n", paste(names(bundles), collapse = ", ")));
    }

    names(file_paths) = names(bundles);
    attr(bundles, "files") = file_paths;
    attr(bundles, "coords_space") = if(is.null(coords)) NULL else coords;

    return(bundles);
}


#' @title Compute the bundle name for a tract file.
#'
#' @param filepath character string, the path to the tract file.
#'
#' @return character string, the file name without the extension (and without a
#'   trailing '.gz').
#'
#' @keywords internal
tract.bundle.name.from.file <- function(filepath) {
    name = basename(filepath);
    name = sub("\\.gz$", "", name, ignore.case = TRUE);
    name = sub("\\.(trk|tck)$", "", name, ignore.case = TRUE);
    return(name);
}


#' @title Check a transformation matrix.
#'
#' @param transform_matrix the object to check.
#'
#' @return the matrix, as a 4x4 numeric matrix.
#'
#' @keywords internal
check.transform.matrix <- function(transform_matrix) {
    if(! is.matrix(transform_matrix) || ! is.numeric(transform_matrix) || any(dim(transform_matrix) != c(4L, 4L))) {
        stop("Parameter 'transform_matrix' must be a 4x4 numeric matrix.\n");
    }
    if(! all(is.finite(transform_matrix))) {
        stop("Parameter 'transform_matrix' must not contain NA, NaN or Inf values.\n");
    }
    return(transform_matrix);
}


#' @title Apply a 4x4 transformation matrix to tracts.
#'
#' @param tracts an \code{fs.tracts} instance.
#'
#' @param transform_matrix a 4x4 numeric matrix.
#'
#' @return the \code{fs.tracts} instance with transformed coordinates.
#'
#' @keywords internal
apply.transform.to.tracts <- function(tracts, transform_matrix) {
    coords = freesurferformats::fs.tracts.coords(tracts);
    transformed = coords %*% t(transform_matrix[1:3, 1:3, drop = FALSE]);
    transformed = sweep(transformed, 2L, transform_matrix[1:3, 4L], "+");
    tracts$coords = unname(transformed);
    return(tracts);
}


#' @title Turn the various supported tract inputs into a list of bundles.
#'
#' @param tracts see \code{\link[fsbrain]{vis.tracts}}.
#'
#' @param coords,transform_matrix,max_tracks,skip_tracks,bbox,silent parameters
#'   passed on to \code{\link[fsbrain]{read.tract.bundles}} if \code{tracts} is a
#'   file path or directory.
#'
#' @return named list of \code{fs.tracts} instances.
#'
#' @keywords internal
as.tract.bundle.list <- function(tracts, coords = "ras", transform_matrix = NULL, max_tracks = Inf,
                                 skip_tracks = 0L, bbox = NULL, silent = FALSE) {
    if(is.character(tracts)) {
        return(read.tract.bundles(tracts, coords = coords, transform_matrix = transform_matrix,
                                  max_tracks = max_tracks, skip_tracks = skip_tracks, bbox = bbox,
                                  silent = silent));
    }

    if(freesurferformats::is.fs.tracts(tracts)) {
        bundles = list(tracts);
        names(bundles) = "tracts";
        return(bundles);
    }

    if(is.matrix(tracts)) {
        bundles = list(freesurferformats::fs.tracts(tracts, nrow(tracts), kind = "tck"));
        names(bundles) = "tracts";
        return(bundles);
    }

    if(! is.list(tracts) || length(tracts) < 1L) {
        stop("Parameter 'tracts' must be a character string (a tract file or a directory of tract files), an fs.tracts instance, an (n, 3) numeric matrix, or a list of such objects.\n");
    }

    # A list of matrices is one tractogram, a list of fs.tracts instances is a set
    # of bundles. Anything else is interpreted as one bundle per list entry, and
    # the entries are converted to fs.tracts.
    is_matrix_list = all(vapply(tracts, function(x) { is.matrix(x) && is.numeric(x) && ncol(x) == 3L }, logical(1L)));
    if(is_matrix_list) {
        bundles = list(streamlines.to.tracts(tracts));
        names(bundles) = "tracts";
        return(bundles);
    }

    bundles = list();
    for(entry_idx in seq_along(tracts)) {
        entry = tracts[[entry_idx]];
        if(freesurferformats::is.fs.tracts(entry)) {
            bundles[[entry_idx]] = entry;
        } else if(is.matrix(entry)) {
            bundles[[entry_idx]] = streamlines.to.tracts(list(entry));
        } else if(is.list(entry)) {
            bundles[[entry_idx]] = streamlines.to.tracts(entry);
        } else {
            stop(sprintf("Parameter 'tracts' contains an unsupported entry at position %d. Supported are fs.tracts instances, (n, 3) matrices, and lists of (n, 3) matrices.\n", entry_idx));
        }
    }

    entry_names = names(tracts);
    if(is.null(entry_names) || any(! nzchar(entry_names))) {
        entry_names = sprintf("bundle%03d", seq_along(bundles));
    }
    names(bundles) = entry_names;
    return(bundles);
}


#' @title Turn a list of streamlines into an fs.tracts instance.
#'
#' @param streamlines list of (n, 3) matrices, one per streamline.
#'
#' @return \code{fs.tracts} instance.
#'
#' @keywords internal
streamlines.to.tracts <- function(streamlines) {
    if(length(streamlines) < 1L) {
        stop("Parameter 'streamlines' must contain at least one streamline.\n");
    }
    bad = which(! vapply(streamlines, function(x) { is.matrix(x) && is.numeric(x) && ncol(x) == 3L }, logical(1L)));
    if(length(bad) > 0L) {
        stop(sprintf("All streamlines must be (n, 3) numeric matrices, but entry %d is not.\n", bad[1L]));
    }
    point_counts = vapply(streamlines, nrow, integer(1L));
    coords = do.call(rbind, lapply(streamlines, function(x) { unname(as.matrix(x)) }));
    return(freesurferformats::fs.tracts(coords, point_counts, kind = "tck"));
}


#' @title Match per-bundle values to the bundles.
#'
#' @param bundle_values named or unnamed numeric vector, or NULL.
#'
#' @param bundles named list of \code{fs.tracts} instances, see
#'   \code{\link[fsbrain]{read.tract.bundles}}.
#'
#' @return numeric vector of length \code{length(bundles)}, or NULL if
#'   \code{bundle_values} was NULL. The values are in the order of \code{bundles}.
#'
#' @keywords internal
match.bundle.values <- function(bundle_values, bundles) {
    if(is.null(bundle_values)) {
        return(NULL);
    }
    if(! is.numeric(bundle_values)) {
        stop("Parameter 'bundle_values' must be a numeric vector (one value per bundle).\n");
    }

    value_names = names(bundle_values);
    if(is.null(value_names)) {
        if(length(bundle_values) != length(bundles)) {
            stop(sprintf("Parameter 'bundle_values' has %d entries, but there are %d bundles. Either pass one value per bundle, or set the names of 'bundle_values' to the bundle names.\n", length(bundle_values), length(bundles)));
        }
        return(as.numeric(bundle_values));
    }

    match_idx = match(normalize.region.names(names(bundles)), normalize.region.names(value_names));
    if(any(is.na(match_idx))) {
        unmatched = names(bundles)[which(is.na(match_idx))];
        stop(sprintf("Found %d bundle(s) without a value in 'bundle_values', e.g. '%s'. The names of 'bundle_values' must be the bundle names (%s).\n", length(unmatched), unmatched[1L], paste(utils::head(names(bundles), 5L), collapse = ", ")));
    }
    return(as.numeric(bundle_values[match_idx]));
}


#' @title Visualize white matter tracts (streamlines) on the cortical surface.
#'
#' @description Draws white matter streamlines (tractography data) as lines, with
#' an optional semi-transparent brain surface as context. The streamlines can come
#' from a tract file (TRK or TCK format), from a directory of such files (a tract
#' atlas with one file per bundle, e.g. the XTRACT atlas, see
#' \code{\link[fsbrain]{download_xtract_tracts}}), or from in-memory data. This is
#' the fsbrain equivalent of the `plot_tracts` function of the Python package
#' `yabplot`.
#'
#' @details The streamlines are drawn with the line renderable of the connectome
#' code, see \code{\link[fsbrain]{fs.coloredpaths}}, so the result is a regular
#' fsbrain renderable list which works with both renderer backends (rgl and the
#' headless scimesh renderer) and can be passed to \code{\link[fsbrain]{export}}
#' to create publication quality figures with a colorbar.
#'
#' The colors can be defined in three ways: (1) as one value per bundle via
#' \code{bundle_values} (e.g. a tractometry measure like the mean fractional
#' anisotropy of each bundle), which is mapped to colors with
#' \code{tract_makecmap_options} and produces a colorbar; (2) as a single color
#' for all bundles via \code{tract_color}; or (3) per segment, based on the
#' direction of the segment, via \code{color_by_orientation}, which is the
#' classic DTI look (red/green/blue for left-right/anterior-posterior/superior-inferior).
#'
#' @param tracts the tracts to draw. One of: a character string, the path to a
#'   tract file (TRK or TCK format) or to a directory containing one file per
#'   bundle (a tract atlas); a vector of tract file paths; an \code{fs.tracts}
#'   instance (\code{freesurferformats}); an (n, 3) matrix of coordinates (a
#'   single streamline); or a list of such objects. A list of (n, 3) matrices is
#'   interpreted as a single bundle (one tractogram), a list of \code{fs.tracts}
#'   instances as one bundle per entry. For the atlas case, the bundle names are
#'   the file names without extension.
#'
#' @param bundle_values numeric vector or NULL, one value per bundle, in the order
#'   of the bundles (or named with the bundle names). The values are mapped to
#'   colors, see the details.
#'
#' @param subjects_dir character string or NULL, the subjects directory that
#'   contains the template. If NULL, the standard fsbrain locations are searched,
#'   see \code{\link[fsbrain]{resolve.template.subjects.dir}}.
#'
#' @param template_id character string, the template or subject to draw the context
#'   surface for. Defaults to 'fs_LR_32', the HCP-style surface space that is
#'   widely used for tractography data. Note that the template meshes are not part
#'   of this package, see \code{\link[fsbrain]{download_fs_LR_32_meshes}}.
#'
#' @param context named list or NULL, the semi-transparent brain surface drawn
#'   around the tracts. Entries are 'surface' (the surface name, default
#'   'midthickness'), 'alpha' (the transparency, default 0.08) and 'color'
#'   (default '#B0B0B0'). Set to NULL to draw the tracts without any context
#'   surface.
#'
#' @param coords character string or NULL, the coordinate system of TRK files, see
#'   \code{\link[fsbrain]{read.tract.bundles}}. Ignored for in-memory data.
#'
#' @param transform_matrix a 4x4 numeric matrix or NULL, a transformation that is
#'   applied to all tract coordinates after reading them. Note that tract data and
#'   template surfaces have to be in the same space for the overlay to be correct:
#'   the XTRACT atlas, for example, is defined in MNI152 space, while the fs_LR_32
#'   and fsaverage templates use an fsaverage-like (MNI305) space, which is close
#'   but not identical, and both are close enough for the overlay to look right
#'   without a transformation. Use this parameter if you have an accurate
#'   transformation, e.g. from a registration of the template to MNI152 space.
#'
#' @param max_tracks numeric, the maximum number of streamlines to read per file,
#'   see \code{\link[fsbrain]{read.tract.bundles}}. This is the way to plot a
#'   subset of a whole-brain tractogram, which can contain millions of streamlines
#'   and cannot be drawn (or held in memory) as a whole.
#'
#' @param skip_tracks integer, the number of streamlines to skip per file, see
#'   \code{\link[fsbrain]{read.tract.bundles}}.
#'
#' @param bbox numeric vector of length 6 or NULL, a bounding box to select the
#'   streamlines that pass through a region, see
#'   \code{\link[fsbrain]{read.tract.bundles}}.
#'
#' @param tract_color vector of hex color strings, the color(s) of the tracts.
#'   Either a single color which is used for all bundles, or one color per bundle.
#'   Ignored if \code{bundle_values} is given. If both \code{tract_color} and
#'   \code{bundle_values} are NULL and \code{color_by_orientation} is FALSE, all
#'   tracts are drawn in a single default color.
#'
#' @param tract_makecmap_options named list of colormap options, see
#'   \code{\link[fsbrain]{mkco.seq}}, used to map \code{bundle_values} to colors.
#'
#' @param tract_width a single positive number, the line width in pixels.
#'
#' @param tract_width_range numeric vector of length 2 or NULL, the range of the
#'   line widths used when the widths are scaled by the bundle values, see
#'   \code{tract_width_scale}. Defaults to \code{c(0.5, 1.5) * tract_width}.
#'
#' @param tract_width_scale character string, either 'none' (all lines have the
#'   same width, the default) or 'value' (the line width encodes the bundle values,
#'   which requires \code{bundle_values} to be given).
#'
#' @param color_by_orientation logical, whether to color the segments by their
#'   direction instead of by bundle, see the details. The colors then do not encode
#'   the bundle values, so no colorbar is drawn.
#'
#' @param views vector of character strings, the views to render, see
#'   \code{\link[fsbrain]{brainviews}}. Pass \code{NULL} to only compute the
#'   renderables without rendering anything (e.g. to pass them to
#'   \code{\link[fsbrain]{export}}).
#'
#' @param rgloptions named list, options for \code{rgl::par3d()}, see
#'   \code{\link[fsbrain]{rglo}}.
#'
#' @param rglactions named list, actions to perform, see
#'   \code{\link[fsbrain]{rglactions}}.
#'
#' @param style character string, the rendering style for the tracts, see
#'   \code{\link[rgl]{material3d}}. The context surface is always rendered with
#'   its own fixed style, so that the requested transparency is used.
#'
#' @param draw_colorbar logical or one of the character strings 'vertical' or
#'   'horizontal', whether to draw a colorbar for the bundle values. Note that the
#'   headless scimesh renderer backend does not support colorbars in this function,
#'   use \code{\link[fsbrain]{export}} instead (see the return value).
#'
#' @param silent logical, whether to suppress the progress messages.
#'
#' @return invisible named list of renderables: the entry \code{tracts} are the
#'   lines (an \code{fs.coloredpaths} instance), and \code{context_lh} and
#'   \code{context_rh} the context surfaces (if \code{context} is not NULL). The
#'   list can be passed to \code{\link[fsbrain]{export}}.
#'
#' @family tracts functions
#'
#' @examples
#' \dontrun{
#'   # Download the XTRACT tract atlas (see the function documentation):
#'   download_xtract_tracts("xtract_tiny");
#'
#'   atlas_dir = file.path(get_optional_data_filepath("tracts"), "xtract_tiny");
#'   bundles = read.tract.bundles(atlas_dir);
#'
#'   # One color per bundle, colors follow a colormap, with a colorbar in the figure:
#'   values = seq(0.2, 0.8, length.out = length(bundles));
#'   names(values) = names(bundles);
#'   tracts = vis.tracts(bundles, bundle_values = values,
#'     views = c("sd_lateral_lh", "sd_lateral_rh"));
#'
#'   # Or a publication quality figure with 3 views and a colorbar:
#'   export(tracts, view_angles = c("sd_lateral_lh", "sd_dorsal", "sd_rostral"),
#'     draw_colorbar = "horizontal", colorbar_legend = "Mean FA",
#'     output_img = "tracts.png");
#'
#'   # Draw a whole-brain tractogram from MRtrix (a subset, it is huge):
#'   vis.tracts("~/data/sub-01_streamlines.tck.gz", max_tracks = 20000);
#' }
#'
#' @export
vis.tracts <- function(tracts, bundle_values = NULL, subjects_dir = NULL, template_id = "fs_LR_32",
        context = list("surface" = "midthickness", "alpha" = 0.08, "color" = "#B0B0B0"),
        coords = "ras", transform_matrix = NULL, max_tracks = Inf, skip_tracks = 0L, bbox = NULL,
        tract_color = NULL, tract_makecmap_options = mkco.seq(),
        tract_width = 1.0, tract_width_range = NULL, tract_width_scale = c("none", "value"),
        color_by_orientation = FALSE,
        views = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"),
        rgloptions = rglo(), rglactions = list(), style = "default",
        draw_colorbar = FALSE, silent = FALSE) {

    tract_width_scale = match.arg(tract_width_scale);

    if(! (is.numeric(tract_width) && length(tract_width) == 1L && is.finite(tract_width) && tract_width > 0.0)) {
        stop("Parameter 'tract_width' must be a single positive number (the line width in pixels).\n");
    }
    if(! is.null(tract_width_range) && ! (is.numeric(tract_width_range) && length(tract_width_range) == 2L && all(is.finite(tract_width_range)) && tract_width_range[1L] > 0.0 && tract_width_range[1L] <= tract_width_range[2L])) {
        stop("Parameter 'tract_width_range' must be NULL, or a numeric vector of length 2 with increasing positive values.\n");
    }
    if(! (is.logical(color_by_orientation) && length(color_by_orientation) == 1L)) {
        stop("Parameter 'color_by_orientation' must be a single logical value.\n");
    }
    if(tract_width_scale == "value" && is.null(bundle_values)) {
        stop("Parameter 'tract_width_scale' is 'value', which requires 'bundle_values' to be given.\n");
    }

    # ── Read/normalize the tracts ─────────────────────────────────────────────
    bundles = as.tract.bundle.list(tracts, coords = coords, transform_matrix = transform_matrix,
                                   max_tracks = max_tracks, skip_tracks = skip_tracks, bbox = bbox,
                                   silent = silent);

    num_bundles = length(bundles);
    values = match.bundle.values(bundle_values, bundles);

    if(! silent) {
        cat(sprintf("Drawing %d bundle(s): %s\n", num_bundles, paste(utils::head(names(bundles), 8L), collapse = ", ")));
    }

    # ── Colors and widths per bundle ──────────────────────────────────────────
    if(color_by_orientation) {
        bundle_colors = NULL;
        tract_metadata = list();
    } else if(! is.null(values)) {
        colorlayer = values.to.colorlayer(values, tract_makecmap_options);
        bundle_colors = colorlayer$colors;
        tract_metadata = list("src_data" = values, "data_range" = range(values, finite = TRUE),
                              "makecmap_options" = colorlayer$makecmap_options);
    } else {
        if(is.null(tract_color)) {
            tract_color = "#FFB000";
        }
        if(! (is.character(tract_color) && length(tract_color) %in% c(1L, num_bundles))) {
            stop(sprintf("Parameter 'tract_color' must be a single hex color string, or one per bundle (%d bundles), but it has length %d.\n", num_bundles, length(tract_color)));
        }
        bundle_colors = recycle(tract_color, num_bundles);
        tract_metadata = list();
    }

    if(tract_width_scale == "value") {
        if(is.null(tract_width_range)) {
            tract_width_range = c(0.5, 1.5) * tract_width;
        }
        bundle_widths = values.to.range(values, tract_width_range);
    } else {
        bundle_widths = rep(tract_width, num_bundles);
    }

    # ── Geometry: one segment matrix for all bundles ──────────────────────────
    from_list = list();
    to_list = list();
    color_list = list();
    width_list = list();

    for(bundle_idx in seq_len(num_bundles)) {
        segments = streamlines.to.segments(bundles[[bundle_idx]]);
        num_segments = nrow(segments$from);
        from_list[[bundle_idx]] = segments$from;
        to_list[[bundle_idx]] = segments$to;
        if(color_by_orientation) {
            color_list[[bundle_idx]] = segment.orientation.colors(segments$from, segments$to);
        } else {
            color_list[[bundle_idx]] = rep(bundle_colors[bundle_idx], num_segments);
        }
        width_list[[bundle_idx]] = rep(bundle_widths[bundle_idx], num_segments);
    }

    tract_from = do.call(rbind, from_list);
    tract_to = do.call(rbind, to_list);
    tract_cols = unlist(color_list, use.names = FALSE);
    tract_widths = unlist(width_list, use.names = FALSE);

    # A streamline with a single point has no segment, and neither has a bundle
    # without any streamline. This is not an error in the data, but there is
    # nothing to draw, and a line renderable needs at least one segment.
    if(nrow(tract_from) < 1L) {
        stop("The tracts do not contain any line segment, so there is nothing to draw. Note that streamlines need at least 2 points each.\n");
    }

    if(! silent) {
        cat(sprintf("Drawing %d line segments.\n", nrow(tract_from)));
    }

    tract_lines = fs.coloredpaths(tract_from, tract_to, col = tract_cols, width = tract_widths,
                                  metadata = tract_metadata);

    renderables = list("tracts" = tract_lines);

    # ── Context surface ───────────────────────────────────────────────────────
    if(! is.null(context)) {
        unknown_context_entries = setdiff(names(context), c("surface", "alpha", "color"));
        if(length(unknown_context_entries) > 0L) {
            stop(sprintf("Unknown entry/entries in parameter 'context': %s. Supported entries are 'surface', 'alpha' and 'color'.\n", paste(unknown_context_entries, collapse = ", ")));
        }
        context_surface = if(is.null(context$surface)) "midthickness" else context$surface;
        context_alpha = if(is.null(context$alpha)) 0.08 else context$alpha;
        context_color = if(is.null(context$color)) "#B0B0B0" else context$color;

        if(! (is.numeric(context_alpha) && length(context_alpha) == 1L && is.finite(context_alpha) && context_alpha >= 0.0 && context_alpha <= 1.0)) {
            stop("The 'alpha' entry of parameter 'context' must be a single number in the range 0 to 1.\n");
        }

        surface_subjects_dir = resolve.template.subjects.dir(template_id, subjects_dir = subjects_dir);
        context_style = list("front" = "filled", "back" = "culled", "lit" = TRUE, "alpha" = context_alpha, "shininess" = 50, "specular" = "black");

        for(context_hemi in c("lh", "rh")) {
            renderables[[sprintf("context_%s", context_hemi)]] = coloredmesh.from.color(surface_subjects_dir, template_id, context_color, context_hemi, surface = context_surface, style = context_style);
        }
    }

    if(is.null(views)) {
        return(invisible(renderables));
    }

    if(! silent) {
        cat(sprintf("Rendering %d view(s) with the %s renderer backend.\n", length(views), get.fsbrain.renderer.backend()));
    }

    brainviews(views, renderables, rgloptions = rgloptions, rglactions = rglactions, style = style, draw_colorbar = draw_colorbar);

    return(invisible(renderables));
}
