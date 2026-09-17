# Downloading of white matter tract atlases (bundles of streamlines).
#
# The tract atlases are not part of this package and not subject to the
# FreeSurfer license: they are downloaded at runtime into the package cache,
# based on a declarative manifest file that ships with the package (see
# inst/extdata/pkgfilecache_manifest_xtract_tracts.csv).


#' @title Download a white matter tract atlas (streamlines).
#'
#' @description Download one of the tract atlases that are distributed as
#' streamlines in TrackVis TRK format, one file per white matter bundle. The
#' bundle files (and the provenance and attribution files that document them) are
#' downloaded into the fsbrain file cache, where
#' \code{\link[fsbrain]{get_optional_data_filepath}} can be used to access them.
#' The bundles can then be read with \code{\link[fsbrain]{read.tract.bundles}}
#' and plotted with \code{\link[fsbrain]{vis.tracts}}.
#'
#' @details The atlases are the XTRACT atlas of the 42 major white matter tracts
#' (Warrington et al., 2020, \doi{10.1126/sciadv.aba8245}), whose streamlines were
#' converted from the probabilistic tract atlases and are available in four levels
#' of spatial detail. Note that the bundles are defined in MNI152 space, while the
#' fs_LR_32 and fsaverage templates of fsbrain use a different (fsaverage-like)
#' space: the two spaces are very similar, so the tracts align with the template
#' surfaces well enough for visualization, but they are not identical. If you need
#' an exact alignment, register the template to MNI152 and pass the resulting
#' matrix to the parameter \code{transform_matrix} of
#' \code{\link[fsbrain]{vis.tracts}}.
#'
#' The files are hosted on the rcmd.org server of this project, in the same way as
#' the other optional data of the package, see
#' \code{\link[fsbrain]{download_optional_data}}. They are redistributed from the
#' archives of the MIT licensed 'yabplot' Python package, which uses the same
#' streamlines; the attribution and provenance files that come with them document
#' the origin (see `tracts/xtract.attribution.json` and the per-level
#' `tracts/xtract_<level>/xtract_<level>.provenance.json` in the file cache). This
#' data is not required for the package to work.
#'
#' @note The levels of detail do not only differ in the number of streamlines, but
#'   also in the set of bundles they contain: 'xtract_tiny' has 37 bundles,
#'   'xtract_small' and 'xtract_medium' have 40, and 'xtract_large' has 42 (the
#'   bundles 'SLF1_L' and 'Cing_PeriGen_L' are only present in the large one).
#'   Since the bundles are selected by name (see the parameter 'bundle_values' of
#'   \code{\link[fsbrain]{vis.tracts}}), data that is mapped to bundles has to match
#'   the atlas that is actually used.
#'
#' @param atlas character string, the atlas to download. One of 'xtract_tiny'
#'   (the smallest one, useful for quick tests), 'xtract_small', 'xtract_medium'
#'   (the default) or 'xtract_large' (the most detailed one).
#'
#' @param download logical, whether to download the files if they are not in the
#'   cache. If FALSE, the function only reports the status of the files.
#'
#' @param scheme character string, the URL scheme to use, see the 'scheme'
#'   parameter of \code{\link[fsbrain]{download_fs_LR_32_meshes}}.
#'
#' @param silent logical, whether to suppress the progress messages.
#'
#' @return named list. The list has the entries "available" (vector of character
#'   strings, the paths of the bundle files that are available in the local file
#'   cache) and "missing" (vector of character strings, the files that could not
#'   be retrieved).
#'
#' @family tracts functions
#'
#' @examples
#' \dontrun{
#'   # Download the small version of the XTRACT atlas:
#'   download_xtract_tracts("xtract_small");
#'
#'   # The bundle files are now in the cache:
#'   atlas_dir = file.path(get_optional_data_filepath("tracts"), "xtract_small");
#'   list.files(atlas_dir);
#' }
#'
#' @export
download_xtract_tracts <- function(atlas = "xtract_medium", download = TRUE, scheme = "https", silent = FALSE) {
    available_atlases = c("xtract_tiny", "xtract_small", "xtract_medium", "xtract_large");
    if(! (is.character(atlas) && length(atlas) == 1L && atlas %in% available_atlases)) {
        stop(sprintf("Parameter 'atlas' must be one of: %s.\n", paste(available_atlases, collapse = ", ")));
    }
    if(! (is.logical(download) && length(download) == 1L)) {
        stop("Parameter 'download' must be a single logical value.\n");
    }

    pkg_info = pkgfilecache::get_pkg_info("fsbrain");
    manifest_file = system.file("extdata", "pkgfilecache_manifest_xtract_tracts.csv", package = "fsbrain");
    if(nchar(manifest_file) == 0L) {
        stop("The manifest file for the tract atlases is missing from the installed package, please re-install fsbrain.\n");
    }
    manifest = pkgfilecache::read_manifest(manifest_file);

    # Download only the files of the requested level of detail, plus the attribution
    # file of the dataset, which all levels share.
    atlas_prefix = sprintf("tracts/%s/", atlas);
    keep = startsWith(manifest$path, atlas_prefix) | (manifest$path == "tracts/xtract.attribution.json");
    manifest = manifest[keep, , drop = FALSE];
    if(nrow(manifest) < 1L) {
        stop(sprintf("The tract atlas '%s' is not defined in the manifest file of the installed package.\n", atlas));
    }
    manifest$url = paste0(scheme, "://", manifest$url);  # The manifest stores scheme-less URLs.

    if(! silent) {
        cat(sprintf("Checking %d file(s) of tract atlas '%s' in the package cache.\n", nrow(manifest), atlas));
    }
    cfiles = pkgfilecache::ensure_files_available_from_manifest(pkg_info, manifest, download = download);

    # Report the status of the bundle files, not the status of the manifest entries.
    atlas_dir = file.path(get_optional_data_filepath("tracts", mustWork = FALSE), atlas);
    available_files = list.tract.bundle.files(atlas_dir);

    missing_files = as.character(cfiles$missing);
    if(! download && length(available_files) < 1L) {
        missing_files = unique(c(missing_files, atlas_prefix));
    }

    return(invisible(list("available" = available_files, "missing" = missing_files)));
}


#' @title List the bundle files of a tract atlas directory.
#'
#' @param atlas_dir character string, the path to the directory.
#'
#' @return vector of character strings, the paths of the bundle files (sorted), or
#'   an empty vector if the directory does not exist or contains no bundle files.
#'
#' @note The file name filter is important: the downloaded archives contain macOS
#'   resource fork files ('._AC.trk' and friends), which are not tract files.
#'
#' @keywords internal
list.tract.bundle.files <- function(atlas_dir) {
    if(! dir.exists(atlas_dir)) {
        return(character(0L));
    }
    bundle_files = list.files(atlas_dir, pattern = "\\.(trk|tck)(\\.gz)?$", full.names = TRUE);
    bundle_files = bundle_files[! startsWith(basename(bundle_files), ".")];
    return(sort(bundle_files));
}
