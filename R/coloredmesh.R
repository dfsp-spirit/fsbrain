# Functions for generating coloredmeshes from data.

#' @title Create a coloredmesh from native space morphometry data.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier.
#'
#' @param measure, string. The morphometry data to use. E.g., 'area' or 'thickness.'
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param surface, string. The display surface. E.g., "white", "pial", or "inflated". Defaults to "white".
#'
#' @param colormap, a colormap function. See the squash package for some colormaps. Defaults to [squash::jet].
#'
#' @param clip, numeric vector of length 2 or NULL. If given, the 2 values are interpreted as lower and upper percentiles, and the morph data is clipped at the given lower and upper percentile (see [fsbrain::clip.data]). Defaults to NULL (no data clipping).
#'
#' @return coloredmesh. A named list with entries: "mesh" the [rgl::tmesh3d] mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @keywords internal
#' @importFrom squash cmap makecmap jet
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.morph.native <- function(subjects_dir, subject_id, measure, hemi, surface="white", colormap=squash::jet, clip=NULL) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    morph_data = subject.morph.native(subjects_dir, subject_id, measure, hemi);

    if(! is.null(clip)) {
        morph_data = clip.data(morph_data, lower=clip[1], upper=clip[2]);
    }

    surface_data = subject.surface(subjects_dir, subject_id, surface, hemi);
    mesh = rgl::tmesh3d(c(t(surface_data$vertices)), c(t(surface_data$faces)), homogeneous=FALSE);
    col = squash::cmap(morph_data, map = squash::makecmap(morph_data, colFn = colormap));
    return(list("mesh"=mesh, "col"=col, "morph_data_was_all_na"=FALSE, "hemi"=hemi, "morph_data"=morph_data, "cmap_fun"=colormap));
}

#' @title Create a coloredmesh from standard space morphometry data.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier.
#'
#' @param measure, string. The morphometry data to use. E.g., 'area' or 'thickness.'
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param fwhm, string, smoothing setting. The smoothing part of the filename, typically something like '0', '5', '10', ...,  or '25'.
#'
#' @param surface, string. The display surface. E.g., "white", "pial", or "inflated". Defaults to "white".
#'
#' @param template_subject The template subject used. This will be used as part of the filename, and its surfaces are loaded for data visualization. Defaults to 'fsaverage'.
#'
#' @param template_subjects_dir The template subjects dir. If NULL, the value of the parameter 'subjects_dir' is used. Defaults to NULL. If you have FreeSurfer installed and configured, and are using the standard fsaverage subject, try passing the result of calling 'file.path(Sys.getenv('FREESURFER_HOME'), 'subjects')'.
#'
#' @param colormap, a colormap function. See the squash package for some colormaps. Defaults to [squash::jet].
#'
#' @param clip, numeric vector of length 2 or NULL. If given, the 2 values are interpreted as lower and upper percentiles, and the morph data is clipped at the given lower and upper percentile (see [fsbrain::clip.data]). Defaults to NULL (no data clipping).
#'
#' @return coloredmesh. A named list with entries: "mesh" the [rgl::tmesh3d] mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @keywords internal
#' @importFrom squash cmap makecmap jet
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.morph.standard <- function(subjects_dir, subject_id, measure, hemi, fwhm, surface="white", template_subject='fsaverage', template_subjects_dir=NULL, colormap=squash::jet, clip = NULL) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    if(is.null(template_subjects_dir)) {
        template_subjects_dir = subjects_dir;
    }

    morph_data = subject.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm = fwhm);

    if(! is.null(clip)) {
        morph_data = clip.data(morph_data, lower=clip[1], upper=clip[2]);
    }

    surface_data = subject.surface(template_subjects_dir, template_subject, surface, hemi);
    mesh = rgl::tmesh3d(c(t(surface_data$vertices)), c(t(surface_data$faces)), homogeneous=FALSE);
    col = squash::cmap(morph_data, map = squash::makecmap(morph_data, colFn = colormap));
    return(list("mesh"=mesh, "col"=col, "morph_data_was_all_na"=FALSE, "hemi"=hemi, "morph_data"=morph_data, "cmap_fun"=colormap));
}


#' @title Create a coloredmesh from arbitrary data.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, containing the subdir of vis_subject_id, the subject that you want to use for visualization.
#'
#' @param vis_subject_id, string. The subject identifier from which to obtain the surface for data visualization. Example: 'fsaverage'.
#'
#' @param morph_data, string. The morphometry data to use. E.g., 'area' or 'thickness.'
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param surface, string. The display surface. E.g., "white", "pial", or "inflated". Defaults to "white".
#'
#' @param colormap, a colormap function. See the squash package for some colormaps. Defaults to [squash::jet].
#'
#' @param all_nan_backup_value, numeric. If all morph_data values are NA/NaN, no color map can be created. In that case, the values are replaced by this value, and this is indicated in the entry morph_data_was_all_na in the return value. Defaults to 0.0.
#'
#' @return coloredmesh. A named list with entries: "mesh" the [rgl::tmesh3d] mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @keywords internal
#' @importFrom squash cmap makecmap jet
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.morphdata <- function(subjects_dir, vis_subject_id, morph_data, hemi, surface="white", colormap=squash::jet, all_nan_backup_value = 0.0) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    surface_data = subject.surface(subjects_dir, vis_subject_id, surface, hemi);

    num_verts = nrow(surface_data$vertices);
    if(length(morph_data) != num_verts) {
        stop(sprintf("Received %d data values, but the hemi '%s' '%s' surface of visualization subject '%s' in dir '%s' has %d vertices. Counts must match.\n", length(morph_data), hemi, surface, vis_subject_id, subjects_dir, num_verts));
    }

    mesh = rgl::tmesh3d(c(t(surface_data$vertices)), c(t(surface_data$faces)), homogeneous=FALSE);

    # If all values are NaN, the following call to squash::cmap fails with an error. We reset the data here to avoid that.
    morph_data_was_all_na = FALSE;
    if(all(is.na(morph_data))) {
        morph_data = as.vector(rep(all_nan_backup_value, length(morph_data)));
        morph_data_was_all_na = TRUE;
    }

    col = squash::cmap(morph_data, map = squash::makecmap(morph_data, colFn = colormap));
    return(list("mesh"=mesh, "col"=col, "morph_data_was_all_na"=morph_data_was_all_na, "hemi"=hemi, "morph_data"=morph_data, "cmap_fun"=colormap));
}



#' @title Create a coloredmesh from an annotation of an atlas.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier.
#'
#' @param atlas, string. The atlas name. E.g., "aparc", "aparc.2009s", or "aparc.DKTatlas". Used to construct the name of the annotation file to be loaded.
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param surface, string. The display surface. E.g., "white", "pial", or "inflated". Defaults to "white".
#'
#' @param colormap, a colormap. See the squash package for some colormaps. Defaults to [squash::jet].
#'
#' @return coloredmesh. A named list with entries: "mesh" the [rgl::tmesh3d] mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @keywords internal
#' @importFrom squash cmap makecmap jet
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.annot <- function(subjects_dir, subject_id, atlas, hemi, surface="white", colormap=squash::jet) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    surface_data = subject.surface(subjects_dir, subject_id, surface, hemi);
    annot = subject.annot(subjects_dir, subject_id, hemi, atlas);
    mesh = rgl::tmesh3d(c(t(surface_data$vertices)), c(t(surface_data$faces)), homogeneous=FALSE);
    col = annot$hex_colors_rgb;
    return(list("mesh"=mesh, "col"=col, "morph_data_was_all_na"=FALSE, "hemi"=hemi, "morph_data"=NULL, "cmap_fun"=colormap));
}


#' @title Create a coloredmesh from an annotation of an atlas.
#'
#' @param subjects_dir string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id string. The subject identifier.
#'
#' @param label string. Name of the label file, without the hemi part (if any), but including the '.label' suffix. E.g., 'cortex.label' for '?h.cortex.label'
#'
#' @param hemi string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param surface string. The display surface. E.g., "white", "pial", or "inflated". Defaults to "white".
#'
#' @param colormap a colormap. See the squash package for some colormaps. Defaults to [squash::rainbow2].
#'
#' @return coloredmesh. A named list with entries: "mesh" the [rgl::tmesh3d] mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @keywords internal
#' @importFrom squash cmap makecmap rainbow2
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.label <- function(subjects_dir, subject_id, label, hemi, surface="white", colormap=squash::rainbow2) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    surface_data = subject.surface(subjects_dir, subject_id, surface, hemi);
    label_data = subject.label(subjects_dir, subject_id, label, hemi);
    mask = mask.from.labeldata.for.hemi(list(label_data), nrow(surface_data$vertices));
    return(coloredmesh.from.mask(subjects_dir, subject_id, mask, hemi, surface=surface, colormap=colormap, surface_data=surface_data));
}



#' @title Create a coloredmesh from a mask.
#'
#' @param subjects_dir string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id string. The subject identifier.
#'
#' @param mask logical vector, contains one logical value per vertex.
#'
#' @param hemi string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param surface string. The display surface. E.g., "white", "pial", or "inflated". Defaults to "white".
#'
#' @param colormap a colormap. See the squash package for some colormaps. Defaults to [squash::rainbow2].
#'
#' @param surface_data optional surface object, as returned by [fsbrain::subject.surface]. If given, used instead of loading the surface data from disk (which users of this function may already have done). Defaults to NULL.
#'
#' @return coloredmesh. A named list with entries: "mesh" the [rgl::tmesh3d] mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @family mask functions
#' @export
coloredmesh.from.mask <- function(subjects_dir, subject_id, mask, hemi, surface="white", colormap=squash::rainbow2, surface_data=NULL) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    if(is.null(surface_data)) {
        surface_data = subject.surface(subjects_dir, subject_id, surface, hemi);
    }

    morph_like_data = as.integer(mask);
    mesh = rgl::tmesh3d(c(t(surface_data$vertices)), c(t(surface_data$faces)), homogeneous=FALSE);
    col = squash::cmap(morph_like_data, map = squash::makecmap(morph_like_data, colFn = colormap));
    return(list("mesh"=mesh, "col"=col, "morph_data_was_all_na"=FALSE, "hemi"=hemi, "morph_data"=morph_like_data, "cmap_fun"=colormap));
}

