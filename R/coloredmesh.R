# Functions for generating coloredmeshes from data and managing their colormaps.



#' @title Recompute the colormaps in the meshes, using data from all meshes.
#'
#' @description Running this function on a set of coloredmeshes ensures that one color represents the same data value over all the meshes. This makes sense if you plot the left and right hemisphere of a subject into a plot. This function only works if the meshes comes with a key named 'morph_data' that contains the raw data values. If there is no such data, the given meshes are returned without changes.
#'
#' @param coloredmeshes list of coloredmeshes
#'
#' @param colormap a colormap function, defaults to NULL, which instructs the function to use the colormap found in the "cmap_fun" property of the first mesh in the list that has a valid entry.
#'
#' @importFrom squash cmap makecmap jet
#' @keywords internal
unify.coloredmeshes.colormaps <- function(coloredmeshes, colormap=NULL) {
    if(length(coloredmeshes) <= 1) {
        return(coloredmeshes);
    }


    comb_res = combine.coloredmeshes.data(coloredmeshes);
    full_data = comb_res$full_data;
    found_morph_data = comb_res$found_morph_data_in_any;

    if(is.null(colormap)) {
        colormap = check.for.coloredmeshes.colormap(coloredmeshes);
    }


    # We have all the data (if any), compute a shared colormap for all the meshes to use.
    # Note: some meshes come without morph data, e.g., those based on annotations. But for them, we do not need to rescale anyways so thats fine.
    if(found_morph_data && length(full_data) > 0) {

        if(is.null(colormap)) {
            warning("Parameter 'colormap' is NULL and no cmap_fun found in the mesh(es). Falling back to default colormap squash::jet.");
            colormap = squash::jet;
        }

        if(! comb_res$found_morph_data_in_all) {
            warning("Found morph_data only in a subset of the meshes. This should not happen in general.");
        }

        coloredmeshes_new_cmap = coloredmeshes;
        for(cmesh_idx in seq_len(length(coloredmeshes_new_cmap))) {
            col_rescaled = squash::cmap(coloredmeshes_new_cmap[[cmesh_idx]]$morph_data, map = squash::makecmap(full_data, colFn = colormap));
            coloredmeshes_new_cmap[[cmesh_idx]]$col = col_rescaled;
        }
        return(coloredmeshes_new_cmap);
    } else {
        return(coloredmeshes);
    }
}


#' @title Combine the data from the coloredmeshes, if any.
#'
#' @param coloredmeshes list of coloredmeshes
#'
#' @return list with entries "full_data": a vector of the combined data, can be NULL if none of the meshes have a valid "morph_data" attribute. "found_morph_data_in_any": logical, whether valid data was found in any of the meshes. "found_morph_data_in_all": logical, whether valid data was found in all of the meshes (FALSE if list of meshes is empty).
#'
#' @keywords internal
combine.coloredmeshes.data <- function(coloredmeshes) {
    full_data = c();
    found_morph_data_in_any = FALSE;
    found_morph_data_in_all = TRUE;
    if(length(coloredmeshes) < 1) {
        found_morph_data_in_all = FALSE;
    }
    for(cmesh in coloredmeshes) {
        if("morph_data" %in% names(cmesh) && !(is.null(cmesh$morph_data))) {
            full_data = c(full_data, cmesh$morph_data);
            found_morph_data_in_any = TRUE;
        } else {
            found_morph_data_in_all = FALSE;
        }
    }
    full_data = sort(full_data);
    return(list("full_data"=full_data, "found_morph_data_in_any"=found_morph_data_in_any, "found_morph_data_in_all"=found_morph_data_in_all));
}


#' @title Return the colormap function from the meshes, if any.
#'
#' @param coloredmeshes list of coloredmeshes
#'
#' @return colormap, a colormap function or NULL if the meshes did not have any.
#'
#' @keywords internal
check.for.coloredmeshes.colormap <- function(coloredmeshes) {
    colormap = NULL;
    for(cmesh in coloredmeshes) {
        if("cmap_fun" %in% names(cmesh) && !(is.null(cmesh$cmap_fun))) {
            if(is.null(colormap)) {
                colormap = cmesh$cmap_fun;
            }
        }
    }
    return(colormap);
}




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
#' @param clip, numeric vector of length 2 or NULL. If given, the 2 values are interpreted as lower and upper percentiles, and the morph data is clipped at the given lower and upper percentile (see [fsbrain::clip.data()]). Defaults to NULL (no data clipping).
#'
#' @param cortex_only logical, whether to mask the medial wall, i.e., whether the morphometry data for all vertices which are *not* part of the cortex (as defined by the label file `label/?h.cortex.label`) should be replaced with NA values. In other words, setting this to TRUE will ignore the values of the medial wall between the two hemispheres. If set to true, the mentioned label file needs to exist for the subject. Defaults to FALSE.
#'
#' @return coloredmesh. A named list with entries: "mesh" the [rgl::tmesh3d] mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @keywords internal
#' @importFrom squash cmap makecmap jet
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.morph.native <- function(subjects_dir, subject_id, measure, hemi, surface="white", colormap=squash::jet, clip=NULL, cortex_only=FALSE) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    morph_data = subject.morph.native(subjects_dir, subject_id, measure, hemi, cortex_only=cortex_only);

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
#
#' @param template_subjects_dir The template subjects dir. If NULL, the value of the parameter 'subjects_dir' is used. Defaults to NULL. If you have FreeSurfer installed and configured, and are using the standard fsaverage subject, try passing the result of calling 'file.path(Sys.getenv('FREESURFER_HOME'), 'subjects')'.
#'
#' @param colormap, a colormap function. See the squash package for some colormaps. Defaults to [squash::jet].
#'
#' @param clip, numeric vector of length 2 or NULL. If given, the 2 values are interpreted as lower and upper percentiles, and the morph data is clipped at the given lower and upper percentile (see [fsbrain::clip.data]). Defaults to NULL (no data clipping).
#'
#' @param cortex_only logical, whether to mask the medial wall, i.e., whether the morphometry data for all vertices which are *not* part of the cortex (as defined by the label file `label/?h.cortex.label`) should be replaced with NA values. In other words, setting this to TRUE will ignore the values of the medial wall between the two hemispheres. If set to true, the mentioned label file needs to exist for the template subject. Defaults to FALSE.
#'
#' @return coloredmesh. A named list with entries: "mesh" the [rgl::tmesh3d()] mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @keywords internal
#' @importFrom squash cmap makecmap jet
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.morph.standard <- function(subjects_dir, subject_id, measure, hemi, fwhm, surface="white", template_subject='fsaverage', template_subjects_dir=NULL, colormap=squash::jet, clip = NULL, cortex_only=FALSE) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    if(is.null(template_subjects_dir)) {
        template_subjects_dir = subjects_dir;
    }

    morph_data = subject.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm = fwhm, cortex_only = cortex_only);

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
#' @param label string or vector of integers. If a string, the name of the label file, without the hemi part (if any), but including the '.label' suffix. E.g., 'cortex.label' for '?h.cortex.label'. Alternatively, the already loaded label data as a vector of integers.
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

    if(is.character(label)) {
        label_data = subject.label(subjects_dir, subject_id, label, hemi);
    } else {
        label_data = label;
    }

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

    morph_like_data[mask == 1L] = NA;     # set positive values to NA so they get rendered as background.

    mesh = rgl::tmesh3d(c(t(surface_data$vertices)), c(t(surface_data$faces)), homogeneous=FALSE);
    col = squash::cmap(morph_like_data, map = squash::makecmap(morph_like_data, colFn = colormap));
    return(list("mesh"=mesh, "col"=col, "morph_data_was_all_na"=FALSE, "hemi"=hemi, "morph_data"=morph_like_data, "cmap_fun"=colormap));
}

