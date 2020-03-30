# Functions for generating coloredmeshes from data and managing their colormaps.



#' @title Recompute the colormaps in the meshes, using data from all meshes.
#'
#' @description Running this function on a set of coloredmeshes ensures that one color represents the same data value over all the meshes. This makes sense if you plot the left and right hemisphere of a subject into a plot. This function only works if the meshes comes with a key named 'morph_data' that contains the raw data values. If there is no such data, the given meshes are returned without changes.
#'
#' @param coloredmeshes list of input coloredmeshes
#'
#' @param colormap a colormap function, defaults to NULL, which instructs the function to use the colormap found in the "cmap_fun" property of the first mesh in the list that has a valid entry.
#'
#' @return the coloredmeshes with merged colormap
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
#' @param subjects_dir string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id string. The subject identifier.
#'
#' @param measure string. The morphometry data to use. E.g., 'area' or 'thickness'. Pass NULL to render the surface in white, without any data. One can also pass the pre-loaded morphometry data as a numerical vector, the length of which must match the number of surface vertices.
#'
#' @param hemi string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param surface character string or `fs.surface` instance. The display surface. E.g., "white", "pial", or "inflated". Defaults to "white".
#'
#' @param colormap a colormap function. See the squash package for some colormaps. Defaults to \code{\link[squash]{jet}}.
#'
#' @param clip numeric vector of length 2 or NULL. If given, the 2 values are interpreted as lower and upper percentiles, and the morph data is clipped at the given lower and upper percentile (see [fsbrain::clip.data()]). Defaults to NULL (no data clipping).
#'
#' @param cortex_only logical, whether to mask the medial wall, i.e., whether the morphometry data for all vertices which are *not* part of the cortex (as defined by the label file `label/?h.cortex.label`) should be replaced with NA values. In other words, setting this to TRUE will ignore the values of the medial wall between the two hemispheres. If set to true, the mentioned label file needs to exist for the subject. Defaults to FALSE.
#'
#' @param makecmap_options named list of parameters to pass to \code{\link[squash]{makecmap}}. Must not include the unnamed first parameter, which is derived from 'measure'.
#'
#' @return coloredmesh. A named list with entries: "mesh" the \code{\link[rgl]{tmesh3d}} mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @export
#' @importFrom squash cmap makecmap jet
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.morph.native <- function(subjects_dir, subject_id, measure, hemi, surface="white", colormap=NULL, clip=NULL, cortex_only=FALSE, makecmap_options=list('colFn'=squash::jet)) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    makecmap_options = makecmakeopts.merge(makecmap_options, colormap);

    if(is.null(measure)) {
        morph_data = NULL;
    } else {
        if(is.numeric(measure)) {
            morph_data = measure;
        } else {
            morph_data = subject.morph.native(subjects_dir, subject_id, measure, hemi, cortex_only=cortex_only);
        }
    }

    if(! is.null(clip)) {
        morph_data = clip.data(morph_data, lower=clip[1], upper=clip[2]);
    }

    if(freesurferformats::is.fs.surface(surface)) {
        surface_mesh = surface;
    } else {
        surface_mesh = subject.surface(subjects_dir, subject_id, surface, hemi);
    }

    if(nrow(surface_mesh$vertices) != length(morph_data)) {
        warning(sprintf("Data mismatch: surface has %d vertices, but %d color values passed in argument 'measure'.\n", nrow(surface_mesh$vertices), length(morph_data)));
    }

    mesh = rgl::tmesh3d(c(t(surface_mesh$vertices)), c(t(surface_mesh$faces)), homogeneous=FALSE);
    col = squash::cmap(morph_data, map = do.call(squash::makecmap, utils::modifyList(list(morph_data), makecmap_options)));

    return(fs.coloredmesh(mesh, col, hemi, "morph_data"=morph_data, "cmap_fun"=makecmap_options$colFn));
}


#' @title Create a coloredmesh from a mesh and pre-defined colors.
#'
#' @inheritParams coloredmesh.from.morph.native
#'
#' @param color_data vector of hex color strings
#'
#' @return coloredmesh. A named list with entries: "mesh" the \code{\link[rgl]{tmesh3d}} mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @note Do not call this, use \code{\link[fsbrain]{coloredmeshes.from.color}} instead.
#'
#' @keywords internal
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.color <- function(subjects_dir, subject_id, color_data, hemi, surface="white") {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    if(freesurferformats::is.fs.surface(surface)) {
        surface_mesh = surface;
    } else {
        surface_mesh = subject.surface(subjects_dir, subject_id, surface, hemi);
    }
    mesh = rgl::tmesh3d(c(t(surface_mesh$vertices)), c(t(surface_mesh$faces)), homogeneous=FALSE);

    if(nrow(surface_mesh$vertices) != length(color_data)) {
        if(length(color_data) == 1L) {
            color_data = rep(color_data, nrow(surface_mesh$vertices));
        } else {
            warning(sprintf("Data mismatch: surface has %d vertices, but %d color values passed in argument 'color_data'.\n", nrow(surface_mesh$vertices), length(color_data)));
        }
    }

    return(fs.coloredmesh(mesh, color_data, hemi));
}


#' @title Create coloredmeshes for both hemis using pre-defined colors.
#'
#' @inheritParams coloredmesh.from.morph.native
#'
#' @param color_data named list with names 'lh' and 'rh', each entry must be a vector of hex color strings
#'
#' @return named list of coloredmeshes. Each entry is a named list with entries: "mesh" the \code{\link[rgl]{tmesh3d}} mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
coloredmeshes.from.color <- function(subjects_dir, subject_id, color_data, hemi, surface="white") {
    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh', or 'both' but is '%s'.\n", hemi));
    }

    if(hemi=="both") {
        if(! is.hemilist(color_data)) {
            stop("The parameter 'color_data' must be a named list with entries 'lh' and 'rh' if 'hemi' is 'both'.");
        }
        lh_cm = coloredmesh.from.color(subjects_dir, subject_id, color_data$lh, 'lh', surface=surface);
        rh_cm = coloredmesh.from.color(subjects_dir, subject_id, color_data$rh, 'rh', surface=surface);
        return(list("lh"=lh_cm, "rh"=rh_cm));
    } else {
        if(is.hemilist(color_data)) {
            color_data = hemilist.unwrap(color_data);
        }
        cm = coloredmesh.from.color(subjects_dir, subject_id, color_data, hemi, surface=surface);
        return(hemilist.wrap(cm, hemi));
    }
}


#' @title Create a coloredmesh from standard space morphometry data.
#'
#' @inheritParams coloredmesh.from.morph.native
#'
#' @param fwhm string, smoothing setting. The smoothing part of the filename, typically something like '0', '5', '10', ...,  or '25'.
#'
#' @param template_subject The template subject used. This will be used as part of the filename, and its surfaces are loaded for data visualization. Defaults to 'fsaverage'.
#
#' @param template_subjects_dir The template subjects dir. If NULL, the value of the parameter 'subjects_dir' is used. Defaults to NULL. If you have FreeSurfer installed and configured, and are using the standard fsaverage subject, try passing the result of calling 'file.path(Sys.getenv('FREESURFER_HOME'), 'subjects')'.
#'
#' @return coloredmesh. A named list with entries: "mesh" the [rgl::tmesh3d()] mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @keywords internal
#' @importFrom squash cmap makecmap jet
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.morph.standard <- function(subjects_dir, subject_id, measure, hemi, fwhm, surface="white", template_subject='fsaverage', template_subjects_dir=NULL, colormap=NULL, clip = NULL, cortex_only=FALSE, makecmap_options=list('colFn'=squash::jet)) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    makecmap_options = makecmakeopts.merge(makecmap_options, colormap);

    if(is.null(template_subjects_dir)) {
        template_subjects_dir = subjects_dir;
    }

    if(is.null(measure)) {
        morph_data = NULL;
    } else {
        if(is.numeric(measure)) {
            morph_data = measure;
        } else {
            morph_data = subject.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm = fwhm, cortex_only = cortex_only);
        }
    }

    if(! is.null(clip)) {
        morph_data = clip.data(morph_data, lower=clip[1], upper=clip[2]);
    }

    if(freesurferformats::is.fs.surface(surface)) {
        surface_mesh = surface;
    } else {
        surface_mesh = subject.surface(template_subjects_dir, template_subject, surface, hemi);
    }

    if(nrow(surface_mesh$vertices) != length(morph_data)) {
        warning(sprintf("Data mismatch: surface has %d vertices, but %d morphometry values passed in argument 'measure'.\n", nrow(surface_mesh$vertices), length(morph_data)));
    }

    mesh = rgl::tmesh3d(c(t(surface_mesh$vertices)), c(t(surface_mesh$faces)), homogeneous=FALSE);

    if(is.null(morph_data)) {
        col = 'white';
    } else {
        col = squash::cmap(morph_data, map = do.call(squash::makecmap, utils::modifyList(list(morph_data), makecmap_options)));
    }
    return(fs.coloredmesh(mesh, col, hemi, "morph_data"=morph_data, "cmap_fun"=makecmap_options$colFn));
}


#' @title Create a coloredmesh from arbitrary data.
#'
#' @inheritParams coloredmesh.from.morph.native
#'
#' @param vis_subject_id string. The subject identifier from which to obtain the surface for data visualization. Example: 'fsaverage'.
#'
#' @param morph_data string. The morphometry data to use. E.g., 'area' or 'thickness.'
#'
#' @param all_nan_backup_value numeric. If all morph_data values are NA/NaN, no color map can be created. In that case, the values are replaced by this value, and this is indicated in the entry morph_data_was_all_na in the return value. Defaults to 0.0.
#'
#' @return coloredmesh. A named list with entries: "mesh" the \code{\link[rgl]{tmesh3d}} mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @keywords internal
#' @importFrom squash cmap makecmap jet
#' @importFrom rgl tmesh3d rgl.open wire3d
#' @importFrom utils modifyList
coloredmesh.from.morphdata <- function(subjects_dir, vis_subject_id, morph_data, hemi, surface="white", colormap=NULL, all_nan_backup_value = 0.0, makecmap_options=list('colFn'=squash::jet)) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    makecmap_options = makecmakeopts.merge(makecmap_options, colormap);

    if(freesurferformats::is.fs.surface(surface)) {
        surface_data = surface;
    } else {
        surface_data = subject.surface(subjects_dir, vis_subject_id, surface, hemi);
    }

    num_verts = nrow(surface_data$vertices);
    if(length(morph_data) != num_verts) {
        warning(sprintf("Received %d data values, but the hemi '%s' '%s' surface of visualization subject '%s' in dir '%s' has %d vertices. Counts must match.\n", length(morph_data), hemi, surface, vis_subject_id, subjects_dir, num_verts));
    }

    mesh = rgl::tmesh3d(c(t(surface_data$vertices)), c(t(surface_data$faces)), homogeneous=FALSE);

    # If all values are NaN, the following call to squash::cmap fails with an error. We reset the data here to avoid that.
    morph_data_was_all_na = FALSE;
    if(all(is.na(morph_data))) {
        morph_data = as.vector(rep(all_nan_backup_value, length(morph_data)));
        morph_data_was_all_na = TRUE;
    }

    col = squash::cmap(morph_data, map = do.call(squash::makecmap, utils::modifyList(list(morph_data), makecmap_options)));
    return(fs.coloredmesh(mesh, col, hemi, render=!morph_data_was_all_na, morph_data = morph_data, "cmap_fun"=makecmap_options$colFn));
}



#' @title Create a coloredmesh from an annotation of an atlas.
#'
#' @inheritParams coloredmesh.from.morph.native
#'
#' @param atlas string or a loaded annotation. If a string, interpreted as the atlas name that should be loaded to get te annotation. E.g., "aparc", "aparc.2009s", or "aparc.DKTatlas". Used to construct the name of the annotation file to be loaded.
#'
#' @param outline logical, whether to draw an outline only instead of filling the regions. Defaults to FALSE. Only makes sense if you did not pass an outline already. The current implementation for outline computation is rather slow, so setting this to TRUE will considerably increase computation time.
#'
#' @return coloredmesh. A named list with entries: "mesh" the \code{\link[rgl]{tmesh3d}} mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @keywords internal
#' @importFrom squash cmap makecmap jet
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.annot <- function(subjects_dir, subject_id, atlas, hemi, surface="white", outline=FALSE) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    if(freesurferformats::is.fs.surface(surface)) {
        surface_mesh = surface;
    } else {
        surface_mesh = subject.surface(subjects_dir, subject_id, surface, hemi);
    }

    if(is.character(atlas)) {
        annot = subject.annot(subjects_dir, subject_id, hemi, atlas);
    } else {
        annot = atlas;
    }
    mesh = rgl::tmesh3d(c(t(surface_mesh$vertices)), c(t(surface_mesh$faces)), homogeneous=FALSE);
    if(outline) {
        col = annot.outline(annot, surface_mesh);
    } else {
        col = annot$hex_colors_rgb;
    }

    if(nrow(surface_mesh$vertices) != length(col)) {
        warning(sprintf("Data mismatch: surface has %d vertices, but %d color values received from annotation.\n", nrow(surface_mesh$vertices), length(col)));
    }

    return(fs.coloredmesh(mesh, col, hemi));
}



#' @title Create a coloredmesh from a label.
#'
#' @inheritParams coloredmesh.from.morph.native
#'
#' @param label string or vector of integers. If a string, the name of the label file, without the hemi part (if any), but including the '.label' suffix. E.g., 'cortex.label' for '?h.cortex.label'. Alternatively, the already loaded label data as a vector of integers.
#'
#' @return coloredmesh. A named list with entries: "mesh" the \code{\link[rgl]{tmesh3d}} mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @keywords internal
#' @importFrom squash cmap makecmap rainbow2
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.label <- function(subjects_dir, subject_id, label, hemi, surface="white", colormap=NULL, makecmap_options=list('colFn'=squash::rainbow2)) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    makecmap_options = makecmakeopts.merge(makecmap_options, colormap);

    if(freesurferformats::is.fs.surface(surface)) {
        surface_data = surface;
    } else {
        surface_data = subject.surface(subjects_dir, subject_id, surface, hemi);
    }

    if(is.character(label)) {
        label_data = subject.label(subjects_dir, subject_id, label, hemi);
    } else {
        label_data = label;
    }

    mask = mask.from.labeldata.for.hemi(list(label_data), nrow(surface_data$vertices));
    return(coloredmesh.from.mask(subjects_dir, subject_id, mask, hemi, surface=surface, makecmap_options=makecmap_options, surface_data=surface_data));
}



#' @title Create a coloredmesh from a mask.
#'
#' @inheritParams coloredmesh.from.morph.native
#'
#' @param mask logical vector, contains one logical value per vertex.
#'
#' @param surface_data optional surface object, as returned by \code{\link[fsbrain]{subject.surface}}. If given, used instead of loading the surface data from disk (which users of this function may already have done). Defaults to NULL.
#'
#' @return coloredmesh. A named list with entries: "mesh" the \code{\link[rgl]{tmesh3d}} mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @family mask functions
#' @export
coloredmesh.from.mask <- function(subjects_dir, subject_id, mask, hemi, surface="white", colormap=NULL, surface_data=NULL, makecmap_options=list('colFn'=squash::rainbow2)) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    makecmap_options = makecmakeopts.merge(makecmap_options, colormap);

    if(is.null(surface_data)) {
        if(freesurferformats::is.fs.surface(surface)) {
            surface_data = surface;
        } else {
            surface_data = subject.surface(subjects_dir, subject_id, surface, hemi);
        }
    }

    morph_like_data = as.integer(mask);

    if(min(mask) < 0L | max(mask) > 1L) {
        warning(sprintf("The data range of the supplied mask is outside of the expected range [0L, 1L]. Is this really a mask?\n", length(mask), nrow(surface_data$vertices)));
    }

    morph_like_data[mask == 1L] = NA;     # set positive values to NA so they get rendered as background.

    if(length(mask) != nrow(surface_data$vertices)) {
        warning(sprintf("The length of the supplied mask (%d) does not match the number of vertices in the surface (%d).\n", length(mask), nrow(surface_data$vertices)));
    }

    mesh = rgl::tmesh3d(c(t(surface_data$vertices)), c(t(surface_data$faces)), homogeneous=FALSE);
    col = squash::cmap(morph_like_data, map = do.call(squash::makecmap, utils::modifyList(list(morph_like_data), makecmap_options)));
    return(fs.coloredmesh(mesh, col, hemi, "morph_data"=morph_like_data, "cmap_fun"=makecmap_options$colFn, "data_range"=c(0L, 1L)));
}


#' @title Print description of a brain coloredmesh (S3).
#'
#' @param x brain surface with class `fs.coloredmesh`.
#'
#' @param ... further arguments passed to or from other methods
#'
#' @export
print.fs.coloredmesh <- function(x, ...) {
    cat(sprintf("Brain coloredmesh with %d vertices and %d faces.\n", ncol(x$mesh$vb), ncol(x$mesh$it)));
    cat(sprintf(" * Hemi is '%s', will be rendered: %s.\n", x$hemi, !x$morph_data_was_all_na));
    cat(sprintf(" * Contains %d color values, %d unique colors.\n", length(x$col), length(unique(x$col))));
}


#' @title Check whether object is an fs.coloredmesh (S3)
#'
#' @param x any `R` object
#'
#' @return TRUE if its argument is a coloredmesh (that is, has "fs.coloredmesh" amongst its classes) and FALSE otherwise.
#'
#' @export
is.fs.coloredmesh <- function(x) inherits(x, "fs.coloredmesh")



#' @title fs.coloredmesh constructor
#'
#' @param mesh a `mesh3d` instance as returned by \code{\link[rgl]{tmesh3d}} or an `fs.surface` brain surface mesh as returned by functions like \code{\link[fsbrain]{subject.surface}}.
#'
#' @param col vector of vertex colors for the mesh, one color per vertex
#'
#' @param hemi character string, one of 'lh' or 'rh'
#'
#' @param render logical, whether to render this mesh during visualization
#'
#' @param morph_data optional, the data used to construct the 'col' values
#'
#' @param cmap_fun optional, the colormap function used to construct the 'col' values
#'
#' @param data_range optional, the range of the data used to construct the 'col' values. Having this is useful for plotting a colorbar during visualization.
#'
#' @return an `fs.coloredmesh` instance. The only fields one should use in client code are 'mesh', 'hemi' and 'col', all others are considered internal and may change without notice.
#'
#' @importFrom freesurferformats is.fs.surface
#' @importFrom rgl tmesh3d
#' @export
fs.coloredmesh <- function(mesh, col, hemi, render=TRUE, morph_data=NULL, cmap_fun=NULL, data_range=NULL) {
    if(freesurferformats::is.fs.surface(mesh)) {
        mesh = rgl::tmesh3d(c(t(mesh$vertices)), c(t(mesh$faces)), homogeneous=FALSE);
    }
    if(!inherits(mesh, "mesh3d")) {
        stop("Parameter 'mesh' must be a mesh3d or fs.surface instance.");
    }
    if(ncol(mesh$vb) != length(col)) {
        warning(sprintf("The mesh3d instance from parameter 'mesh' has %d vertices, but %d colors passed in parameter 'col'.\n", ncol(mesh$vb), length(col)));
    }
    if(!is.null(hemi)) {
        if(!(hemi %in% c("lh", "rh"))) {
            stop("Parameter 'hemi' must be 'lh', 'rh', or NULL.");
        }
    }
    if(is.null(data_range)) {
        if(!is.null(morph_data)) {
            data_range = range(morph_data, finite=TRUE);
        }
    }
    cm = list("mesh"=mesh, "col"=col, "morph_data_was_all_na"=!render, "render"=render, "hemi"=hemi, "morph_data"=morph_data, "cmap_fun"=cmap_fun, "data_range"=data_range);
    class(cm) = c("fs.coloredmesh", class(cm));
    return(cm);
}


