# Functions for the interactive visualization of morphometry and atlas data (on subject level).


#' @title Visualize native space morphometry data for a subject.
#'
#' @description Creates a surface mesh, applies a colormap transform the morphometry data values into colors, and renders the resulting colored mesh in an interactive window. If hemi is 'both', the data is rendered for the wholw brain.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier.
#'
#' @param measure, string. The morphometry data to use. E.g., 'area' or 'thickness.'
#'
#' @param hemi, string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param surface, string. The display surface. E.g., "white", "pial", or "inflated". Defaults to "white".
#'
#' @param colormap, a colormap. See the squash package for some colormaps. Defaults to [squash::jet].
#'
#' @param views, list of strings. Valid entries include: 'si': single interactive view. 't4': tiled view showing the brain from 4 angles. 't9': tiled view showing the brain from 9 angles.
#'
#' @param rgloptions option list passed to [rgl::par3d()]. Example: rgloptions = list("windowRect"=c(50,50,1000,1000));
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    vis.subject.morph.native(subjects_dir, 'subject1', 'thickness', 'lh');
#' }
#'
#' @family visualization functions
#'
#' @importFrom squash jet
#' @export
vis.subject.morph.native <- function(subjects_dir, subject_id, measure, hemi, surface="white", colormap=squash::jet, views=c("t4"), rgloptions = list(), rglactions = list()) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(hemi == "both") {
        lh_cmesh = coloredmesh.from.morph.native(subjects_dir, subject_id, measure, 'lh', surface=surface, colormap=colormap);
        rh_cmesh = coloredmesh.from.morph.native(subjects_dir, subject_id, measure, 'rh', surface=surface, colormap=colormap);
        coloredmeshes = list(lh_cmesh, rh_cmesh);
    } else {
        cmesh = coloredmesh.from.morph.native(subjects_dir, subject_id, measure, hemi, surface=surface, colormap=colormap);
        coloredmeshes = list(cmesh);
    }

    invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions));
}


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



#' @title Visualize native space morphometry data for a subject.
#'
#' @description Creates a surface mesh, applies a colormap transform the morphometry data values into colors, and renders the resulting colored mesh in an interactive window. If hemi is 'both', the data is rendered for the wholw brain.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier.
#'
#' @param measure, string. The morphometry data to use. E.g., 'area' or 'thickness.'
#'
#' @param hemi, string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param fwhm, string, smoothing setting. The smoothing part of the filename, typically something like '0', '5', '10', ...,  or '25'.
#'
#' @param surface, string. The display surface. E.g., "white", "pial", or "inflated". Defaults to "white".
#'
#' @param template_subject The template subject used. This will be used as part of the filename, and its surfaces are loaded for data visualization. Defaults to 'fsaverage'.
#'
#' @param template_subjects_dir The template subjects dir. If NULL, the value of the parameter 'subjects_dir' is used. Defaults to NULL. If you have FreeSurfer installed and configured, and are using the standard fsaverage subject, try passing the result of calling 'file.path(Sys.getenv('FREESURFER_HOME'), 'subjects')'.
#'
#' @param colormap, a colormap. See the squash package for some colormaps. Defaults to [squash::jet].
#'
#' @param views, list of strings. Valid entries include: 'si': single interactive view. 't4': tiled view showing the brain from 4 angles. 't9': tiled view showing the brain from 9 angles.
#'
#' @param rgloptions option list passed to [rgl::par3d()]. Example: rgloptions = list("windowRect"=c(50,50,1000,1000));
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    fsaverage_dir = file.path(Sys.getenv('FREESURFER_HOME'), 'subjects');
#'    if(dir.exists(fsaverage_dir)) {
#'        vis.subject.morph.standard(subjects_dir, 'subject1', 'thickness', 'lh',
#'        '10', template_subjects_dir=fsaverage_dir);
#'    }
#'    # The last command will load the file
#'    #  *<subjects_dir>/subject1/surf/lh.thickness.fwhm10.fsaverage.mgh* and
#'    #  visualize the data on *$FREESURFER_HOME/subjects/fsaverage/surf/lh.white*.
#' }
#'
#' @family visualization functions
#'
#' @importFrom squash jet
#' @export
vis.subject.morph.standard <- function(subjects_dir, subject_id, measure, hemi, fwhm, surface="white", template_subject = 'fsaverage', template_subjects_dir = NULL, colormap=squash::jet, views=c("t4"), rgloptions = list(), rglactions = list()) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(is.null(template_subjects_dir)) {
        template_subjects_dir = subjects_dir;
    }

    if(hemi == "both") {
        lh_cmesh = coloredmesh.from.morph.standard(subjects_dir, subject_id, measure, 'lh', fwhm, surface=surface, template_subject=template_subject, template_subjects_dir=template_subjects_dir, colormap=colormap);
        rh_cmesh = coloredmesh.from.morph.standard(subjects_dir, subject_id, measure, 'rh', fwhm, surface=surface, template_subject=template_subject, template_subjects_dir=template_subjects_dir, colormap=colormap);
        coloredmeshes = list(lh_cmesh, rh_cmesh);
    } else {
        cmesh = coloredmesh.from.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm, surface=surface, template_subject=template_subject, template_subjects_dir=template_subjects_dir, colormap=colormap);
        coloredmeshes = list(cmesh);
    }

    invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions));
}


#' @title Show one or more views of the given meshes in rgl windows.
#'
#' @param views, list of strings. Valid entries include: 'si': single interactive view. 'sr': single rotating view. 't4': tiled view showing the brain from 4 angles. 't9': tiled view showing the brain from 9 angles.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param rgloptions option list passed to [rgl::par3d()]. Example: rgloptions = list("windowRect"=c(50,50,1000,1000));
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @keywords internal
brainviews <- function(views, coloredmeshes, rgloptions = list(), rglactions = list()) {
    if(length(views)) {
        for(view in views) {
            if(view == "t4") {
                invisible(brainview.t4(coloredmeshes, rgloptions = rgloptions, rglactions = rglactions));
            } else if(view == "t9") {
                invisible(brainview.t9(coloredmeshes, rgloptions = rgloptions, rglactions = rglactions));
            } else if(view == "si") {
                invisible(brainview.si(coloredmeshes, rgloptions = rgloptions, rglactions = rglactions));
            } else if(view == "sr") {
                invisible(brainview.sr(coloredmeshes, rgloptions = rgloptions, rglactions = rglactions));
            } else {
                stop(sprintf("Invalid view '%s'. Valid ones include 'si', t4' and 't9'.\n", view));
            }
        }
    } else {
        invisible(coloredmeshes);
    }
}


#' @title Visualize arbitrary data on the surface of any subject.
#'
#' @description Creates a surface mesh, applies a colormap transform the morphometry data values into colors, and renders the resulting colored mesh in an interactive window. If hemi is 'both', the data is rendered for the wholw brain.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, containing the subdir of vis_subject_id, the subject that you want to use for visualization.
#'
#' @param vis_subject_id, string. The subject identifier from which to obtain the surface for data visualization. Example: 'fsaverage'.
#'
#' @param morph_data_lh, numeric vector or NULL, the data to visualize on the left hemisphere surface. Must have the same length as the surface of the vis_subject_id has vertices. If NULL, this surface will not be rendered. Only one of morph_data_lh or morph_data_rh is allowed to be NULL.
#'
#' @param morph_data_rh, numeric vector or NULL, the data to visualize on the right hemisphere surface. Must have the same length as the surface of the vis_subject_id has vertices. If NULL, this surface will not be rendered. Only one of morph_data_lh or morph_data_rh is allowed to be NULL.
#'
#' @param surface, string. The display surface. E.g., "white", "pial", or "inflated". Defaults to "white".
#'
#' @param colormap, a colormap. See the squash package for some colormaps. Defaults to [squash::jet].
#'
#' @param views, list of strings. Valid entries include: 'si': single interactive view. 't4': tiled view showing the brain from 4 angles. 't9': tiled view showing the brain from 9 angles.
#'
#' @param rgloptions option list passed to [rgl::par3d()]. Example: rgloptions = list("windowRect"=c(50,50,1000,1000));
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    morph_data_lh = subject.morph.native(subjects_dir, 'subject1', 'thickness', 'lh');
#'    morph_data_rh = NULL;
#'    vis.data.on.subject(subjects_dir, 'subject1', morph_data_lh, morph_data_rh);
#' }
#'
#' @family visualization functions
#'
#' @importFrom squash jet
#' @export
vis.data.on.subject <- function(subjects_dir, vis_subject_id, morph_data_lh, morph_data_rh, surface="white", colormap=squash::jet, views=c('t4'), rgloptions=list(), rglactions = list()) {

    if(is.null(morph_data_lh) && is.null(morph_data_rh)) {
        stop(sprintf("Only one of morph_data_lh or morph_data_rh can be NULL.\n"));
    }

    coloredmeshes = list();

    if(! is.null(morph_data_lh)) {
        cmesh_lh = coloredmesh.from.morphdata(subjects_dir, vis_subject_id, morph_data_lh, 'lh', surface=surface, colormap=colormap);
        coloredmeshes$lh = cmesh_lh;
    }

    if(! is.null(morph_data_rh)) {
        cmesh_rh = coloredmesh.from.morphdata(subjects_dir, vis_subject_id, morph_data_rh, 'rh', surface=surface, colormap=colormap);
        coloredmeshes$rh = cmesh_rh;
    }

    invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions));
}


#' @title Visualize arbitrary data on the fsaverage template subject, if available.
#'
#' @description Creates a surface mesh, applies a colormap transform the morphometry data values into colors, and renders the resulting colored mesh in an interactive window. If hemi is 'both', the data is rendered for the whole brain. This function tries to automatically retrieve the subjects dir of the fsaverage template subject by checking the environment variables SUBJECTS_DIR and FREESURFER_HOME for the subject. The subject is required for its surfaces, which are not shipped with this package for licensing reasons.
#'
#' @param subjects_dir, string or NULL. The FreeSurfer SUBJECTS_DIR, containing the subdir of vis_subject_id, the subject that you want to use for visualization. If NULL, this function tries to determine it automatically from environment variables. If they are not set, an error is raised.
#'
#' @param vis_subject_id, string. The subject identifier from which to obtain the surface for data visualization. Defaults to 'fsaverage'.
#'
#' @param morph_data_lh, numeric vector or NULL, the data to visualize on the left hemisphere surface. Must have the same length as the lh surface of the vis_subject_id has vertices. If NULL, this surface will not be rendered. Only one of morph_data_lh or morph_data_rh is allowed to be NULL.
#'
#' @param morph_data_rh, numeric vector or NULL, the data to visualize on the right hemisphere surface. Must have the same length as the rh surface of the vis_subject_id has vertices. If NULL, this surface will not be rendered. Only one of morph_data_lh or morph_data_rh is allowed to be NULL.
#'
#' @param surface, string. The display surface. E.g., "white", "pial", or "inflated". Defaults to "white".
#'
#' @param colormap, a colormap. See the squash package for some colormaps. Defaults to [squash::jet].
#'
#' @param views, list of strings. Valid entries include: 'si': single interactive view. 't4': tiled view showing the brain from 4 angles. 't9': tiled view showing the brain from 9 angles.
#'
#' @param rgloptions option list passed to [rgl::par3d()]. Example: rgloptions = list("windowRect"=c(50,50,1000,1000));
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @family visualization functions
#'
#' @importFrom squash jet
#' @export
vis.data.on.fsaverage <- function(subjects_dir=NULL, vis_subject_id="fsaverage", morph_data_lh, morph_data_rh, surface="white", colormap=squash::jet, views=c('t4'), rgloptions = list(), rglactions = list()) {

    if(is.null(subjects_dir)) {
        subjects_dir = find.subjectsdir.of(subject_id=vis_subject_id, mustWork = TRUE);
    }

    invisible(vis.data.on.subject(subjects_dir, vis_subject_id, morph_data_lh, morph_data_rh, surface=surface, colormap=colormap, views=views, rgloptions=rgloptions, rglactions = rglactions));
}


#' @title Visualize an annotation for a subject.
#'
#' @description Creates a surface mesh, loads the colors from the annotation, and renders the resulting colored mesh in an interactive window. If hemi is 'both', the data is rendered for the wholw brain.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier.
#'
#' @param atlas, string. The atlas name. E.g., "aparc", "aparc.2009s", or "aparc.DKTatlas". Used to construct the name of the annotation file to be loaded.
#'
#' @param hemi, string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param surface, string. The display surface. E.g., "white", "pial", or "inflated". Defaults to "white".
#'
#' @param views, list of strings. Valid entries include: 'si': single interactive view. 't4': tiled view showing the brain from 4 angles. 't9': tiled view showing the brain from 9 angles.
#'
#' @param rgloptions option list passed to [rgl::par3d()]. Example: rgloptions = list("windowRect"=c(50,50,1000,1000));
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    vis.subject.annot(subjects_dir, 'subject1', 'aparc', 'both');
#' }
#'
#' @family visualization functions
#'
#' @export
vis.subject.annot <- function(subjects_dir, subject_id, atlas, hemi, surface="white", views=c('t4'), rgloptions=list(), rglactions = list()) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(hemi == "both") {
        lh_cmesh = coloredmesh.from.annot(subjects_dir, subject_id, atlas, 'lh', surface=surface);
        rh_cmesh = coloredmesh.from.annot(subjects_dir, subject_id, atlas, 'rh', surface=surface);
        coloredmeshes = list(lh_cmesh, rh_cmesh);
    } else {
        cmesh = coloredmesh.from.annot(subjects_dir, subject_id, atlas, hemi, surface=surface);
        coloredmeshes = list(cmesh);
    }

    invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions));
}



#' @title Visualize arbitrary data, one value per atlas region, on the surface of any subject (including template subjects).
#'
#' @description This function can be used for rendering a single value (color) for all vertices of an atlas region. The typical usecase is the visualization of results of atlas-based analyses, e.g., p-value, means or other aggregated values over all vertices of a region.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, containing the subdir of vis_subject_id, the subject that you want to use for visualization.
#'
#' @param subject_id, string. The subject identifier from which to obtain the surface for data visualization. Example: 'fsaverage'.
#'
#' @param atlas, string. The brain atlas to use. E.g., 'aparc' or 'aparc.a2009s'.
#'
#' @param lh_region_value_list, named list. A list for the left hemisphere in which the names are atlas regions, and the values are the value to write to all vertices of that region.
#'
#' @param rh_region_value_list, named list. A list for the right hemisphere in which the names are atlas regions, and the values are the value to write to all vertices of that region.
#'
#' @param surface, string. The display surface. E.g., "white", "pial", or "inflated". Defaults to "white".
#'
#' @param colormap, a colormap function. See the squash package for some colormaps. Defaults to [grDevices::heat.colors].
#'
#' @param views, list of strings. Valid entries include: 'si': single interactive view. 't4': tiled view showing the brain from 4 angles. 't9': tiled view showing the brain from 9 angles.
#'
#' @param rgloptions option list passed to [rgl::par3d()]. Example: rgloptions = list("windowRect"=c(50,50,1000,1000));
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @param value_for_unlisted_regions numerical scalar or NaN, the value to assign to regions which do not occur in the region_value_lists. Defaults to NaN.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    atlas = 'aparc';   # Desikan atlas
#'    # For the left hemisphere, we just assign a subset of the
#'    # atlas regions. The others will get the default value.
#'    lh_region_value_list = list("bankssts"=0.9, "precuneus"=0.7, "postcentral"=0.8, "lingual"=0.6);
#'    # For the right hemisphere, we retrieve the full list of regions for
#'    # the atlas, and assign random values to all of them.
#'    atlas_region_names = get.atlas.region.names(atlas, template_subjects_dir = subjects_dir,
#'     template_subject='subject1');
#'    rh_region_value_list = rnorm(length(atlas_region_names), 3.0, 1.0);
#'    names(rh_region_value_list) = atlas_region_names;
#'    vis.region.values.on.subject(subjects_dir, 'subject1', atlas,
#'     lh_region_value_list, rh_region_value_list);
#' }
#'
#' @family visualization functions
#'
#' @importFrom squash heat
#' @importFrom grDevices heat.colors
#' @export
vis.region.values.on.subject <- function(subjects_dir, subject_id, atlas, lh_region_value_list, rh_region_value_list, surface="white", colormap=grDevices::heat.colors, views=c('t4'), rgloptions=list(), rglactions = list(), value_for_unlisted_regions = NaN) {
    morph_like_data = spread.values.over.subject(subjects_dir, subject_id, atlas, lh_region_value_list, rh_region_value_list, value_for_unlisted_regions = value_for_unlisted_regions);
    invisible(vis.data.on.subject(subjects_dir, subject_id, morph_like_data$lh, morph_like_data$rh, surface=surface, colormap=colormap, views=views, rgloptions=rgloptions, rglactions=rglactions));
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
#' @return coloredmesh. A named list with entries: "mesh" the [rgl::tmesh3d] mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @keywords internal
#' @importFrom squash cmap makecmap jet
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.morph.native <- function(subjects_dir, subject_id, measure, hemi, surface="white", colormap=squash::jet) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    morph_data = subject.morph.native(subjects_dir, subject_id, measure, hemi);
    surface_data = subject.surface(subjects_dir, subject_id, surface, hemi);
    mesh = rgl::tmesh3d(unlist(surface_data$vertices), unlist(surface_data$faces), homogeneous=FALSE);
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
#' @return coloredmesh. A named list with entries: "mesh" the [rgl::tmesh3d] mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @keywords internal
#' @importFrom squash cmap makecmap jet
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.morph.standard <- function(subjects_dir, subject_id, measure, hemi, fwhm, surface="white", template_subject='fsaverage', template_subjects_dir=NULL, colormap=squash::jet) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    if(is.null(template_subjects_dir)) {
        template_subjects_dir = subjects_dir;
    }

    morph_data = subject.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm = fwhm);
    surface_data = subject.surface(template_subjects_dir, template_subject, surface, hemi);
    mesh = rgl::tmesh3d(unlist(surface_data$vertices), unlist(surface_data$faces), homogeneous=FALSE);
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

    mesh = rgl::tmesh3d(unlist(surface_data$vertices), unlist(surface_data$faces), homogeneous=FALSE);

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
    mesh = rgl::tmesh3d(unlist(surface_data$vertices), unlist(surface_data$faces), homogeneous=FALSE);
    col = annot$hex_colors_rgb;
    return(list("mesh"=mesh, "col"=col, "morph_data_was_all_na"=FALSE, "hemi"=hemi, "morph_data"=NULL, "cmap_fun"=colormap));
}
