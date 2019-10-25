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
#' @param colormap, a colormap. See the squash package for some colormaps. Defaults to squash::jet.
#'
#' @param views, list of strings. Valid entries include: 'si': single interactive view. 't4': tiled view showing the brain from 4 angles. 't9': tiled view showing the brain from 9 angles.
#'
#' @param rgloptions option list passed to [rgl::par3d()]. Example: rgloptions = list("windowRect"=c(50,50,1000,1000));
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
vis.subject.morph.native <- function(subjects_dir, subject_id, measure, hemi, surface="white", colormap=squash::jet, views=c("t4"), rgloptions = list()) {

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

    invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions));
}


#' @title Show one or more views of the given meshes in rgl windows.
#'
#' @param views, list of strings. Valid entries include: 'si': single interactive view. 't4': tiled view showing the brain from 4 angles. 't9': tiled view showing the brain from 9 angles.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @param rgloptions option list passed to [rgl::par3d()]. Example: rgloptions = list("windowRect"=c(50,50,1000,1000));
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @keywords internal
brainviews <- function(views, coloredmeshes, rgloptions = list()) {
    if(length(views)) {
        for(view in views) {
            if(view == "t4") {
                invisible(brainview.t4(coloredmeshes, rgloptions = rgloptions));
            } else if(view == "t9") {
                invisible(brainview.t9(coloredmeshes, rgloptions = rgloptions));
            } else if(view == "si") {
                invisible(brainview.si(coloredmeshes, rgloptions = rgloptions));
            } else if(view == "sr") {
                invisible(brainview.sr(coloredmeshes, rgloptions = rgloptions));
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
#' @param colormap, a colormap. See the squash package for some colormaps. Defaults to squash::jet.
#'
#' @param views, list of strings. Valid entries include: 'si': single interactive view. 't4': tiled view showing the brain from 4 angles. 't9': tiled view showing the brain from 9 angles.
#'
#' @param rgloptions option list passed to [rgl::par3d()]. Example: rgloptions = list("windowRect"=c(50,50,1000,1000));
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
vis.data.on.subject <- function(subjects_dir, vis_subject_id, morph_data_lh, morph_data_rh, surface="white", colormap=squash::jet, views=c('t4'), rgloptions=list()) {

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

    invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions));
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
#' @param colormap, a colormap. See the squash package for some colormaps. Defaults to squash::jet.
#'
#' @param views, list of strings. Valid entries include: 'si': single interactive view. 't4': tiled view showing the brain from 4 angles. 't9': tiled view showing the brain from 9 angles.
#'
#' @param rgloptions option list passed to [rgl::par3d()]. Example: rgloptions = list("windowRect"=c(50,50,1000,1000));
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @family visualization functions
#'
#' @importFrom squash jet
#' @export
vis.data.on.fsaverage <- function(subjects_dir=NULL, vis_subject_id="fsaverage", morph_data_lh, morph_data_rh, surface="white", colormap=squash::jet, views=c('t4'), rgloptions = list()) {

    if(is.null(subjects_dir)) {
        subjects_dir = find.subjectsdir.of(subject_id=vis_subject_id, mustWork = TRUE);
    }

    invisible(vis.data.on.subject(subjects_dir, vis_subject_id, morph_data_lh, morph_data_rh, surface=surface, colormap=colormap, views=views, rgloptions=rgloptions));
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
vis.subject.annot <- function(subjects_dir, subject_id, atlas, hemi, surface="white", views=c('t4'), rgloptions=list()) {

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

    invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions));
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
#' @param colormap, a colormap. See the squash package for some colormaps. Defaults to squash::jet.
#'
#' @return coloredmesh. A named list with entries: "mesh" the rgl::tmesh3d mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
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
    return(list("mesh"=mesh, "col"=col, "morph_data_was_all_na"=FALSE, "hemi"=hemi));
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
#' @param colormap, a colormap. See the squash package for some colormaps. Defaults to squash::jet.
#'
#' @param all_nan_backup_value, numeric. If all morph_data values are NA/NaN, no color map can be created. In that case, the values are replaced by this value, and this is indicated in the entry morph_data_was_all_na in the return value. Defaults to 0.0.
#'
#' @return coloredmesh. A named list with entries: "mesh" the rgl::tmesh3d mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
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
    return(list("mesh"=mesh, "col"=col, "morph_data_was_all_na"=morph_data_was_all_na, "hemi"=hemi));
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
#' @param colormap, a colormap. See the squash package for some colormaps. Defaults to squash::jet.
#'
#' @return coloredmesh. A named list with entries: "mesh" the rgl::tmesh3d mesh object. "col": the mesh colors. "morph_data_was_all_na", logical. Whether the mesh values were all NA, and thus replaced by the all_nan_backup_value. "hemi": the hemisphere, one of 'lh' or 'rh'.
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
    return(list("mesh"=mesh, "col"=col, "morph_data_was_all_na"=FALSE, "hemi"=hemi));
}
