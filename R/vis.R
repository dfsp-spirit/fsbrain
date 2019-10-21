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
#' @importFrom squash jet
#' @export
vis.subject.morph.native <- function(subjects_dir, subject_id, measure, hemi, surface="white", colormap=squash::jet) {

    if(hemi == "both") {
        lh_cmesh = coloredmesh.from.morph.native(subjects_dir, subject_id, measure, 'lh', surface=surface, colormap=colormap);
        rh_cmesh = coloredmesh.from.morph.native(subjects_dir, subject_id, measure, 'rh', surface=surface, colormap=colormap);
        coloredmeshes = list(lh_cmesh, rh_cmesh);
    } else {
        cmesh = coloredmesh.from.morph.native(subjects_dir, subject_id, measure, hemi, surface=surface, colormap=colormap);
        coloredmeshes = list(cmesh);
    }
    vis.coloredmeshes(coloredmeshes);
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
#' @importFrom squash jet
#' @export
vis.data.on <- function(subjects_dir, vis_subject_id, morph_data_lh, morph_data_rh, surface="white", colormap=squash::jet) {

    if(is.null(morph_data_lh) && is.null(morph_data_rh)) {
        stop(sprintf("Only one of morph_data_lh or morph_data_rh can be NULL."));
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

    vis.coloredmeshes(coloredmeshes);
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
#' @export
vis.subject.annot <- function(subjects_dir, subject_id, atlas, hemi, surface="white") {

    if(hemi == "both") {
        lh_cmesh = coloredmesh.from.annot(subjects_dir, subject_id, atlas, 'lh', surface=surface);
        rh_cmesh = coloredmesh.from.annot(subjects_dir, subject_id, atlas, 'rh', surface=surface);
        coloredmeshes = list(lh_cmesh, rh_cmesh);
    } else {
        cmesh = coloredmesh.from.annot(subjects_dir, subject_id, atlas, hemi, surface=surface);
        coloredmeshes = list(cmesh);
    }
    vis.coloredmeshes(coloredmeshes);
}



#' @title Visualize a list of colored meshes in a single scene.
#'
#' @param coloredmeshes, list of coloredmesh. A coloredmesh is a named list as returned by the coloredmesh.from.* functions. It has the entries 'mesh' of type tmesh3d, a 'col', which is a color specification for such a mesh.
#'
#' @keywords internal
vis.coloredmeshes <- function(coloredmeshes) {

    if(!is.list(coloredmeshes)) {
        stop("Parameter coloredmeshes must be a list.");
    }

    rgl.open();
    for(cmesh in coloredmeshes) {
        wire3d(cmesh$mesh, col=cmesh$col);
    }
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
#' @keywords internal
#' @importFrom squash cmap makecmap jet
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.morph.native <- function(subjects_dir, subject_id, measure, hemi, surface="white", colormap=squash::jet) {
    morph_data = subject.morph.native(subjects_dir, subject_id, measure, hemi);
    surface_data = subject.surface(subjects_dir, subject_id, surface, hemi);
    mesh = rgl::tmesh3d(unlist(surface_data$vertices), unlist(surface_data$faces), homogeneous=FALSE);
    col = squash::cmap(morph_data, map = squash::makecmap(morph_data, colFn = colormap));
    return(list("mesh"=mesh, "col"=col));
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
#' @keywords internal
#' @importFrom squash cmap makecmap jet
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.morphdata <- function(subjects_dir, vis_subject_id, morph_data, hemi, surface="white", colormap=squash::jet) {
    surface_data = subject.surface(subjects_dir, vis_subject_id, surface, hemi);

    num_verts = nrow(surface_data$vertices);
    if(length(morph_data) != num_verts) {
        stop(sprintf("Received %d data values, but the hemi '%s' '%s' surface of visualization subject '%s' in dir '%s' has %d vertices. Counts must match.\n", length(morph_data), hemi, surface, vis_subject_id, subjects_dir, num_verts));
    }

    mesh = rgl::tmesh3d(unlist(surface_data$vertices), unlist(surface_data$faces), homogeneous=FALSE);
    col = squash::cmap(morph_data, map = squash::makecmap(morph_data, colFn = colormap));
    return(list("mesh"=mesh, "col"=col));
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
#' @keywords internal
#' @importFrom squash cmap makecmap jet
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.annot <- function(subjects_dir, subject_id, atlas, hemi, surface="white", colormap=squash::jet) {
    surface_data = subject.surface(subjects_dir, subject_id, surface, hemi);
    annot = subject.annot(subjects_dir, subject_id, hemi, atlas);
    mesh = rgl::tmesh3d(unlist(surface_data$vertices), unlist(surface_data$faces), homogeneous=FALSE);
    col = annot$hex_colors_rgb;
    return(list("mesh"=mesh, "col"=col));
}
