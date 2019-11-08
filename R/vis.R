# Functions for the interactive visualization of morphometry and atlas data (on subject level).


#' @title Visualize native space morphometry data for a subject.
#'
#' @description Creates a surface mesh, applies a colormap transform the morphometry data values into colors, and renders the resulting colored mesh in an interactive window. If hemi is 'both', the data is rendered for the whole brain.
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
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
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
vis.subject.morph.native <- function(subjects_dir, subject_id, measure, hemi, surface="white", colormap=squash::jet, views=c("t4"), rgloptions = list(), rglactions = list(), draw_colorbar = FALSE) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }


    if(rglactions.has.key(rglactions, 'clip_data')) {
        clip = rglactions$clip_data;
    } else {
        clip = NULL;
    }


    if(hemi == "both") {
        lh_cmesh = coloredmesh.from.morph.native(subjects_dir, subject_id, measure, 'lh', surface=surface, colormap=colormap, clip = clip);
        rh_cmesh = coloredmesh.from.morph.native(subjects_dir, subject_id, measure, 'rh', surface=surface, colormap=colormap, clip = clip);
        coloredmeshes = list(lh_cmesh, rh_cmesh);
    } else {
        cmesh = coloredmesh.from.morph.native(subjects_dir, subject_id, measure, hemi, surface=surface, colormap=colormap, clip = clip);
        coloredmeshes = list(cmesh);
    }

    invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar));
}


#' @title Visualize a label for a subject.
#'
#' @description Visualize a label for a subject. A label is jsut a logical vector with one entry for each vertex in the mesh.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id, string. The subject identifier.
#'T
#' @param label, string. Name of the label file, without the hemi part (if any), but including the '.label' suffix. E.g., 'cortex.label' for '?h.cortex.label'.
#'
#' @param hemi, string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param surface, string. The display surface. E.g., "white", "pial", or "inflated". Defaults to "white".
#'
#' @param colormap, a colormap. See the squash package for some colormaps. Defaults to [squash::rainbow2].
#'
#' @param views, list of strings. Valid entries include: 'si': single interactive view. 't4': tiled view showing the brain from 4 angles. 't9': tiled view showing the brain from 9 angles.
#'
#' @param rgloptions option list passed to [rgl::par3d()]. Example: rgloptions = list("windowRect"=c(50,50,1000,1000));
#'
#' @param rglactions, named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    subject_id = 'subject1';
#'    surface = 'white';
#'    hemi = 'both';
#'    label = 'cortex.label';
#'    vis.subject.label(subjects_dir, subject_id, label, hemi);
#' }
#'
#' @family visualization functions
#' @family label functions
#'
#' @importFrom squash rainbow2
#' @export
vis.subject.label <- function(subjects_dir, subject_id, label, hemi, surface="white", colormap=squash::rainbow2, views=c("t4"), rgloptions = list(), rglactions = list(), draw_colorbar = FALSE) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(hemi == "both") {
        lh_cmesh = coloredmesh.from.label(subjects_dir, subject_id, label, 'lh', surface=surface, colormap=colormap);
        rh_cmesh = coloredmesh.from.label(subjects_dir, subject_id, label, 'rh', surface=surface, colormap=colormap);
        coloredmeshes = list(lh_cmesh, rh_cmesh);
    } else {
        cmesh = coloredmesh.from.label(subjects_dir, subject_id, label, hemi, surface=surface, colormap=colormap);
        coloredmeshes = list(cmesh);
    }

    invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar));
}



#' @title Visualize native space morphometry data for a subject.
#'
#' @description Creates a surface mesh, applies a colormap transform the morphometry data values into colors, and renders the resulting colored mesh in an interactive window. If hemi is 'both', the data is rendered for the whole brain.
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
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
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
vis.subject.morph.standard <- function(subjects_dir, subject_id, measure, hemi, fwhm, surface="white", template_subject = 'fsaverage', template_subjects_dir = NULL, colormap=squash::jet, views=c("t4"), rgloptions = list(), rglactions = list(), draw_colorbar = FALSE) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(is.null(template_subjects_dir)) {
        template_subjects_dir = subjects_dir;
    }

    if(rglactions.has.key(rglactions, 'clip_data')) {
        clip = rglactions$clip_data;
    } else {
        clip = NULL;
    }

    if(hemi == "both") {
        lh_cmesh = coloredmesh.from.morph.standard(subjects_dir, subject_id, measure, 'lh', fwhm, surface=surface, template_subject=template_subject, template_subjects_dir=template_subjects_dir, colormap=colormap, clip = clip);
        rh_cmesh = coloredmesh.from.morph.standard(subjects_dir, subject_id, measure, 'rh', fwhm, surface=surface, template_subject=template_subject, template_subjects_dir=template_subjects_dir, colormap=colormap, clip = clip);
        coloredmeshes = list(lh_cmesh, rh_cmesh);
    } else {
        cmesh = coloredmesh.from.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm, surface=surface, template_subject=template_subject, template_subjects_dir=template_subjects_dir, colormap=colormap, clip = clip);
        coloredmeshes = list(cmesh);
    }

    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar)));
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
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @keywords internal
brainviews <- function(views, coloredmeshes, rgloptions = list(), rglactions = list(), draw_colorbar = FALSE) {
    if(length(views)) {
        for(view in views) {
            if(view == "t4") {
                invisible(brainview.t4(coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar));
            } else if(view == "t9") {
                invisible(brainview.t9(coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar));
            } else if(view == "si") {
                invisible(brainview.si(coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar));
            } else if(view == "sr") {
                invisible(brainview.sr(coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar));
            } else {
                stop(sprintf("Invalid view '%s'. Valid ones include 'si', 'sr', t4' and 't9'.\n", view));
            }
        }
    }
    return(invisible(coloredmeshes));
}


#' @title Visualize arbitrary data on the surface of any subject.
#'
#' @description Creates a surface mesh, applies a colormap transform the morphometry data values into colors, and renders the resulting colored mesh in an interactive window. If hemi is 'both', the data is rendered for the whole brain.
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
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
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
vis.data.on.subject <- function(subjects_dir, vis_subject_id, morph_data_lh, morph_data_rh, surface="white", colormap=squash::jet, views=c('t4'), rgloptions=list(), rglactions = list(), draw_colorbar = FALSE) {

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

    invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar));
}


#' @title Visualize a vertex mask on the surface of a subject.
#'
#' @description A mask is a logical vector that contains one value per vertex. You can create it manually, or use functions like [fsbrain::mask.from.labeldata.for.hemi] to create and modify it. Check the example for this function.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, containing the subdir of vis_subject_id, the subject that you want to use for visualization.
#'
#' @param vis_subject_id, string. The subject identifier from which to obtain the surface for data visualization. Example: 'fsaverage'.
#'
#' @param mask_lh, logical vector or NULL, the mask to visualize on the left hemisphere surface. Must have the same length as the lh surface of the vis_subject_id has vertices. If NULL, this surface will not be rendered. Only one of mask_lh or mask_rh is allowed to be NULL.
#'
#' @param mask_rh, logical vector or NULL, the mask to visualize on the right hemisphere surface. Must have the same length as the rh surface of the vis_subject_id has vertices. If NULL, this surface will not be rendered. Only one of mask_lh or mask_rh is allowed to be NULL.
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
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'
#'   # Define the data to use:
#'   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'   subject_id = 'subject1';
#'   surface = 'white';
#'   hemi = 'both';
#'   atlas = 'aparc';
#'   region = 'bankssts';
#'
#'   # Create a mask from a region of an annotation:
#'   lh_annot = subject.annot(subjects_dir, subject_id, 'lh', atlas);
#'   rh_annot = subject.annot(subjects_dir, subject_id, 'rh', atlas);
#'   lh_label = label.from.annotdata(lh_annot, region);
#'   rh_label = label.from.annotdata(rh_annot, region);
#'   lh_mask = mask.from.labeldata.for.hemi(lh_label, length(lh_annot$vertices));
#'   rh_mask = mask.from.labeldata.for.hemi(rh_label, length(rh_annot$vertices));
#'
#'   # Edit the mask: add the vertices from another region to it:
#'   region2 = 'medialorbitofrontal';
#'   lh_label2 = label.from.annotdata(lh_annot, region2);
#'   rh_label2 = label.from.annotdata(rh_annot, region2);
#'   lh_mask2 = mask.from.labeldata.for.hemi(lh_label2, length(lh_annot$vertices),
#'    existing_mask = lh_mask);
#'   rh_mask2 = mask.from.labeldata.for.hemi(rh_label2, length(rh_annot$vertices),
#'    existing_mask = rh_mask);
#'   # Visualize the mask:
#'   vis.mask.on.subject(subjects_dir, subject_id, lh_mask2, rh_mask2);
#' }
#'
#' @family mask functions
#' @importFrom squash rainbow2
#' @export
vis.mask.on.subject <- function(subjects_dir, vis_subject_id, mask_lh, mask_rh, surface="white", colormap=squash::rainbow2, views=c('t4'), rgloptions=list(), rglactions = list(), draw_colorbar = FALSE) {

    if(is.null(mask_lh) && is.null(mask_rh)) {
        stop(sprintf("Only one of mask_lh or mask_rh can be NULL.\n"));
    }

    coloredmeshes = list();

    if(! is.null(mask_lh)) {
        coloredmeshes$lh = coloredmesh.from.mask(subjects_dir, vis_subject_id, mask_lh, 'lh', surface=surface, colormap=colormap);
    }

    if(! is.null(mask_rh)) {
        coloredmeshes$rh = coloredmesh.from.mask(subjects_dir, vis_subject_id, mask_rh, 'rh', surface=surface, colormap=colormap);
    }

    invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar));
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
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @family visualization functions
#'
#' @importFrom squash jet
#' @export
vis.data.on.fsaverage <- function(subjects_dir=NULL, vis_subject_id="fsaverage", morph_data_lh, morph_data_rh, surface="white", colormap=squash::jet, views=c('t4'), rgloptions = list(), rglactions = list(), draw_colorbar = FALSE) {

    if(is.null(subjects_dir)) {
        subjects_dir = find.subjectsdir.of(subject_id=vis_subject_id, mustWork = TRUE);
    }

    invisible(vis.data.on.subject(subjects_dir, vis_subject_id, morph_data_lh, morph_data_rh, surface=surface, colormap=colormap, views=views, rgloptions=rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar));
}


#' @title Visualize an annotation for a subject.
#'
#' @description Creates a surface mesh, loads the colors from the annotation, and renders the resulting colored mesh in an interactive window. If hemi is 'both', the data is rendered for the whole brain.
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
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE.
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
vis.region.values.on.subject <- function(subjects_dir, subject_id, atlas, lh_region_value_list, rh_region_value_list, surface="white", colormap=grDevices::heat.colors, views=c('t4'), rgloptions=list(), rglactions = list(), value_for_unlisted_regions = NaN, draw_colorbar = FALSE) {
    morph_like_data = spread.values.over.subject(subjects_dir, subject_id, atlas, lh_region_value_list, rh_region_value_list, value_for_unlisted_regions = value_for_unlisted_regions);
    invisible(vis.data.on.subject(subjects_dir, subject_id, morph_like_data$lh, morph_like_data$rh, surface=surface, colormap=colormap, views=views, rgloptions=rgloptions, rglactions=rglactions, draw_colorbar = draw_colorbar));
}



