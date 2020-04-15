# Functions for the interactive visualization of morphometry and atlas data (on subject level).


#' @title Visualize native space morphometry data for a subject.
#'
#' @description Creates a surface mesh, applies a colormap transform the morphometry data values into colors, and renders the resulting colored mesh in an interactive window. If hemi is 'both', the data is rendered for the whole brain.
#'
#' @param subjects_dir string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id string. The subject identifier.
#'
#' @param measure string. The morphometry data to use. E.g., 'area' or 'thickness'. Pass NULL to render just the surface in white, without any data.
#'
#' @param hemi string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to construct the names of the label data files to be loaded.
#'
#' @param surface string. The display surface. E.g., "white", "pial", or "inflated". Defaults to "white".
#'
#' @param colormap a colormap function. **DEPRECATED**: use parameter 'makecmap_options' instead.
#'
#' @param views list of strings. Valid entries include: 'si': single interactive view. 't4': tiled view showing the brain from 4 angles. 't9': tiled view showing the brain from 9 angles.
#'
#' @param rgloptions option list passed to \code{\link[rgl]{par3d}}. Example: \code{rgloptions = list("windowRect"=c(50,50,1000,1000))}.
#'
#' @param rglactions named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action.
#'
#' @param draw_colorbar logical, whether to draw a colorbar. WARNING: The colorbar is drawn to a subplot, and this only works if there is enough space for it. You will have to increase the plot size using the 'rlgoptions' parameter for the colorbar to show up. Defaults to FALSE. See  \code{\link[fsbrain]{coloredmesh.plot.colorbar.separate}} for an alternative.
#'
#' @param cortex_only logical, whether to mask the medial wall, i.e., whether the morphometry data for all vertices which are *not* part of the cortex (as defined by the label file `label/?h.cortex.label`) should be replaced with NA values. In other words, setting this to TRUE will ignore the values of the medial wall between the two hemispheres. If set to true, the mentioned label file needs to exist for the subject. Defaults to FALSE.
#'
#' @param style character string, a rendering style, e.g., 'default', 'shiny' or 'semitransparent'.
#'
#' @param makecmap_options named list of parameters to pass to \code{\link[squash]{makecmap}}. Must not include the unnamed first parameter, which is derived from 'measure'. Should include at least a colormap function as name 'colFn'.
#'
#' @param bg a background definition. Can be a surface color layer or a character string, see \code{\link[fsbrain]{collayer.bg}} for valid strings.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    vis.subject.morph.native(subjects_dir, 'subject1', 'thickness', 'lh', views=c("t9"));
#' }
#'
#' @family visualization functions
#' @family morphometry visualization functions
#'
#' @importFrom squash jet
#' @export
vis.subject.morph.native <- function(subjects_dir, subject_id, measure, hemi="both", surface="white", colormap=NULL, views=c("t4"), rgloptions = list(), rglactions = list(), draw_colorbar = FALSE, cortex_only=FALSE, style = 'default', makecmap_options=list('colFn'=squash::jet), bg=NULL) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    makecmap_options = makecmakeopts.merge(makecmap_options, colormap);

    if(is.hemilist(measure)) {
        measure_data = measure;
        hemi = hemilist.derive.hemi(measure_data);  # need to rewrite the hemi, depending on the passed data
    } else {
        measure_data = subject.morph.native(subjects_dir, subject_id, measure, hemi, cortex_only=cortex_only, split_by_hemi=TRUE);
    }

    if(rglactions.has.key(rglactions, 'clip_data')) {
        clip_range = rglactions$clip_data;
        measure_data = clip.data(measure_data, lower=clip_range[1], upper=clip_range[2]);
    }

    both_hemi_colors = collayer.from.morphlike.data(measure_data$lh, measure_data$rh, makecmap_options=makecmap_options, return_map=TRUE);
    map = both_hemi_colors$map;
    both_hemi_colors$map = NULL;
    if(!is.null(bg)) {
        background = collayer.bg(subjects_dir, subject_id, bg, hemi=hemi);
        both_hemi_colors = collayers.merge(list("fg"=both_hemi_colors, "bg"=background));
    }

    coloredmeshes = coloredmeshes.from.color(subjects_dir, subject_id, both_hemi_colors, hemi, surface=surface, metadata=list('src_data'=measure_data, 'map'=map, 'makecmap_options'=makecmap_options));

    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar, style = style)));
}




#' @title Visualize native space morphometry data for a subject.
#'
#' @description Creates a surface mesh, applies a colormap transform the morphometry data values into colors, and renders the resulting colored mesh in an interactive window. If hemi is 'both', the data is rendered for the whole brain.
#'
#' @inheritParams vis.subject.morph.native
#'
#' @param fwhm string, smoothing setting. The smoothing part of the filename, typically something like '0', '5', '10', ...,  or '25'.
#'
#' @param template_subject The template subject used. This will be used as part of the filename, and its surfaces are loaded for data visualization. Defaults to 'fsaverage'.
#'
#' @param template_subjects_dir The template subjects dir. If NULL, the value of the parameter 'subjects_dir' is used. If you have FreeSurfer installed and configured, and are using the standard fsaverage subject, try passing the result of calling 'file.path(Sys.getenv('FREESURFER_HOME'), 'subjects')'.
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
#' @family morphometry visualization functions
#'
#' @importFrom squash jet
#' @export
vis.subject.morph.standard <- function(subjects_dir, subject_id, measure, hemi="both", fwhm="10", surface="white", template_subject = 'fsaverage', template_subjects_dir = NULL, colormap=NULL, views=c("t4"), rgloptions = list(), rglactions = list(), draw_colorbar = FALSE, cortex_only = FALSE, makecmap_options=list('colFn'=squash::jet), bg=NULL) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    makecmap_options = makecmakeopts.merge(makecmap_options, colormap);

    if(is.null(template_subjects_dir)) {
        template_subjects_dir = subjects_dir;
    }

    if(is.hemilist(measure)) {
        measure_data = measure;
        hemi = hemilist.derive.hemi(measure_data);  # need to rewrite the hemi, depending on the passed data
    } else {
        measure_data = subject.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm=fwhm, template_subject=template_subject, cortex_only=cortex_only, split_by_hemi=TRUE);
    }

    if(rglactions.has.key(rglactions, 'clip_data')) {
        clip_range = rglactions$clip_data;
        measure_data = clip.data(measure_data, lower=clip_range[1], upper=clip_range[2]);
    }

    both_hemi_colors = collayer.from.morphlike.data(measure_data$lh, measure_data$rh, makecmap_options=makecmap_options, return_map = TRUE);
    map = both_hemi_colors$map;
    both_hemi_colors$map = NULL;
    if(!is.null(bg)) {
        background = collayer.bg(subjects_dir, subject_id, bg, hemi=hemi);
        both_hemi_colors = collayers.merge(list("fg"=both_hemi_colors, "bg"=background));
    }

    coloredmeshes = coloredmeshes.from.color(template_subjects_dir, template_subject, both_hemi_colors, hemi, surface=surface, metadata=list("src_data"=measure_data, "map"=map, "makecmap_options"=makecmap_options));

    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar)));
}


#' @title Visualize a binary label for a subject.
#'
#' @description Visualize a label for a subject. A label is just a logical vector with one entry for each vertex in the mesh. Each vertex may additionally be associated with a scalar value, but this function ignored that.
#'
#' @inheritParams vis.subject.morph.native
#'
#' @param label string. Name of the label file, without the hemi part (if any), but including the '.label' suffix. E.g., 'cortex.label' for '?h.cortex.label'.
#'
#' @param map_to_NA the value or value range that should **not** be considered part of the label, and should thus be plotted as background color. Only used if 'bg' is not `NULL`. If a single value, only excatly this value is used (typically 0). If two values, they are interpreted as a range, and a values between them are mapped to NA. If you prefer to map the data to NA yourself before using this function, pass `NULL`.
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
#'    vis.subject.label(subjects_dir, subject_id, label, hemi, views="si");
#' }
#'
#' @family visualization functions
#' @family label functions
#'
#' @importFrom squash rainbow2
#' @export
vis.subject.label <- function(subjects_dir, subject_id, label, hemi, surface="white", colormap=NULL, views=c("t4"), rgloptions = list(), rglactions = list(), draw_colorbar = FALSE, makecmap_options=list('colFn'=squash::rainbow2, 'col.na'='#FFFFFF00'), map_to_NA=0L, bg=NULL) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    makecmap_options = makecmakeopts.merge(makecmap_options, colormap);

    mask_data = list();
    if(hemi %in% c("lh", "both")) {
        lh_label_data = subject.label(subjects_dir, subject_id, label, 'lh');
        lh_surface_data = subject.surface(subjects_dir, subject_id, surface, 'lh');
        lh_mask = mask.from.labeldata.for.hemi(list(lh_label_data), nrow(lh_surface_data$vertices));
        if(!is.null(bg)) {
            lh_mask = perform.na.mapping(lh_mask, map_to_NA);
        }
        mask_data$lh = lh_mask;
    }
    if(hemi %in% c("rh", "both")) {
        rh_label_data = subject.label(subjects_dir, subject_id, label, 'rh');
        rh_surface_data = subject.surface(subjects_dir, subject_id, surface, 'rh');
        rh_mask = mask.from.labeldata.for.hemi(list(rh_label_data), nrow(rh_surface_data$vertices));
        if(!is.null(bg)) {
            rh_mask = perform.na.mapping(rh_mask, map_to_NA);
        }
        mask_data$rh = rh_mask;
    }


    both_hemi_colors = collayer.from.mask.data(mask_data$lh, mask_data$rh, makecmap_options=makecmap_options);
    if(!is.null(bg)) {
        background = collayer.bg(subjects_dir, subject_id, bg, hemi=hemi);
        both_hemi_colors = collayers.merge(list("fg"=both_hemi_colors, "bg"=background));
    }

    coloredmeshes = coloredmeshes.from.color(subjects_dir, subject_id, both_hemi_colors, hemi, surface=surface);
    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar)));
}




#' @title Visualize arbitrary data on the surface of any subject.
#'
#' @description Creates a surface mesh, applies a colormap transform the morphometry data values into colors, and renders the resulting colored mesh in an interactive window. If hemi is 'both', the data is rendered for the whole brain.
#'
#' @inheritParams vis.subject.morph.native
#'
#' @param vis_subject_id string. The subject identifier from which to obtain the surface for data visualization. Example: 'fsaverage'.
#'
#' @param morph_data_lh numeric vector or character string or NULL, the data to visualize on the left hemisphere surface. If a string, it is treated as a filename and data is loaded from it first. When it is a numerical vector, this is assumed to be the data already. The data must have the same length as the surface of the vis_subject_id has vertices. If NULL, this surface will not be rendered. Only one of morph_data_lh or morph_data_rh is allowed to be NULL.
#'
#' @param morph_data_rh numeric vector or character string or NULL, the data to visualize on the right hemisphere surface. If a string, it is treated as a filename and data is loaded from it first. When it is a numerical vector, this is assumed to be the data already.  The data must have the same length as the surface of the vis_subject_id has vertices. If NULL, this surface will not be rendered. Only one of morph_data_lh or morph_data_rh is allowed to be NULL.
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
#' @family morphometry visualization functions
#'
#' @importFrom squash jet
#' @export
vis.data.on.subject <- function(subjects_dir, vis_subject_id, morph_data_lh, morph_data_rh, surface="white", colormap=NULL, views=c('t4'), rgloptions=list(), rglactions = list(), draw_colorbar = FALSE, makecmap_options=list('colFn'=squash::jet), bg=NULL) {

    if(is.null(morph_data_lh) && is.null(morph_data_rh)) {
        stop(sprintf("Only one of morph_data_lh or morph_data_rh can be NULL.\n"));
    }

    makecmap_options = makecmakeopts.merge(makecmap_options, colormap);

    measure = hemilist.wrap(morph_data_lh, 'lh');
    measure = hemilist.wrap(morph_data_rh, 'rh', measure);

    return(vis.subject.morph.native(subjects_dir, vis_subject_id, measure, surface=surface, views=views, rgloptions=rgloptions, rglactions=rglactions, draw_colorbar=draw_colorbar, makecmap_options=makecmap_options, bg=bg));
}


#' @title Visualize clusters or activation data on the surface of any subject.
#'
#' @description This function is intended to plot symmetric data around zero (like positive and negative activation data, signed p-values, etc.) on a subject's surface. It is a thin wrapper around \code{\link[fsbrain]{vis.data.on.subject}}.
#'
#' @inheritParams vis.data.on.subject
#'
#' @param map_to_NA the value or value range that should **not** be considered a cluster, and should thus be plotted as background color. If a single value, only exactly this value is used (typically 0). If two values, they are interpreted as a range, and a values between them are mapped to NA. If you prefer to map the data to NA yourself before using this function, pass `NULL`.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    morph_data_lh = subject.morph.native(subjects_dir, 'subject1', 'thickness', 'lh');
#'    morph_data_rh = NULL;
#'    vis.symmetric.data.on.subject(subjects_dir, 'subject1', morph_data_lh, morph_data_rh);
#' }
#'
#' @family visualization functions
#' @family morphometry visualization functions
#'
#' @importFrom squash jet
#' @export
vis.symmetric.data.on.subject <- function(subjects_dir, vis_subject_id, morph_data_lh, morph_data_rh, surface="white", colormap=NULL, views=c('t4'), rgloptions=list(), rglactions = list(), draw_colorbar = FALSE, makecmap_options=list('colFn'=squash::jet, symm=TRUE, col.na='#FFFFFF00'), map_to_NA=c(0), bg=NULL) {
    makecmap_options = makecmakeopts.merge(makecmap_options, colormap);

    morph_data_lh = perform.na.mapping(morph_data_lh, map_to_NA);
    morph_data_rh = perform.na.mapping(morph_data_rh, map_to_NA);

    return(vis.data.on.subject(subjects_dir, vis_subject_id, morph_data_lh, morph_data_rh, surface=surface, views=views, rgloptions=rgloptions, rglactions=rglactions, draw_colorbar=draw_colorbar, makecmap_options=makecmap_options, bg=bg));
}


#' Perform NA mapping for transparency
#'
#' @description Usually this is done so that the NA values are plotted transparently, so your can see a background color through the respective colors.
#'
#' @param data numeric vector
#'
#' @param map_to_NA the value or value range that should **not** be mapped to `NA`. If a single value, only exactly this value is used. If two values, they are interpreted as a range, and a values between them are mapped to NA. If you pass `NULL`, the data are returned as-is.
#'
#' @return the mapped data
#'
#' @keywords internal
perform.na.mapping <- function(data, map_to_NA) {

    if(is.null(data)) {
        return(NULL);
    }

    if(is.null(map_to_NA)) {
        return(data);
    }


    if(length(map_to_NA) == 1L) {
        data[data == map_to_NA] = NA;
    } else if(length(map_to_NA) == 2L) { # treat as a range
        data[which(data > map_to_NA[1] & data < map_to_NA[2])] = NA;
    }
    return(data);
}


#' @title Visualize pre-defined vertex colors on a subject.
#'
#' @inheritParams vis.data.on.subject
#'
#' @param color_lh vector of colors to visualize on the left hemisphere surface.
#'
#' @param color_rh vector of colors to visualize on the right hemisphere surface.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'    color_lh = '#ff0000';
#'    num_verts_subject1_rh = 153333;
#'    color_rh = rep('#333333', num_verts_subject1_rh);
#'    color_rh[1:30000] = '#00ff00';
#'    color_rh[30001:60000] = '#ff0000';
#'    color_rh[60001:90000] = '#0000ff';
#'    color_rh[90001:120000] = '#ffff00';
#'    color_rh[120001:150000] = '#00ffff';
#'    vis.color.on.subject(subjects_dir, 'subject1', color_lh, color_rh);
#' }
#'
#' @family visualization functions
#' @family surface visualization functions
#'
#' @export
vis.color.on.subject <- function(subjects_dir, vis_subject_id, color_lh, color_rh, surface="white", views=c('t4'), rgloptions=list(), rglactions = list()) {

    if(is.null(color_lh) && is.null(color_rh)) {
        stop(sprintf("Only one of color_lh or color_rh can be NULL.\n"));
    }

    coloredmeshes = list();

    if(! is.null(color_lh)) {
        cmesh_lh = coloredmesh.from.color(subjects_dir, vis_subject_id, color_lh, 'lh', surface=surface);
        coloredmeshes$lh = cmesh_lh;
    }

    if(! is.null(color_rh)) {
        cmesh_rh = coloredmesh.from.color(subjects_dir, vis_subject_id, color_rh, 'rh', surface=surface);
        coloredmeshes$rh = cmesh_rh;
    }

    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions)));
}


#' @title Visualize a vertex mask on the surface of a subject.
#'
#' @description A mask is a logical vector that contains one value per vertex. You can create it manually, or use functions like [fsbrain::mask.from.labeldata.for.hemi] to create and modify it. Check the example for this function.
#'
#' @inheritParams vis.data.on.subject
#'
#' @param mask_lh logical vector or NULL, the mask to visualize on the left hemisphere surface. Must have the same length as the lh surface of the vis_subject_id has vertices. If NULL, this surface will not be rendered. Only one of mask_lh or mask_rh is allowed to be NULL.
#'
#' @param mask_rh logical vector or NULL, the mask to visualize on the right hemisphere surface. Must have the same length as the rh surface of the vis_subject_id has vertices. If NULL, this surface will not be rendered. Only one of mask_lh or mask_rh is allowed to be NULL.
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
#' @family visualization functions
#'
#' @importFrom squash rainbow2
#' @export
vis.mask.on.subject <- function(subjects_dir, vis_subject_id, mask_lh, mask_rh, surface="white", colormap=NULL, views=c('t4'), rgloptions=list(), rglactions = list(), draw_colorbar = FALSE, makecmap_options=list('colFn'=squash::rainbow2)) {

    if(is.null(mask_lh) && is.null(mask_rh)) {
        stop(sprintf("Only one of mask_lh or mask_rh can be NULL.\n"));
    }

    makecmap_options = makecmakeopts.merge(makecmap_options, colormap);

    coloredmeshes = list();

    if(! is.null(mask_lh)) {
        coloredmeshes$lh = coloredmesh.from.mask(subjects_dir, vis_subject_id, mask_lh, 'lh', surface=surface, makecmap_options=makecmap_options);
    }

    if(! is.null(mask_rh)) {
        coloredmeshes$rh = coloredmesh.from.mask(subjects_dir, vis_subject_id, mask_rh, 'rh', surface=surface, makecmap_options=makecmap_options);
    }

    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar)));
}


#' @title Visualize a label on the surface of a subject.
#'
#' @description Visualizes a label. Note that a label is just a set of vertices, and that you can use this function to visualize sets of vertices, e.g., to see where on the mesh a certain vertex lies. It may be helpful the visualize the vertex with its neighbors, because otherwise it may be too small to spot. Use the function [fsbrain::mesh.vertex.neighbors] to get them. It is advisable to set the view to the interactive 'si' mode and use the 'inflated' surface to identify single vertices.
#'
#' @inheritParams vis.data.on.subject
#'
#' @param lh_labeldata  integer vector of vertex indices for the left hemisphere
#'
#' @param rh_labeldata integer vector of vertex indices for the right hemisphere
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @examples
#' \donttest{
#'    fsbrain::download_optional_data();
#'
#'   # Define the data to use:
#'   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'   lh_labeldata = c(1000, 1001, 1002);   # only the vertices, will be tiny.
#'   subject_id = 'subject1';
#'   surface = 'white'; # Should use 'inflated', but we do not currently
#'                      # ship it for the example subject to reduce download size.
#'
#'   # For the right hemi, extend them to neighborhood for better visibility:
#'   rh_labeldata = c(500, 5000);
#'   rh_surface = subject.surface(subjects_dir, subject_id, surface, 'rh');
#'   rh_labeldata_neighborhood = mesh.vertex.neighbors(rh_surface, rh_labeldata);
#'   vis.labeldata.on.subject(subjects_dir, subject_id, lh_labeldata,
#'    rh_labeldata_neighborhood$vertices, surface=surface, views=c('si'));
#' }
#'
#' @family label functions
#' @family visualization functions
#'
#' @importFrom squash rainbow2
#' @export
vis.labeldata.on.subject <- function(subjects_dir, vis_subject_id, lh_labeldata, rh_labeldata, surface="white", colormap=NULL, views=c('t4'), rgloptions=list(), rglactions = list(), draw_colorbar = FALSE, makecmap_options=list('colFn'=squash::rainbow2)) {

    if(is.null(lh_labeldata) && is.null(rh_labeldata)) {
        stop(sprintf("Only one of lh_labeldata or rh_labeldata can be NULL.\n"));
    }

    makecmap_options = makecmakeopts.merge(makecmap_options, colormap);

    coloredmeshes = list();

    if(! is.null(lh_labeldata)) {
        coloredmeshes$lh = coloredmesh.from.label(subjects_dir, vis_subject_id, lh_labeldata, 'lh', surface=surface, makecmap_options=makecmap_options);
    }

    if(! is.null(rh_labeldata)) {
        coloredmeshes$rh = coloredmesh.from.label(subjects_dir, vis_subject_id, rh_labeldata, 'rh', surface=surface, makecmap_options=makecmap_options);
    }

    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar)));
}


#' @title Visualize arbitrary data on the fsaverage template subject, if available.
#'
#' @description Creates a surface mesh, applies a colormap transform the morphometry data values into colors, and renders the resulting colored mesh in an interactive window. If hemi is 'both', the data is rendered for the whole brain. This function tries to automatically retrieve the subjects dir of the fsaverage template subject by checking the environment variables SUBJECTS_DIR and FREESURFER_HOME for the subject. The subject is required for its surfaces, which are not shipped with this package for licensing reasons.
#'
#' @inheritParams vis.data.on.subject
#'
#' @param vis_subject_id string. The subject identifier from which to obtain the surface for data visualization. Defaults to 'fsaverage'.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @family visualization functions
#' @family morphometry visualization functions
#'
#' @importFrom squash jet
#' @export
vis.data.on.fsaverage <- function(subjects_dir=NULL, vis_subject_id="fsaverage", morph_data_lh, morph_data_rh, surface="white", colormap=NULL, views=c('t4'), rgloptions = list(), rglactions = list(), draw_colorbar = FALSE, makecmap_options=list('colFn'=squash::jet), bg=NULL) {

    if(is.null(subjects_dir)) {
        subjects_dir = find.subjectsdir.of(subject_id=vis_subject_id, mustWork = TRUE);
    }

    makecmap_options = makecmakeopts.merge(makecmap_options, colormap);

    return(invisible(vis.data.on.subject(subjects_dir, vis_subject_id, morph_data_lh, morph_data_rh, surface=surface, views=views, rgloptions=rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar, makecmap_options=makecmap_options, bg=bg)));
}


#' @title Visualize an annotation for a subject.
#'
#' @description Creates a surface mesh, loads the colors from the annotation, and renders the resulting colored mesh in an interactive window. If hemi is 'both', the data is rendered for the whole brain.
#'
#' @inheritParams vis.subject.morph.native
#'
#' @param atlas string. The atlas name. E.g., "aparc", "aparc.2009s", or "aparc.DKTatlas". Used to construct the name of the annotation file to be loaded.
#'
#' @param outline logical, whether to draw an outline only instead of filling the regions. Defaults to FALSE.
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
#' @family region-based visualization functions
#'
#' @export
vis.subject.annot <- function(subjects_dir, subject_id, atlas, hemi='both', surface="white", views=c('t4'), rgloptions=list(), rglactions = list(), outline=FALSE) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    coloredmeshes = list();

    if(hemi %in% c("both", "lh")) {
        coloredmeshes$lh = coloredmesh.from.annot(subjects_dir, subject_id, atlas, 'lh', surface=surface, outline=outline);
    }
    if(hemi %in% c("both", "rh")) {
        coloredmeshes$rh = coloredmesh.from.annot(subjects_dir, subject_id, atlas, 'rh', surface=surface, outline=outline);
    }

    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions)));
}



#' @title Visualize arbitrary data, one value per atlas region, on the surface of any subject (including template subjects).
#'
#' @description This function can be used for rendering a single value (color) for all vertices of an atlas region. The typical usecase is the visualization of results of atlas-based analyses, e.g., p-value, means or other aggregated values over all vertices of a region.
#'
#' @inheritParams vis.subject.morph.native
#'
#' @param atlas string. The brain atlas to use. E.g., 'aparc' or 'aparc.a2009s'.
#'
#' @param lh_region_value_list named list. A list for the left hemisphere in which the names are atlas regions, and the values are the value to write to all vertices of that region.
#'
#' @param rh_region_value_list named list. A list for the right hemisphere in which the names are atlas regions, and the values are the value to write to all vertices of that region.
#'
#' @param value_for_unlisted_regions numerical scalar or `NaN`, the value to assign to regions which do not occur in the region_value_lists. Defaults to `NaN`.
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
#' @family region-based visualization functions
#'
#' @importFrom grDevices heat.colors
#' @export
vis.region.values.on.subject <- function(subjects_dir, subject_id, atlas, lh_region_value_list, rh_region_value_list, surface="white", colormap=NULL, views=c('t4'), rgloptions=list(), rglactions = list(), value_for_unlisted_regions = NA, draw_colorbar = FALSE, makecmap_options=list('colFn'=grDevices::heat.colors), bg=NULL) {
    makecmap_options = makecmakeopts.merge(makecmap_options, colormap);
    morph_like_data = spread.values.over.subject(subjects_dir, subject_id, atlas, lh_region_value_list, rh_region_value_list, value_for_unlisted_regions = value_for_unlisted_regions);
    return(invisible(vis.data.on.subject(subjects_dir, subject_id, morph_like_data$lh, morph_like_data$rh, surface=surface, views=views, rgloptions=rgloptions, rglactions=rglactions, draw_colorbar = draw_colorbar, makecmap_options=makecmap_options, bg=bg)));
}



