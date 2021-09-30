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
#' @param views list of strings. Valid entries include: 'si': single interactive view. 't4': tiled view showing the brain from 4 angles. 't9': tiled view showing the brain from 9 angles.
#'
#' @param rgloptions option list passed to \code{\link{par3d}}. Example: \code{rgloptions = list("windowRect"=c(50,50,1000,1000))}.
#'
#' @param rglactions named list. A list in which the names are from a set of pre-defined actions. The values can be used to specify parameters for the action. The following example clips outliers in the data before plotting and writes a screenshot in PNG format: \code{rglactions = list("snapshot_png"="~/fsbrain.png", "clip_data"=c(0.05, 0.95))}. See \code{\link{rglactions}}.
#'
#' @param draw_colorbar logical or one of the character strings 'vertical' or 'horizontal', whether to draw a colorbar. Notice: the colorbar is drawn to a separate subplot, and this only works if there is enough space for it, i.e., the plot resolution must be high enough. You may have to increase the plot size for the colorbar to show up, see the vignette for instructions. Defaults to `FALSE`. See  \code{\link[fsbrain]{coloredmesh.plot.colorbar.separate}} for an alternative.
#'
#' @param cortex_only logical, whether to mask the medial wall, i.e., whether the morphometry data for all vertices which are *not* part of the cortex (as defined by the label file `label/?h.cortex.label`) should be replaced with NA values. In other words, setting this to TRUE will ignore the values of the medial wall between the two hemispheres. If set to true, the mentioned label file needs to exist for the subject. Defaults to FALSE.
#'
#' @param style character string, a rendering style, e.g., 'default', 'shiny' or 'semitransparent'.
#'
#' @param makecmap_options named list of parameters to pass to \code{\link{makecmap}}. Must not include the unnamed first parameter, which is derived from 'measure'. Should include at least a colormap function as name 'colFn'.
#'
#' @param bg a background definition. Can be a surface color layer or a character string like 'curv_light' to select a pre-defined layer, see \code{\link[fsbrain]{collayer.bg}} for valid strings.
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
#' @export
vis.subject.morph.native <- function(subjects_dir, subject_id, measure, hemi="both", surface="white", views=c("t4"), rgloptions = rglo(), rglactions = list(), draw_colorbar = FALSE, cortex_only=FALSE, style = 'default', makecmap_options=mkco.seq(), bg=NULL) {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(is.hemilist(measure)) {
        measure_data = measure;
        hemi = hemilist.derive.hemi(measure_data);  # need to rewrite the hemi, depending on the passed data
    } else {
        measure_data = subject.morph.native(subjects_dir, subject_id, measure, hemi, cortex_only=cortex_only, split_by_hemi=TRUE);
    }

    measure_data = rglactions.transform(measure_data, rglactions); # apply transform or data clipping

    both_hemi_colors = collayer.from.morphlike.data(measure_data$lh, measure_data$rh, makecmap_options=makecmap_options, return_metadata=TRUE);
    metadata = both_hemi_colors$metadata;
    both_hemi_colors$metadata = NULL;
    if(!is.null(bg)) {
        background = collayer.bg(subjects_dir, subject_id, bg, hemi=hemi);
        both_hemi_colors = collayers.merge(collayers = list("fg"=both_hemi_colors, "bg"=background), opaque_background = FALSE);
    }

    if(! is.hemilist(both_hemi_colors)) {
        stop("both_hemi_colors must be a hemilist")
    }

    coloredmeshes = coloredmeshes.from.color(subjects_dir, subject_id, both_hemi_colors, hemi, surface=surface, metadata=list('src_data'=measure_data, 'map'=metadata$map, 'makecmap_options'=makecmap_options));

    if(hasIn(rglactions, c('replace_final_colors_by_vertex'))) {
        if(hemi %in% c('both', 'lh')) {
            if(hasIn(rglactions, c('replace_final_colors_by_vertex', 'lh'))) {
                for(color in names(rglactions$replace_final_colors_by_vertex$lh)) {
                    vert_indices = rglactions$replace_final_colors_by_vertex$lh[[color]];
                    coloredmeshes$lh$col[vert_indices] = color;
                }
            }
        }
        if(hemi %in% c('both', 'rh')) {
            if(hasIn(rglactions, c('replace_final_colors_by_vertex', 'rh'))) {
                for(color in names(rglactions$replace_final_colors_by_vertex$rh)) {
                    vert_indices = rglactions$replace_final_colors_by_vertex$rh[[color]];
                    coloredmeshes$rh$col[vert_indices] = color;
                }
            }
        }

    }

    if(hasIn(rglactions, c('no_vis'))) {
        return(coloredmeshes);
    }

    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar, style = style)));
}



#' @ title Ensure an key for a hemilist exists.
#'
#' @param hemilist a hemilist
#'
#' @param required_hemi string, one of 'lh', 'rh' or 'both'.
#'
#' @keywords internal
hemlist.ensure.contains <- function(hemilist, required_hemi, error_tag="") {
    if(!(required_hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'required_hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", required_hemi));
    }
    if(required_hemi == "both") {
        if(!("lh" %in% names(hemilist))) {
            stop(sprintf("Hemilist %s does not contain required valid (non-NA/NULL) entry for key 'lh', and both 'lh' and 'rh' are required.\n", error_tag));
        }
        if(!("rh" %in% names(hemilist))) {
            stop(sprintf("Hemilist %s does not contain required valid (non-NA/NULL) entry for key 'rh', and both 'lh' and 'rh' are required.\n", error_tag));
        }
    } else {
        if(!(required_hemi %in% names(hemilist))) {
            stop(sprintf("Hemilist %s does not contain required valid entry for key '%s'.\n", error_tag, required_hemi));
        }

    }
    return(invisible(NULL));
}


#' @title Visualize pre-loaded data.
#'
#' @inheritParams vis.subject.morph.native
#'
#' @param surfaces a \code{\link[fsbrain]{hemilist}} of surfaces loaded with a function like \code{freesurferformats::read.fs.surface}.
#'
#' @param pervertex_data a \code{\link[fsbrain]{hemilist}} of per-vertex data for the surfaces, i.e., a list of numeric vectors. E.g., loaded  from a moorphometry data file with a function like \code{freesurferformats::read.fs.morph}.
#'´
#' @family visualization functions
#'
#' @export
vis.subject.pre <- function(surfaces, pervertex_data, hemi="both", views=c("t4"), rgloptions = rglo(), rglactions = list(), draw_colorbar = FALSE, style = 'default', makecmap_options=mkco.seq()) {
    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    if(! is.hemilist(pervertex_data)) {
        if(hemi %in% c("lh", "rh")) { # let me fix this for you.
            pervertex_data = hemilist.wrap(pervertex_data, hemi=hemi);
        } else {
            stop("Parameter 'pervertex_data' must be a hemilist if 'hemi' is set to 'both'.");
        }
    }
    pervertex_data = rglactions.transform(pervertex_data, rglactions); # apply transform or data clipping

    if(! is.hemilist(surfaces)) {
        if(hemi %in% c("lh", "rh")) { # let me fix this for you.
            surfaces = hemilist.wrap(surfaces, hemi=hemi);
        } else {
            stop("Parameter 'surfaces' must be a hemilist if 'hemi' is set to 'both'.");
        }
    }

    hemlist.ensure.contains(surfaces, hemi, "surfaces");
    hemlist.ensure.contains(pervertex_data, hemi, "pervertex_data");

    both_hemi_colors = collayer.from.morphlike.data(pervertex_data$lh, pervertex_data$rh, makecmap_options=makecmap_options, return_metadata=TRUE);
    metadata = both_hemi_colors$metadata;
    both_hemi_colors$metadata = NULL;

    if(! is.hemilist(both_hemi_colors)) {
        stop("both_hemi_colors must be a hemilist")
    }

    coloredmeshes = coloredmeshes.from.color(subjects_dir=NULL, subject_id=NULL, both_hemi_colors, hemi, surface=surfaces, metadata=list('src_data'=pervertex_data, 'map'=metadata$map, 'makecmap_options'=makecmap_options));

    if(hasIn(rglactions, c('no_vis'))) {
        return(coloredmeshes);
    }

    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar, style = style)));
}





#' @title Visualize native space morphometry data for a subject or a group.
#'
#' @description Renders standard space morphometry data for a single subject, or the group mean for a group of subjects. The default template subject is fsaverage.
#'
#' @inheritParams vis.subject.morph.native
#'
#' @param subject_id character string or vector of character strings, the subject or subjects. For a single subjects, its data will be plotted. If a group of subjects is given instead, at each vertex the mean value over all the subjects will be plotted.
#'
#' @param fwhm string, smoothing setting (full width at half maximum of the kernel). The smoothing part of the filename, typically something like '0', '5', '10', ...,  or '25'.
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
#' @export
vis.subject.morph.standard <- function(subjects_dir, subject_id, measure, hemi="both", fwhm="10", surface="white", template_subject = 'fsaverage', template_subjects_dir = NULL, views=c("t4"), rgloptions = rglo(), rglactions = list(), draw_colorbar = FALSE, cortex_only = FALSE, makecmap_options=mkco.seq(), bg=NULL, style = 'default') {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

    check.subjectslist(subject_id, subjects_dir=subjects_dir);

    if(is.null(template_subjects_dir)) {
        template_subjects_dir = subjects_dir;
    }

    if(is.hemilist(measure)) {
        measure_data = measure;
        hemi = hemilist.derive.hemi(measure_data);  # need to rewrite the hemi, depending on the passed data
    } else {
        if(length(subject_id) > 1L) {
            subjects_list = subject_id;
            measure_data = group.morph.agg.standard.vertex(subjects_dir, subjects_list, measure, hemi, fwhm=fwhm, template_subject=template_subject, cortex_only=cortex_only, split_by_hemi=TRUE)
        } else {
            measure_data = subject.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm=fwhm, template_subject=template_subject, cortex_only=cortex_only, split_by_hemi=TRUE);
        }
    }

    measure_data = rglactions.transform(measure_data, rglactions);

    both_hemi_colors = collayer.from.morphlike.data(measure_data$lh, measure_data$rh, makecmap_options=makecmap_options, return_metadata = TRUE);
    metadata = both_hemi_colors$metadata;
    both_hemi_colors$metadata = NULL;
    if(!is.null(bg)) {
        background = collayer.bg(subjects_dir, subject_id, bg, hemi=hemi);
        both_hemi_colors = collayers.merge(list("fg"=both_hemi_colors, "bg"=background));
    }

    coloredmeshes = coloredmeshes.from.color(template_subjects_dir, template_subject, both_hemi_colors, hemi, surface=surface, metadata=list("src_data"=measure_data, "map"=metadata$map, "makecmap_options"=makecmap_options));

    if(hasIn(rglactions, c('no_vis'))) {
        return(coloredmeshes);
    }

    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar, style = style)));
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
#' @note Drawing a colorbar for label data makes limited sense, use a legend instead. The colorbar can give a rough overview of the relative number of label and non-label vertices though, so it is possible to request one.
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
vis.subject.label <- function(subjects_dir, subject_id, label, hemi, surface="white", views=c("t4"), rgloptions = rglo(), rglactions = list(), draw_colorbar = FALSE, makecmap_options=list('colFn'=label.colFn.inv, 'col.na'='#FFFFFF00'), map_to_NA=0L, bg=NULL, style = "default") {

    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh' or 'both' but is '%s'.\n", hemi));
    }

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

    mask_data = lapply(mask_data, as.integer);

    both_hemi_colors = collayer.from.morphlike.data(mask_data$lh, mask_data$rh, makecmap_options=makecmap_options, return_metadata = TRUE);
    metadata = both_hemi_colors$metadata;
    both_hemi_colors$metadata = NULL;
    if(!is.null(bg)) {
        background = collayer.bg(subjects_dir, subject_id, bg, hemi=hemi);
        both_hemi_colors = collayers.merge(list("fg"=both_hemi_colors, "bg"=background));
    }

    coloredmeshes = coloredmeshes.from.color(subjects_dir, subject_id, both_hemi_colors, hemi, surface=surface, metadata=list("src_data"=mask_data, "map"=metadata$map, "makecmap_options"=makecmap_options));

    if(hasIn(rglactions, c('no_vis'))) {
        return(coloredmeshes);
    }

    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar, style = style)));
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
#' @param morph_data_both numeric vector or NULL, the data to visualize on both hemispheres. This must be a single vector with length equal to the sum of the vertex counts of the left and the right hemisphere. The data for the left hemisphere must come first. If this is given, 'morph_data_lh' and 'morph_data_rh' must be NULL.
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
#' @export
vis.data.on.subject <- function(subjects_dir, vis_subject_id, morph_data_lh=NULL, morph_data_rh=NULL, surface="white", views=c('t4'), rgloptions=rglo(), rglactions = list(), draw_colorbar = FALSE, makecmap_options=mkco.seq(), bg=NULL, morph_data_both=NULL, style = "default") {

    if(is.null(morph_data_lh) && is.null(morph_data_rh)) {
        if(is.null(morph_data_both)) {
            stop(sprintf("Only two of 'morph_data_lh', 'morph_data_rh' and 'morph_data_both' can be NULL.\n"));
        } else {
            # Split the single vector into 2 vectors with lengths of the respective hemispheres.
            measure = vdata.split.by.hemi(subjects_dir, vis_subject_id, morph_data_both, surface=surface);
        }
    } else {
        if( ! is.null(morph_data_both)) {
            stop(sprintf("If 'morph_data_lh' or 'morph_data_rh' is given, 'morph_data_both' must be NULL.\n"));
        }
        if(is.character(morph_data_lh)) { # Treat as filepath
            morph_data_lh = freesurferformats::read.fs.morph(morph_data_lh);
        }
        if(is.character(morph_data_rh)) { # Treat as filepath
            morph_data_rh = freesurferformats::read.fs.morph(morph_data_rh);
        }
        measure = hemilist.wrap(morph_data_lh, 'lh');
        measure = hemilist.wrap(morph_data_rh, 'rh', measure);
    }

    return(vis.subject.morph.native(subjects_dir, vis_subject_id, measure, surface=surface, views=views, rgloptions=rgloptions, rglactions=rglactions, draw_colorbar=draw_colorbar, makecmap_options=makecmap_options, bg=bg, style = style));
}


#' @title Visualize clusters or activation data on the surface of any subject.
#'
#' @description This function is intended to plot symmetric data around zero (like positive and negative activation data, signed p-values, etc.) on a subject's surface. It is a thin wrapper around \code{\link[fsbrain]{vis.data.on.subject}}.
#'
#' @inheritParams vis.data.on.subject
#'
#' @param map_to_NA the value or value range that should **not** be considered a cluster, and should thus be plotted as background color. These values will be set to NA, leading to transparcent rendering, so the background will be visible instead. If a single value, only exactly this value is used (typically 0). If two values, they are interpreted as a range, and a values between them are mapped to NA. If you prefer to map the data to NA yourself before using this function or do not want to use a , pass `NULL`.
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
#' @export
vis.symmetric.data.on.subject <- function(subjects_dir, vis_subject_id, morph_data_lh=NULL, morph_data_rh=NULL, surface="white", views=c('t4'), rgloptions=rglo(), rglactions = list(), draw_colorbar = TRUE, makecmap_options=list('colFn'=cm.cbry(), symm=TRUE, col.na='#FFFFFF00', 'n'=200), map_to_NA=c(0), bg=NULL, morph_data_both=NULL, style = "default") {

    if(is.null(morph_data_lh) && is.null(morph_data_rh)) {
        if(is.null(morph_data_both)) {
            stop(sprintf("Only two of 'morph_data_lh', 'morph_data_rh' and 'morph_data_both' can be NULL.\n"));
        } else {
            # Split the single vector into 2 vectors with lengths of the respective hemispheres.
            morph_data_by_hemi = vdata.split.by.hemi(subjects_dir, vis_subject_id, morph_data_both, surface=surface);
            morph_data_lh = morph_data_by_hemi$lh;
            morph_data_rh = morph_data_by_hemi$rh;
        }
    } else {
        if( ! is.null(morph_data_both)) {
            stop(sprintf("If 'morph_data_lh' or 'morph_data_rh' is given, 'morph_data_both' must be NULL.\n"));
        }
        if(is.character(morph_data_lh)) { # Treat as filepath
            morph_data_lh = freesurferformats::read.fs.morph(morph_data_lh);
        }
        if(is.character(morph_data_rh)) { # Treat as filepath
            morph_data_rh = freesurferformats::read.fs.morph(morph_data_rh);
        }
    }

    morph_data_lh = perform.na.mapping(morph_data_lh, map_to_NA);
    morph_data_rh = perform.na.mapping(morph_data_rh, map_to_NA);

    return(vis.data.on.subject(subjects_dir, vis_subject_id, morph_data_lh, morph_data_rh, surface=surface, views=views, rgloptions=rgloptions, rglactions=rglactions, draw_colorbar=draw_colorbar, makecmap_options=makecmap_options, bg=bg, style = style));
}


#' Perform NA mapping for transparency
#'
#' @description Usually this is done so that the `NA` values are plotted transparently, so your can see a background color through the respective colors.
#'
#' @param data numeric vector
#'
#' @param map_to_NA the value or value range that should be mapped to `NA`. If a single value, only exactly this value is used. If two values, they are interpreted as a range, and a values between them are mapped to `NA`. If you pass `NULL`, the data are returned as-is.
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
#' @param color_lh vector of colors to visualize on the left hemisphere surface. Length must match number of vertices in hemi surface, or be a single color.
#'
#' @param color_rh vector of colors to visualize on the right hemisphere surface. Length must match number of vertices in hemi surface, or be a single color.
#'
#' @param color_both vector of colors to visualize on the left and right hemispheres. Alternative to 'color_lh' and 'color_rh'. Length must match sum of vertices in both hemis. Can also be a hemilist.
#'
#' @param style character string or rgl rendering style, see \code{\link[fsbrain]{get.rglstyle}}.
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
vis.color.on.subject <- function(subjects_dir, vis_subject_id, color_lh=NULL, color_rh=NULL, surface="white", views=c('t4'), rgloptions=rglo(), rglactions = list(), color_both=NULL, style = "default") {

    coloredmeshes = list();

    if(is.null(color_lh) && is.null(color_rh)) {
        if(is.null(color_both)) {
            stop(sprintf("Only two of 'color_lh', 'color_rh' and 'color_both' can be NULL.\n"));
        } else {
            # Split the single vector into 2 vectors with lengths of the respective hemispheres.
            if(is.hemilist(color_both)) {
                color_by_hemi = color_both;
            } else {
                color_by_hemi = vdata.split.by.hemi(subjects_dir, vis_subject_id, color_both, surface=surface);
            }
            color_lh = color_by_hemi$lh;
            color_rh = color_by_hemi$rh;
        }
    } else {
        if( ! is.null(color_both)) {
            stop(sprintf("If 'color_lh' or 'color_rh' is given, 'color_both' must be NULL.\n"));
        }
    }

    if(! is.null(color_lh)) {
        cmesh_lh = coloredmesh.from.color(subjects_dir, vis_subject_id, color_lh, 'lh', surface=surface);
        coloredmeshes$lh = cmesh_lh;
    }

    if(! is.null(color_rh)) {
        cmesh_rh = coloredmesh.from.color(subjects_dir, vis_subject_id, color_rh, 'rh', surface=surface);
        coloredmeshes$rh = cmesh_rh;
    }

    if(hasIn(rglactions, c('no_vis'))) {
        return(coloredmeshes);
    }

    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, style = style)));
}


#' @title Visualize a vertex mask on the surface of a subject.
#'
#' @description A mask is a logical vector that contains one value per vertex. You can create it manually, or use functions like \code{\link[fsbrain]{mask.from.labeldata.for.hemi}} to create and modify it. Check the example for this function.
#'
#' @inheritParams vis.data.on.subject
#'
#' @param mask_lh logical vector or NULL, the mask to visualize on the left hemisphere surface. Must have the same length as the lh surface of the vis_subject_id has vertices. If NULL, this surface will not be rendered. Only one of mask_lh or mask_rh is allowed to be NULL.
#'
#' @param mask_rh logical vector or NULL, the mask to visualize on the right hemisphere surface. Must have the same length as the rh surface of the vis_subject_id has vertices. If NULL, this surface will not be rendered. Only one of mask_lh or mask_rh is allowed to be NULL.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization.
#'
#' @note Drawing a colorbar for label data makes limited sense, use a legend instead. The colorbar can give a rough overview of the relative number of label and non-label vertices though, so it is possible to request one.
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
#' @export
vis.mask.on.subject <- function(subjects_dir, vis_subject_id, mask_lh, mask_rh, surface="white", views=c('t4'), rgloptions=rglo(), rglactions = list(), draw_colorbar = FALSE, makecmap_options=list('colFn'=label.colFn.inv), style = "default") {

    if(is.null(mask_lh) && is.null(mask_rh)) {
        stop(sprintf("Only one of mask_lh or mask_rh can be NULL.\n"));
    }

    coloredmeshes = list();

    if(! is.null(mask_lh)) {
        coloredmeshes$lh = coloredmesh.from.mask(subjects_dir, vis_subject_id, mask_lh, 'lh', surface=surface, makecmap_options=makecmap_options);
    }

    if(! is.null(mask_rh)) {
        coloredmeshes$rh = coloredmesh.from.mask(subjects_dir, vis_subject_id, mask_rh, 'rh', surface=surface, makecmap_options=makecmap_options);
    }

    if(hasIn(rglactions, c('no_vis'))) {
        return(coloredmeshes);
    }

    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar, style = style)));
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
#' @param ... extra arguments to pass to \code{\link{coloredmesh.from.label}}.
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
#' @note Drawing a colorbar for label data makes limited sense, use a legend instead. The colorbar can give a rough overview of the relative number of label and non-label vertices though, so it is possible to request one.
#'
#' @family label functions
#' @family visualization functions
#'
#' @importFrom squash rainbow2
#' @export
vis.labeldata.on.subject <- function(subjects_dir, vis_subject_id, lh_labeldata, rh_labeldata, surface="white", views=c('t4'), rgloptions=rglo(), rglactions = list(), draw_colorbar = FALSE, makecmap_options=list('colFn'=label.colFn.inv), style = "default", ...) {

    if(is.null(lh_labeldata) && is.null(rh_labeldata)) {
        stop(sprintf("Only one of lh_labeldata or rh_labeldata can be NULL.\n"));
    }

    coloredmeshes = list();

    if(! is.null(lh_labeldata)) {
        coloredmeshes$lh = coloredmesh.from.label(subjects_dir, vis_subject_id, lh_labeldata, 'lh', surface=surface, makecmap_options=makecmap_options, ...);
    }

    if(! is.null(rh_labeldata)) {
        coloredmeshes$rh = coloredmesh.from.label(subjects_dir, vis_subject_id, rh_labeldata, 'rh', surface=surface, makecmap_options=makecmap_options, ...);
    }

    if(hasIn(rglactions, c('no_vis'))) {
        return(coloredmeshes);
    }

    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar, style = style)));
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
#' @export
vis.data.on.fsaverage <- function(subjects_dir=NULL, vis_subject_id="fsaverage", morph_data_lh=NULL, morph_data_rh=NULL, surface="white", views=c('t4'), rgloptions = rglo(), rglactions = list(), draw_colorbar = FALSE, makecmap_options=mkco.seq(), bg=NULL, morph_data_both=NULL, style = "default") {

    if(is.null(subjects_dir)) {
        subjects_dir = find.subjectsdir.of(subject_id=vis_subject_id, mustWork = TRUE);
    }

    return(invisible(vis.data.on.subject(subjects_dir, vis_subject_id, morph_data_lh, morph_data_rh, surface=surface, views=views, rgloptions=rgloptions, rglactions = rglactions, draw_colorbar = draw_colorbar, makecmap_options=makecmap_options, bg=bg, morph_data_both=morph_data_both, style = style)));
}


#' @title Visualize an annotation for a subject.
#'
#' @description Creates a surface mesh, loads the colors from the annotation, and renders the resulting colored mesh in an interactive window. If hemi is 'both', the data is rendered for the whole brain.
#'
#' @inheritParams vis.subject.morph.native
#'
#' @param atlas string. The atlas name. E.g., "aparc", "aparc.2009s", or "aparc.DKTatlas". Used to construct the name of the annotation file to be loaded. Can also be a hemilist of already loaded annotations.
#'
#' @param outline logical, whether to draw an outline only instead of filling the regions. Defaults to `FALSE`. Instead of passing `TRUE`, one can also pass a list of extra parameters to pass to \code{\link[fsbrain]{annot.outline}}, e.g., \code{outline=list('outline_color'='#000000')}. Using this increases computation time dramatically, sorry for the performance.
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
vis.subject.annot <- function(subjects_dir, subject_id, atlas, hemi='both', surface="white", views=c('t4'), rgloptions=rglo(), rglactions = list(), outline=FALSE, style = "default") {

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

    if(hasIn(rglactions, c('no_vis'))) {
        return(coloredmeshes);
    }

    return(invisible(brainviews(views, coloredmeshes, rgloptions = rgloptions, rglactions = rglactions, style = style)));
}



#' @title Visualize arbitrary data, one value per atlas region, on the surface of any subject (including template subjects).
#'
#' @description This function can be used for rendering a single value (color) for all vertices of an atlas region. The typical usecase is the visualization of results of atlas-based analyses, e.g., p-value, means or other aggregated values over all vertices of a region.
#'
#' @inheritParams vis.subject.morph.native
#'
#' @param atlas string. The brain atlas to use. E.g., 'aparc' or 'aparc.a2009s'.
#'
#' @param lh_region_value_list named list. A list for the left hemisphere in which the names are atlas regions, and the values are the value to write to all vertices of that region. You can pass an unnamed list, but then the its length must exactly match the number of atlas regions. The order of values must also match the order of regions in the annotation, of course. The resulting mapping will be printed so you can check it (unless 'silent' is set).
#'
#' @param rh_region_value_list named list. A list for the right hemisphere in which the names are atlas regions, and the values are the value to write to all vertices of that region.
#'
#' @param value_for_unlisted_regions numerical scalar or `NA`, the value to assign to regions which do not occur in the region_value_lists. Defaults to `NA`.
#'
#' @param silent logical, whether to suppress mapping info in case of unnamed region value lists (see 'lh_region_value_list' description).
#'
#' @param border logical, whether to add a black border around the regions. Alternatively, the parameter can be given as a named list with entries 'color' and 'expand_inwards', where the latter defines the borders thickness. E.g., \code{border = list('color'='#FF0000', 'expand_inwards'=2L)}. Border computation is slow, sorry.
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
vis.region.values.on.subject <- function(subjects_dir, subject_id, atlas, lh_region_value_list, rh_region_value_list, surface="white", views=c('t4'), rgloptions=rglo(), rglactions = list(), value_for_unlisted_regions = NA, draw_colorbar = FALSE, makecmap_options=mkco.heat(), bg=NULL, silent=FALSE, style = "default", border=NULL) {
    morph_like_data = spread.values.over.subject(subjects_dir, subject_id, atlas, lh_region_value_list, rh_region_value_list, value_for_unlisted_regions = value_for_unlisted_regions, silent=silent);
    if(! is.null(border)) {
        border_expand_inwards = getIn(border, 'expand_inwards', default = 0L);
        border_color = getIn(border, 'color', default = "#000000");
        border_verts = subject.annot.border(subjects_dir, subject_id, hemi="both", atlas=atlas, expand_inwards=border_expand_inwards); # these are by region, we use 'unlist' in the next line to merge all of them together.
        lh_replace_list = list();
        lh_replace_list[[border_color]]=unlist(unname(border_verts$lh));
        rh_replace_list = list();
        rh_replace_list[[border_color]]=unlist(unname(border_verts$rh));
        rglactions = list('replace_final_colors_by_vertex'=list('lh'=lh_replace_list, 'rh'=rh_replace_list));
    }
    return(invisible(vis.data.on.subject(subjects_dir, subject_id, morph_like_data$lh, morph_like_data$rh, surface=surface, views=views, rgloptions=rgloptions, rglactions=rglactions, draw_colorbar = draw_colorbar, makecmap_options=makecmap_options, bg=bg, style = style)));
}


#' @title Visualize fs.surface mesh
#'
#' @description Render a mesh. All mesh formats supported by the *freesurferformats* package are supported, including OFF, PLY, OBJ, STL, and many more.
#'
#' @param fs_surface an fs.surface instance, as returned by function like \code{\link[fsbrain]{subject.surface}} or \code{\link[freesurferformats]{read.fs.surface}}. If a character string, it is assumed to be the full path of a surface file, and the respective file is loaded with \code{\link[freesurferformats]{read.fs.surface}}. If parameter 'hemi' is 'both', this must be a hemilist. A single \code{rgl::tmesh} is also fine.
#'
#' @param col vector of colors, the per-vertex-colors. Defaults to white. Must be a single color or one color per vertex. If parameter 'hemi' is 'both', this must be a hemilist.
#'
#' @param per_vertex_data numerical vector, per-vertex data. If given, takes precedence over 'col'. Used to color the mesh using the colormap options in parameter 'makecmap_options'. If a character string, it is assumed to be the full path of a morphometry data file, and the respective file is loaded with \code{\link[freesurferformats]{read.fs.morph}}. If parameter 'hemi' is 'both', this must be a hemilist.
#'
#' @inheritParams fs.coloredmesh
#'
#' @inheritParams vis.subject.morph.native
#'
#' @param ... extra parameters to pass to \code{\link[fsbrain]{vis.coloredmeshes}}.
#'
#' @note This function can be used to visualize arbitrary triangular meshes in R. Despite its name, it is not limited to brain surface meshes.
#'
#' @return see \code{\link{vis.coloredmeshes}}
#'
#' @export
vis.fs.surface <- function(fs_surface, col="white", per_vertex_data=NULL, hemi="lh", makecmap_options=mkco.seq(), ...) {
    if(! hemi %in% c('lh', 'rh', 'both')) {
        stop("Hemi must be one of 'lh', 'rh', 'both'.");
    }
    if( ! is.null(per_vertex_data)) {
        col = NULL;
        if(is.hemilist(per_vertex_data)) {
            if(hemi != 'both') {
                stop("Parameter 'hemi' must be 'both' if hemilist is passed for 'per_vertex_data'.");
            }
            if(is.character(per_vertex_data$lh)) {   # treat as path to a morph file
                per_vertex_data$lh = freesurferformats::read.fs.morph(per_vertex_data$lh);
            }
            if(is.character(per_vertex_data$rh)) {   # treat as path to a morph file
                per_vertex_data$rh = freesurferformats::read.fs.morph(per_vertex_data$rh);
            }
        } else {
            if(is.character(per_vertex_data)) {   # treat as path to a morph file
                per_vertex_data = freesurferformats::read.fs.morph(per_vertex_data);
            }
        }
    }
    if(is.hemilist(fs_surface)) {
        if(hemi != 'both') {
            stop("Parameter 'hemi' must be 'both' if hemilist is passed for 'fs_surface'.");
        }
        if(is.character(fs_surface$lh)) {
            fs_surface$lh = freesurferformats::read.fs.surface(fs_surface$lh);
        }
        if(is.character(fs_surface$rh)) {
            fs_surface$rh = freesurferformats::read.fs.surface(fs_surface$rh);
        }
    } else {
        if(is.character(fs_surface)) {
            fs_surface = freesurferformats::read.fs.surface(fs_surface);
        } else if("mesh3d" %in% class(fs_surface)) {
            fs_surface = tmesh3d.to.fs.surface(fs_surface);
        }
    }
    cm_list = list();
    if(hemi == 'both') {
        if(! is.hemilist(col)) {
            col = list('lh'=col, 'rh'=col);
        }
        cm_list[['lh']] = coloredmesh.from.preloaded.data(fs_surface$lh, morph_data=per_vertex_data$lh, col=col$lh, hemi='lh', makecmap_options=makecmap_options);
        cm_list[['rh']] = coloredmesh.from.preloaded.data(fs_surface$rh, morph_data=per_vertex_data$rh, col=col$rh, hemi='rh', makecmap_options=makecmap_options);
    } else {
        cm_list[[hemi]] = coloredmesh.from.preloaded.data(fs_surface, morph_data=per_vertex_data, col=col, hemi=hemi, makecmap_options=makecmap_options);
    }
    return(invisible(vis.coloredmeshes(cm_list, ...)));
}

#' @title Determine vertex count of left hemi from hemilist of surfaces or the count itself.
#'
#' @param surfaces hemilist of surfaces, or a single integer, which will be interpreted as the number of vertices of the left hemisphere surface.
#'
#' @return integer, the number of vertices.
#'
#' @export
numverts.lh <- function(surfaces) {
    lh_nv = NULL; # vertex count of left hemi
    if(is.hemilist(surfaces)) {
        if(freesurferformats::is.fs.surface(surfaces$lh)) {
            lh_nv = nrow(surfaces$lh$vertices);
        }
    } else if(is.integer(surfaces)) {
        if(length(surfaces) == 1L) {
            lh_nv = surfaces;
        }
    } else {
        stop("Invalid 'surfaces' parameter.");
    }

    if(is.null(lh_nv)) {
        stop("Cannot determine vertex count of left hemi, invalid 'surfaces' parameter.");
    }
    return(lh_nv);
}


#' @title Determine vertex count of right hemi from hemilist of surfaces or the count itself.
#'
#' @param surfaces hemilist of surfaces, or a single integer, which will be interpreted as the number of vertices of the right hemisphere surface.
#'
#' @return integer, the number of vertices.
#'
#' @export
numverts.rh <- function(surfaces) {
    rh_nv = NULL; # vertex count of right hemi
    if(is.hemilist(surfaces)) {
        if(freesurferformats::is.fs.surface(surfaces$rh)) {
            rh_nv = nrow(surfaces$rh$vertices);
        }
    } else if(is.integer(surfaces)) {
        if(length(surfaces) == 1L) {
            rh_nv = surfaces;
        }
    } else {
        stop("Invalid 'surfaces' parameter.");
    }

    if(is.null(rh_nv)) {
        stop("Cannot determine vertex count of right hemi, invalid 'surfaces' parameter.");
    }
    return(rh_nv);
}


#' @title Transform surfaces indices which go over two surfaces to per-hemi indices.
#'
#' @param surfaces hemilist of surfaces, or a single integer, which will be interpreted as the number of vertices of the left hemisphere surface.
#'
#' @param vertices positive integer vector, the query vertex indices. Must be in range 1 to (num_verts_lh + num_verts_rh).
#'
#' @return named list with the following entries: 'vertices', hemilist of indices for the hemispheres. 'query_indices', hemilist of the indices of the respective vertices in the vector that was passed as parameter vertices. 'vertices_hemi': vector of character strings containing the hemi value (lh or rh) for the query vertices.
#'
#' @keywords internal
per.hemi.vertex.indices <- function(surfaces, vertices) {
    lh_nv = numverts.lh(surfaces);

    lh_vertices_idx = which(vertices <= lh_nv);
    lh_vertices = vertices[lh_vertices_idx];

    # Save which hemi each vertex belongs to.
    vertices_hemi = vertex.hemis(surfaces, vertices);

    rh_vertices_idx = which(vertices > lh_nv);
    rh_vertices = vertices[rh_vertices_idx];
    rh_vertices = rh_vertices - lh_nv;
    vertices = list('lh' = lh_vertices, 'rh' = rh_vertices);
    source_indices = list('lh' = lh_vertices_idx, 'rh' = rh_vertices_idx);
    return(list('vertices' = vertices, 'query_indices' = source_indices, 'vertices_hemi' = vertices_hemi));
}
