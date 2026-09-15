# Functions to visualize data on atlases that ship their own surface mesh, instead of being
# defined by a parcellation of a cortical surface. The main use case is the subcortical atlas,
# but any atlas that comes with a mesh and a FreeSurfer annotation file works.
#
# Note that in contrast to the cortical surface, a mesh atlas is defined for one specific
# template space: the mesh and the annotation file are stored in the subject directory
# ('surf/lh.<surface>' and 'label/lh.<atlas>.annot'), so they are tied to the subject they
# are stored in. If you want to render a cortex as context, it must be defined in the same
# space, i.e., it must be taken from the same subject.


#' @title Get the file paths of a mesh atlas for a subject.
#'
#' @description Compute the paths of the surface mesh and annotation files of a mesh atlas.
#'   The files are not required to exist, see \code{\link[fsbrain]{mesh.atlas.check.files}}.
#'
#' @param subjects_dir string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id string. The subject identifier.
#'
#' @param atlas string. The atlas name, used to construct the annotation file name.
#'
#' @param surface string. The name of the surface mesh that belongs to the atlas.
#'
#' @return vector of strings, the file paths of the atlas files.
#'
#' @keywords internal
mesh.atlas.file.paths <- function(subjects_dir, subject_id, atlas, surface) {
    return(c(
        file.path(subjects_dir, subject_id, "surf", sprintf("lh.%s", surface)),
        file.path(subjects_dir, subject_id, "surf", sprintf("rh.%s", surface)),
        file.path(subjects_dir, subject_id, "label", sprintf("lh.%s.annot", atlas)),
        file.path(subjects_dir, subject_id, "label", sprintf("rh.%s.annot", atlas))
    ));
}


#' @title Find a subjects dir that contains a mesh atlas for a subject.
#'
#' @description Several locations can contain the data of a template subject like fsaverage, e.g.\ the package cache and the subjects dir of a FreeSurfer installation, and not all of them necessarily contain the mesh atlas. This function checks the locations in the order in which they are returned by \code{\link[fsbrain]{find.subjectsdir.of}} and returns the first one that actually contains the atlas files. If none contains them, the first location found is returned, so that the caller can report the missing files.
#'
#' @inheritParams mesh.atlas.file.paths
#'
#' @return string, the path of a subjects dir.
#'
#' @keywords internal
mesh.atlas.resolve.subjects.dir <- function(subject_id, atlas = "subcortical", surface = "subcortical") {
    found = find.subjectsdir.of(subject_id = subject_id, mustWork = TRUE);   # returns the path of the first location, or stops with a helpful message.
    all_locations = find.subjectsdir.of(subject_id = subject_id, mustWork = FALSE)$found_all_locations;
    for(location in all_locations) {
        if(all(file.exists(mesh.atlas.file.paths(location, subject_id, atlas, surface)))) {
            return(location);
        }
    }
    return(found);
}


#' @title Check that a mesh atlas is available for a subject.
#'
#' @description Check that the surface mesh and the annotation files of a mesh atlas exist for
#'   a subject, and stop with a helpful error message otherwise.
#'
#' @param subjects_dir string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subject_id string. The subject identifier.
#'
#' @param atlas string. The atlas name, used to construct the annotation file name.
#'
#' @param surface string. The name of the surface mesh that belongs to the atlas.
#'
#' @return vector of strings, the file paths of the atlas files, invisibly. The function stops
#'   if any of the files is missing.
#'
#' @keywords internal
mesh.atlas.check.files <- function(subjects_dir, subject_id, atlas, surface) {

    atlas_files = mesh.atlas.file.paths(subjects_dir, subject_id, atlas, surface);

    missing_files = atlas_files[! file.exists(atlas_files)];
    if(length(missing_files) > 0L) {
        stop(sprintf("The mesh atlas '%s' (mesh: '%s') is not available for subject '%s' in subjects dir '%s', the following file(s) are missing:\n  %s\nUse 'fsbrain::download_optional_data()' or 'fsbrain::download_fsaverage_atlases()' to download the atlas into the package cache, or make sure the files exist for your subject.\n", atlas, surface, subject_id, subjects_dir, paste(missing_files, collapse = "\n  ")));
    }

    return(invisible(atlas_files));
}


#' @title Compute the context layer of a mesh atlas visualization.
#'
#' @description Compute the optional context mesh (typically a semi-transparent cortex) that is
#'   rendered together with the data meshes of a mesh atlas.
#'
#' @param subjects_dir string, the subjects dir to use for the context mesh.
#'
#' @param subject_id string, the subject id to use for the context mesh.
#'
#' @param cortex the context definition. Either a character string (the name of the surface mesh to
#'   use, e.g., 'white'), a named list of options for the context mesh (supported entries are
#'   'surface', 'color', 'alpha', 'style', 'subjects_dir' and 'subject_id'), an
#'   \code{fs.coloredmesh} instance, or a hemilist of such instances (which is then used as-is).
#'
#' @return a named list with entries 'lh' and 'rh', the context coloredmeshes.
#'
#' @keywords internal
mesh.atlas.context.layer <- function(subjects_dir, subject_id, cortex) {

    if(is.fs.coloredmesh(cortex)) {   # a single, pre-built mesh: use as-is.
        return(list(cortex));
    }

    if(is.list(cortex) && all(c("lh", "rh") %in% names(cortex)) && is.fs.coloredmesh(cortex$lh) && is.fs.coloredmesh(cortex$rh)) {
        return(cortex);   # a hemilist of pre-built meshes: use as-is.
    }

    ctx_subjects_dir = subjects_dir;
    ctx_subject_id = subject_id;
    ctx_surface = "white";
    ctx_color = "#B0B0B0";
    ctx_alpha = 0.12;
    ctx_style = NULL;

    if(is.character(cortex)) {
        ctx_surface = cortex;
    } else if(is.list(cortex)) {
        ctx_subjects_dir = getIn(cortex, "subjects_dir", default = ctx_subjects_dir);
        ctx_subject_id = getIn(cortex, "subject_id", default = ctx_subject_id);
        ctx_surface = getIn(cortex, "surface", default = ctx_surface);
        ctx_color = getIn(cortex, "color", default = ctx_color);
        ctx_alpha = getIn(cortex, "alpha", default = ctx_alpha);
        ctx_style = getIn(cortex, "style", default = ctx_style);
    } else {
        stop("Parameter 'cortex' must be a surface name, a named list of options for the context mesh, an fs.coloredmesh instance, or a hemilist of fs.coloredmesh instances.\n");
    }

    if(is.null(ctx_style)) {
        # Mostly the 'glass' style, but with a lower alpha value to keep the data meshes visible.
        ctx_style = list("shininess" = 50, "specular" = "black", "alpha" = ctx_alpha, "front" = "filled", "back" = "culled");
    }

    # The context mesh and the atlas mesh must be defined in the same coordinate space, so we
    # check that the requested surface is available for the subject and explain how to get it.
    if(is.character(ctx_surface)) {
        missing_ctx_files = file.path(ctx_subjects_dir, ctx_subject_id, "surf", sprintf("%s.%s", c("lh", "rh"), ctx_surface));
        missing_ctx_files = missing_ctx_files[! file.exists(missing_ctx_files)];
        if(length(missing_ctx_files) > 0L) {
            stop(sprintf("The context surface '%s' is not available for subject '%s' in subjects dir '%s', the following file(s) are missing:\n  %s\nNote that the context mesh must be defined in the same space as the atlas mesh, so it must come from the same subject. Use 'fsbrain::download_fsaverage(accept_freesurfer_license = TRUE)' to download the cortical surfaces of the fsaverage template subject into the package cache, or pass another subjects dir via 'cortex = list(\"subjects_dir\" = ...)'.\n", ctx_surface, ctx_subject_id, ctx_subjects_dir, paste(missing_ctx_files, collapse = "\n  ")));
        }
    }

    ctx_meshes = list();
    for(hemi in c("lh", "rh")) {
        ctx_meshes[[hemi]] = coloredmesh.from.color(ctx_subjects_dir, ctx_subject_id, ctx_color, hemi, surface = ctx_surface, style = ctx_style);
    }

    return(ctx_meshes);
}


#' @title Visualize one value per region of the subcortical atlas of a subject.
#'
#' @description Render the subcortical structures of a subject and assign one color per structure, based on one value per atlas region. The subcortical atlas is not defined on the cortical surface: it comes with its own surface mesh that contains the 8 subcortical structures per hemisphere (accumbens area, amygdala, caudate, hippocampus, pallidum, putamen, thalamus and lateral ventricle). The mesh and the annotation file are expected in the subject directory ('surf/lh.subcortical', 'surf/rh.subcortical', 'label/lh.subcortical.annot', 'label/rh.subcortical.annot').
#'
#'   The atlas files for the fsaverage template subject are not part of FreeSurfer and are not
#'   required for the package to work. They can be downloaded with \code{\link[fsbrain]{download_optional_data}} or \code{\link[fsbrain]{download_fsaverage_atlases}} into the package cache, which is searched for a subject named 'fsaverage' by default.
#'
#'   Optionally, the structures can be rendered inside a semi-transparent context mesh, typically the cortex of the same subject (see parameter 'cortex'). Note that the context mesh and the atlas mesh must be defined in the same coordinate space, which is why both are taken from the same subject by default.
#'
#' @param subjects_dir string or `NULL`. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier. If `NULL`, the locations searched by \code{\link[fsbrain]{find.subjectsdir.of}} (package cache and FreeSurfer/SUBJECTS_DIR configuration) are checked for one that contains the atlas files, and the first such location is used.
#'
#' @param subject_id string. The subject identifier. Defaults to 'fsaverage', the template subject for which the subcortical atlas is available for download.
#'
#' @param lh_region_value_list named list. A list for the left hemisphere in which the names are atlas regions, and the values are the value to write to all vertices of that region, see \code{\link[fsbrain]{vis.region.values.on.subject}}.
#'
#' @param rh_region_value_list named list, the same for the right hemisphere.
#'
#' @param atlas string. The name of the atlas to use. Defaults to 'subcortical', the subcortical atlas that ships with the package. Used to construct the annotation file name.
#'
#' @param surface string. The name of the surface mesh that belongs to the atlas. Defaults to 'subcortical'. Used to construct the surface file name, in contrast to the other vis functions this is not a cortical surface.
#'
#' @param cortex `NULL` or the definition of a context mesh to render the structures in, typically a semi-transparent cortex of the same subject. Supported values are a character string (the surface name, e.g., 'white' or 'pial'), a named list of options for the context mesh (entries 'surface', 'color', 'alpha', 'style', 'subjects_dir' and 'subject_id'), an \code{\link[fsbrain]{coloredmesh.from.color}} instance, or a hemilist of such instances. Use `NULL` to render the structures without any context.
#'
#' @param views list of strings. The views to render. Defaults to lateral and medial views of both hemispheres. See \code{\link[fsbrain]{get.view.angle.names}} for valid entries.
#'
#' @param rgloptions option list passed to \code{\link[rgl]{par3d}}. Example: \code{rgloptions = list("windowRect"=c(50,50,1000,1000))}.
#'
#' @param rglactions named list. A list in which the names are from a set of pre-defined actions, see \code{\link[fsbrain]{rglactions}}. Note that the action 'shift_hemis_apart' is not supported here: the structures of a mesh atlas are rendered in their anatomical position.
#'
#' @param value_for_unlisted_regions numerical scalar or `NA`, the value to assign to regions which do not occur in the region value lists, see \code{\link[fsbrain]{vis.region.values.on.subject}}.
#'
#' @param draw_colorbar logical. Whether to draw a colorbar. Defaults to FALSE, see \code{\link[fsbrain]{coloredmesh.plot.colorbar.separate}} for a better looking alternative.
#'
#' @param makecmap_options named list of parameters to pass to \code{\link[squash]{makecmap}}. Must not include the unnamed first parameter, which is derived from the data.
#'
#' @param style a rendering style for the data meshes, see \code{\link[fsbrain]{get.rglstyle}}. Defaults to 'default'. The context mesh defined via parameter 'cortex' has its own style, see there.
#'
#' @param silent logical, whether to suppress messages.
#'
#' @return list of coloredmeshes. The coloredmeshes used for the visualization, invisibly. The list contains the data meshes (and the context meshes, if any) as a flat list, so it can be passed to \code{\link[fsbrain]{export}} to save the rendered views into an image file.
#'
#' @note The subcortical atlas is defined in MNI305 space (fsaverage surface RAS), so it can be combined with the cortical surfaces of the fsaverage template subject. The 8 structures per hemisphere are colored with the standard FreeSurfer 'aseg' colors when you visualize the atlas itself, this function assigns data-driven colors instead. Region names are the FreeSurfer 'aseg' structure names, e.g., 'Left-Hippocampus' or 'Right-Thalamus-Proper'.
#'
#'   This function is not limited to the subcortical atlas: any atlas that comes with its own mesh and annotation files works, pass the respective names via parameters 'atlas' and 'surface'.
#'
#' @examples
#' \dontrun{
#'    fsbrain::download_optional_data();   # includes the subcortical atlas for fsaverage
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'
#'    # One value per region, for all 8 subcortical structures of the left and right hemisphere.
#'    lh_region_values = list("Left-Accumbens-area"=0.1, "Left-Amygdala"=0.2,
#'     "Left-Caudate"=0.3, "Left-Hippocampus"=0.4, "Left-Pallidum"=0.5,
#'     "Left-Putamen"=0.6, "Left-Thalamus-Proper"=0.7, "Left-Lateral-Ventricle"=0.8);
#'    rh_region_values = list("Right-Accumbens-area"=0.1, "Right-Amygdala"=0.2,
#'     "Right-Caudate"=0.3, "Right-Hippocampus"=0.4, "Right-Pallidum"=0.5,
#'     "Right-Putamen"=0.6, "Right-Thalamus-Proper"=0.7, "Right-Lateral-Ventricle"=0.8);
#'
#'    # Render the structures on their own, and save the result to a file:
#'    cm = vis.subcortical.region.values(subjects_dir, "fsaverage", lh_region_values,
#'     rh_region_values, rglactions = list("no_vis" = TRUE));
#'    export(cm, colorbar_legend = "my values", output_img = "subcortical.png");
#'
#'    # Render the structures inside a semi-transparent cortex:
#'    cm_ctx = vis.subcortical.region.values(subjects_dir, "fsaverage", lh_region_values,
#'     rh_region_values, cortex = "white", rglactions = list("no_vis" = TRUE));
#'    export(cm_ctx, colorbar_legend = "my values", output_img = "subcortical_in_cortex.png");
#' }
#'
#' @family visualization functions
#' @family region-based visualization functions
#'
#' @export
vis.subcortical.region.values <- function(subjects_dir = NULL, subject_id = "fsaverage",
        lh_region_value_list, rh_region_value_list, atlas = "subcortical", surface = "subcortical",
        cortex = NULL, views = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"),
        rgloptions = rglo(), rglactions = list(), value_for_unlisted_regions = NA,
        draw_colorbar = FALSE, makecmap_options = mkco.seq(), style = "default", silent = FALSE) {

    if(is.null(lh_region_value_list) && is.null(rh_region_value_list)) {
        stop("At least one of the parameters 'lh_region_value_list' and 'rh_region_value_list' must be given.\n");
    }

    if(! silent) {
        cat(sprintf("Visualizing atlas region values for atlas '%s' (mesh: '%s') of subject '%s'.\n", atlas, surface, subject_id));
    }

    if(is.null(subjects_dir)) {
        subjects_dir = mesh.atlas.resolve.subjects.dir(subject_id = subject_id, atlas = atlas, surface = surface);
    }

    mesh.atlas.check.files(subjects_dir, subject_id, atlas, surface);

    if(rglactions.has.key(rglactions, 'shift_hemis_apart')) {
        warning("The rglactions key 'shift_hemis_apart' is not supported by 'vis.subcortical.region.values', the structures are rendered in their anatomical position.\n");
    }

    # Compute the data meshes, i.e., one coloredmesh per hemisphere which contains the region values.
    data_meshes = vis.region.values.on.subject(subjects_dir, subject_id, atlas = atlas, surface = surface,
        lh_region_value_list = lh_region_value_list, rh_region_value_list = rh_region_value_list,
        value_for_unlisted_regions = value_for_unlisted_regions, makecmap_options = makecmap_options,
        rglactions = utils::modifyList(list("no_vis" = TRUE), rglactions), silent = silent);

    # The data meshes get the style requested by the user.
    data_meshes = lapply(data_meshes, function(cmesh) { cmesh$style = style; return(cmesh); });

    # Compute the optional context meshes (e.g., a semi-transparent cortex). They are kept free of
    # colormap metadata, otherwise they would contribute to the colorbar of the data meshes.
    context_meshes = NULL;
    if(! is.null(cortex)) {
        context_meshes = mesh.atlas.context.layer(subjects_dir, subject_id, cortex);
    }

    # Visualize the meshes. This has to be a flat list of meshes (and must not be a hemilist),
    # otherwise one of the two meshes of a hemisphere would silently be dropped by the renderers.
    # The order matters: the (potentially semi-transparent) context mesh is rendered first, the
    # opaque data meshes are rendered after (and thus on top of) it. Otherwise the context mesh
    # would hide the structures.
    renderable = c();
    if(! is.null(context_meshes)) {
        renderable = c(renderable, unname(context_meshes));
    }
    if(! is.null(data_meshes$lh)) {
        renderable = c(renderable, list(data_meshes$lh));
    }
    if(! is.null(data_meshes$rh)) {
        renderable = c(renderable, list(data_meshes$rh));
    }

    if(! hasIn(rglactions, c('no_vis'))) {
        # Per-mesh styles are used here, the meshes have been assigned their style above.
        brainviews(views, renderable, rgloptions = rgloptions, rglactions = rglactions, style = "from_mesh", draw_colorbar = draw_colorbar);
    }

    return(invisible(renderable));
}
