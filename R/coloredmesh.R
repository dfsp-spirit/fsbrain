# Functions for generating coloredmeshes from data.


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
#' @param clip numeric vector of length 2 or NULL. If given, the 2 values are interpreted as lower and upper percentiles, and the morph data is clipped at the given lower and upper percentile (see \code{\link[fsbrain]{clip.data}}). Defaults to NULL (no data clipping).
#'
#' @param cortex_only logical, whether to mask the medial wall, i.e., whether the morphometry data for all vertices which are *not* part of the cortex (as defined by the label file `label/?h.cortex.label`) should be replaced with NA values. In other words, setting this to TRUE will ignore the values of the medial wall between the two hemispheres. If set to true, the mentioned label file needs to exist for the subject. Defaults to FALSE.
#'
#' @param makecmap_options named list of parameters to pass to \code{\link{makecmap}}. Must not include the unnamed first parameter, which is derived from 'measure'.
#'
#' @return coloredmesh. A named list with entries: "mesh" the \code{\link{tmesh3d}} mesh object. "col": the mesh colors. "render", logical, whether to render the mesh. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @family coloredmesh functions
#'
#' @export
#' @importFrom squash cmap makecmap
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.morph.native <- function(subjects_dir, subject_id, measure, hemi, surface="white", clip=NULL, cortex_only=FALSE, makecmap_options=mkco.seq()) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

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
    } else if(is.hemilist(surface)) {
        surface_mesh = surface[[hemi]];
        if(! freesurferformats::is.fs.surface(surface_mesh)) {
            stop(sprintf("Hemilist in parameter 'surface' does not contain an fs.surface instance for hemi '%s'.\n", hemi));
        }
    } else {
        surface_mesh = subject.surface(subjects_dir, subject_id, surface, hemi);
    }

    if(nrow(surface_mesh$vertices) != length(morph_data)) {
        warning(sprintf("Data mismatch: surface has %d vertices, but %d color values passed in argument 'measure'.\n", nrow(surface_mesh$vertices), length(morph_data)));
    }

    mesh = fs.surface.to.tmesh3d(surface_mesh);

    morph_data_hl = hemilist.wrap(morph_data, hemi);
    cmr = common.makecmap.range(makecmap_options, lh_data = morph_data_hl$lh, rh_data = morph_data_hl$rh, return_metadata = TRUE);
    map = cmr$map;
    col = cmr$collayer[[hemi]];

    return(fs.coloredmesh(mesh, col, hemi, metadata=list("src_data"=morph_data, "fs_mesh"=surface_mesh, "map"=map, "data_range"=range(morph_data, finite=TRUE), "makecmap_options"=makecmap_options)));
}


#' @title Get an rgl tmesh3d instance from a brain surface mesh.
#'
#' @param surface an fs.surface instance, as returned by \code{subject.surface} or \code{freesurferformats::read.fs.surface}.
#'
#' @return a tmesh3d instance, see \code{rgl::tmesh3d} for details.
#'
#' @export
fs.surface.to.tmesh3d <- function(surface) {
    if( ! freesurferformats::is.fs.surface(surface)) {
        stop("Parameter 'surface' must be an instance of freesurferformats::fs.surface.");
    }
    return(rgl::tmesh3d(c(t(surface$vertices)), c(t(surface$faces)), homogeneous=FALSE));
}

#' @title Get an fs.surface brain mesh from an rgl tmesh3d instance.
#'
#' @param tmesh a tmesh3d instance, see \code{rgl::tmesh3d} for details.
#'
#' @return an fs.surface instance, as returned by \code{subject.surface} or \code{freesurferformats::read.fs.surface}.
#'
#' @export
tmesh3d.to.fs.surface <- function(tmesh) {
    vertices = t(tmesh$vb[1:3,]);
    faces = t(tmesh$it);
    surface = list('vertices'=vertices, 'faces'=faces);
    class(surface) <- c(class(surface), 'fs.surface');
    return(surface);
}


#' @title Create a coloredmesh from a mesh and pre-defined colors.
#'
#' @inheritParams coloredmeshes.from.color
#'
#' @param color_data vector of hex color strings, a single one or one per vertex.
#'
#' @return coloredmesh. A named list with entries: "mesh" the \code{\link{tmesh3d}} mesh object. "col": the mesh colors. "render", logical, whether to render the mesh. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @note Do not call this directly, use \code{\link[fsbrain]{coloredmeshes.from.color}} instead.
#'
#' @keywords internal
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.color <- function(subjects_dir, subject_id, color_data, hemi, surface="white", metadata=list()) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    if(freesurferformats::is.fs.surface(surface)) {
        surface_mesh = surface;
    } else if(is.hemilist(surface)) {
        surface_mesh = surface[[hemi]];
        if(! freesurferformats::is.fs.surface(surface_mesh)) {
            stop(sprintf("Hemilist in parameter 'surface' does not contain an fs.surface instance for hemi '%s'.\n", hemi));
        }
    } else {
        surface_mesh = subject.surface(subjects_dir, subject_id, surface, hemi);
    }
    mesh = fs.surface.to.tmesh3d(surface_mesh);

    if(nrow(surface_mesh$vertices) != length(color_data)) {
        if(length(color_data) == 1L) {
            color_data = rep(color_data, nrow(surface_mesh$vertices));
        } else if(length(color_data) == 0L) {
            color_data = rep('#FEFEFE', nrow(surface_mesh$vertices));
        } else {
            warning(sprintf("Data mismatch: surface has %d vertices, but %d color values passed in argument 'color_data'.\n", nrow(surface_mesh$vertices), length(color_data)));
        }
    }

    metadata$fs_mesh = surface_mesh;

    return(fs.coloredmesh(mesh, color_data, hemi, metadata=metadata));
}


#' @title Create coloredmeshes for both hemis using pre-defined colors.
#'
#' @inheritParams coloredmesh.from.morph.native
#'
#' @param color_data a hemilist containing vectors of hex color strings
#'
#' @param metadata a named list, can contain whatever you want. Typical entries are: 'src_data' a hemilist containing the source data from which the 'color_data' was created, optional. If available, it is encoded into the coloredmesh and can be used later to plot a colorbar. 'makecmap_options': the options used to created the colormap from the data.
#'
#' @return named list of coloredmeshes. Each entry is a named list with entries: "mesh" the \code{\link{tmesh3d}} mesh object. "col": the mesh colors. "render", logical, whether to render the mesh. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @family coloredmesh functions
#'
#' @export
coloredmeshes.from.color <- function(subjects_dir, subject_id, color_data, hemi, surface="white", metadata=list()) {
    if(!(hemi %in% c("lh", "rh", "both"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh', 'rh', or 'both' but is '%s'.\n", hemi));
    }

    if(hemi=="both") {
        if(! is.hemilist(color_data)) {
            stop("The parameter 'color_data' must be a named list with entries 'lh' and 'rh' if 'hemi' is 'both'.");
        }
        lh_cm = coloredmesh.from.color(subjects_dir, subject_id, color_data$lh, 'lh', surface=surface, metadata=metadata);
        rh_cm = coloredmesh.from.color(subjects_dir, subject_id, color_data$rh, 'rh', surface=surface, metadata=metadata);
        return(list("lh"=lh_cm, "rh"=rh_cm));
    } else {
        if(is.hemilist(color_data)) {
            color_data = hemilist.unwrap(color_data);
        }
        cm = coloredmesh.from.color(subjects_dir, subject_id, color_data, hemi, surface=surface, metadata=metadata);
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
#' @param template_subjects_dir The template subjects dir. If `NULL`, the value of the parameter 'subjects_dir' is used. Defaults to NULL. If you have FreeSurfer installed and configured, and are using the standard fsaverage subject, try passing the result of calling 'file.path(Sys.getenv('FREESURFER_HOME'), 'subjects')'.
#'
#' @return coloredmesh. A named list with entries: "mesh" the \code{\link{tmesh3d}} mesh object. "col": the mesh colors. "render", logical, whether to render the mesh. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @family coloredmesh functions
#'
#' @export
#' @importFrom squash cmap makecmap
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.morph.standard <- function(subjects_dir, subject_id, measure, hemi, fwhm, surface="white", template_subject='fsaverage', template_subjects_dir=NULL, clip = NULL, cortex_only=FALSE, makecmap_options=mkco.seq()) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    if(is.null(template_subjects_dir)) {
        template_subjects_dir = subjects_dir;
    }

    if(is.null(measure)) {
        morph_data = NULL;
    } else {
        if(is.numeric(measure)) {
            morph_data = measure;
        } else {
            morph_data = subject.morph.standard(subjects_dir, subject_id, measure, hemi, fwhm = fwhm, cortex_only = cortex_only, template_subject = template_subject);
        }
    }

    if(! is.null(clip)) {
        morph_data = clip.data(morph_data, lower=clip[1], upper=clip[2]);
    }

    if(freesurferformats::is.fs.surface(surface)) {
        surface_mesh = surface;
    } else if(is.hemilist(surface)) {
        surface_mesh = surface[[hemi]];
        if(! freesurferformats::is.fs.surface(surface_mesh)) {
            stop(sprintf("Hemilist in parameter 'surface' does not contain an fs.surface instance for hemi '%s'.\n", hemi));
        }
    } else {
        surface_mesh = subject.surface(template_subjects_dir, template_subject, surface, hemi);
    }

    if(nrow(surface_mesh$vertices) != length(morph_data)) {
        warning(sprintf("Data mismatch: template surface has %d vertices, but %d morphometry values passed in argument 'measure'. Is the template subject '%s' correct?\n", nrow(surface_mesh$vertices), length(morph_data), template_subject));
    }

    mesh = fs.surface.to.tmesh3d(surface_mesh);

    if(is.null(morph_data)) {
        map = NULL;
        col = 'white';
    } else {
        morph_data_hl = hemilist.wrap(morph_data, hemi);
        cmr = common.makecmap.range(makecmap_options, lh_data = morph_data_hl$lh, rh_data = morph_data_hl$rh);
        map = cmr$map;
        col = cmr$collayer[[hemi]];
    }
    return(fs.coloredmesh(mesh, col, hemi, metadata=list("src_data"=morph_data, "fs_mesh"=surface_mesh, "map"=map, "data_range"=range(morph_data, finite=TRUE), "makecmap_options"=makecmap_options)));
}


#' @title Create a coloredmesh from arbitrary data.
#'
#' @inheritParams coloredmesh.from.morph.native
#'
#' @param vis_subject_id string. The subject identifier from which to obtain the surface for data visualization. Example: 'fsaverage'.
#'
#' @param morph_data string. The morphometry data to use. E.g., 'area' or 'thickness.'
#'
#' @return coloredmesh. A named list with entries: "mesh" the \code{\link{tmesh3d}} mesh object. "col": the mesh colors. "render", logical, whether to render the mesh. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @family coloredmesh functions
#'
#' @export
#' @importFrom squash cmap makecmap
#' @importFrom rgl tmesh3d rgl.open wire3d
#' @importFrom utils modifyList
coloredmesh.from.morphdata <- function(subjects_dir, vis_subject_id, morph_data, hemi, surface="white", makecmap_options=mkco.seq()) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    if(freesurferformats::is.fs.surface(surface)) {
        surface_mesh = surface;
    } else {
        surface_mesh = subject.surface(subjects_dir, vis_subject_id, surface, hemi);
    }

    num_verts = nrow(surface_mesh$vertices);
    if(length(morph_data) != num_verts) {
        warning(sprintf("Received %d data values, but the hemi '%s' '%s' surface of visualization subject '%s' in dir '%s' has %d vertices. Counts must match.\n", length(morph_data), hemi, surface, vis_subject_id, subjects_dir, num_verts));
    }

    mesh = fs.surface.to.tmesh3d(surface_mesh);

    morph_data_hl = hemilist.wrap(morph_data, hemi);
    cmr = common.makecmap.range(makecmap_options, lh_data = morph_data_hl$lh, rh_data = morph_data_hl$rh);
    map = cmr$map;
    col = cmr$collayer[[hemi]];

    return(fs.coloredmesh(mesh, col, hemi, metadata=list("src_data"=morph_data, "fs_mesh"=surface_mesh, "map"=map, "data_range"=range(morph_data, finite=TRUE), "cmap_fun"=makecmap_options$colFn, "makecmap_options"=makecmap_options)));
}


#' @title Generate coloredmesh from loaded data.
#'
#' @param fs_surface an fs.surface instance or a character string, which will be interpreted as the path to a file and loaded with \code{freesurferformats::read.fs.surface}.
#'
#' @param morph_data numerical vector, per-vertex data (typically morphometry) for the mesh. If given, takes precedence over 'col' parameter.
#'
#' @param col vector of colors, typically hex color strings like '#FF00FF'. The per-vertex-colors for the mesh. Alternative to morph_data.
#'
#' @param hemi character string, one of 'lh' or 'rh'. Metadata, the hemisphere. May be used by visualization functions to decide whether to draw the mesh in certain views.
#'
#' @inheritParams coloredmesh.from.morphdata
#'
#' @return as fs.coloredmesh instance
#'
#' @export
coloredmesh.from.preloaded.data <- function(fs_surface, morph_data=NULL, col=NULL, hemi='lh', makecmap_options=mkco.seq()) {
    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    if( ! freesurferformats::is.fs.surface(fs_surface)) {
        if(is.character(fs_surface)) {
            fs_surface = freesurferformats::read.fs.surface(fs_surface);
        } else {
            stop("Parameter 'fs_surface' must be an fs.surface instance or filepath.");
        }
    }

    mesh = fs.surface.to.tmesh3d(fs_surface);
    if(! is.null(morph_data)) {
        if(! hasIn(makecmap_options, c('colFn'))) {
            makecmap_options$colFn = mkco.seq()$colFn;
        }
        data_range = range(morph_data, finite=TRUE);

        morph_data_hl = hemilist.wrap(morph_data, hemi);
        cmr = common.makecmap.range(makecmap_options, lh_data = morph_data_hl$lh, rh_data = morph_data_hl$rh);
        map = cmr$map;
        col = cmr$collayer[[hemi]];
    } else {
        map = NULL;
        data_range = NULL;
        if(is.null(col)) {
            col = rep("#FFFFFF", nrow(fs_surface$vertices));
        }
        if(length(col) == 1L) {
            col = rep(col, nrow(fs_surface$vertices));
        }
    }
    return(fs.coloredmesh(mesh, col, hemi, metadata=list("src_data"=morph_data, "fs_mesh"=fs_surface, "map"=map, "data_range"=data_range, "cmap_fun"=makecmap_options$colFn, "makecmap_options"=makecmap_options)));
}



#' @title Create a coloredmesh from an annotation of an atlas.
#'
#' @inheritParams coloredmesh.from.morph.native
#'
#' @param atlas string or a loaded annotation. If a string, interpreted as the atlas name that should be loaded to get te annotation. E.g., "aparc", "aparc.2009s", or "aparc.DKTatlas". Used to construct the name of the annotation file to be loaded.
#'
#' @param outline logical, whether to draw an outline only instead of filling the regions. Defaults to FALSE. Only makes sense if you did not pass an outline already. The current implementation for outline computation is rather slow, so setting this to TRUE will considerably increase computation time.
#'
#' @return coloredmesh. A named list with entries: "mesh" the \code{\link{tmesh3d}} mesh object. "col": the mesh colors. "render", logical, whether to render the mesh. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @family coloredmesh functions
#'
#' @export
#' @importFrom squash cmap makecmap
#' @importFrom rgl tmesh3d rgl.open wire3d
#' @importFrom utils modifyList
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
    } else if (freesurferformats::is.fs.annot(atlas)) {
        annot = atlas;
    } else if(is.hemilist(atlas)) {
        annot = atlas[[hemi]];
    } else {
        stop("Parameter 'atlas' has invalid type.");
    }
    mesh = fs.surface.to.tmesh3d(surface_mesh);

    if(is.list(outline)) {
        annot_outline_extra_options = outline;
        annot_outline_full_options = utils::modifyList(list(annot, surface_mesh), annot_outline_extra_options);
        col = do.call(annot.outline, annot_outline_full_options);
    } else if(outline == TRUE) {
        col = annot.outline(annot, surface_mesh);
    } else {
        col = annot$hex_colors_rgb;
    }

    if(nrow(surface_mesh$vertices) != length(col)) {
        warning(sprintf("Data mismatch: surface has %d vertices, but %d color values received from annotation.\n", nrow(surface_mesh$vertices), length(col)));
    }

    return(fs.coloredmesh(mesh, col, hemi, metadata = list('fs_mesh'=surface_mesh)));
}


#' @title Create a coloredmesh from a label.
#'
#' @inheritParams coloredmesh.from.morph.native
#'
#' @param label string or vector of integers. If a string, the name of the label file, without the hemi part (if any), but including the '.label' suffix. E.g., 'cortex.label' for '?h.cortex.label'. Alternatively, the already loaded label data as a vector of integers.
#'
#' @param binary logical, whether to treat the label as binary
#'
#' @return coloredmesh. A named list with entries: "mesh" the \code{\link{tmesh3d}} mesh object. "col": the mesh colors. "render", logical, whether to render the mesh. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @family coloredmesh functions
#'
#' @export
#' @importFrom squash cmap makecmap rainbow2
#' @importFrom rgl tmesh3d rgl.open wire3d
coloredmesh.from.label <- function(subjects_dir, subject_id, label, hemi, surface="white", makecmap_options=list('colFn'=squash::rainbow2), binary = TRUE) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

    if(freesurferformats::is.fs.surface(surface)) {
        surface_mesh = surface;
    } else {
        surface_mesh = subject.surface(subjects_dir, subject_id, surface, hemi);
    }

    if(is.character(label)) {
        if(binary) {
            label_data = subject.label(subjects_dir, subject_id, label, hemi);
        } else {
            label_data = subject.label(subjects_dir, subject_id, label, hemi, full = TRUE);
        }
    } else {
        label_data = label;
    }

    if(binary) {
        mask = mask.from.labeldata.for.hemi(list(label_data), nrow(surface_mesh$vertices));
        return(coloredmesh.from.mask(subjects_dir, subject_id, mask, hemi, surface=surface, makecmap_options=makecmap_options, surface_data=surface_mesh));
    } else {
        if(freesurferformats::is.fs.label(label_data)) {
            morph_data = rep(NA, subject.num.verts(subjects_dir, subject_id, surface = surface, hemi = hemi));
            morph_data[label_data$vertexdata$vertex_index] = label_data$vertexdata$value;
            return(coloredmesh.from.preloaded.data(surface_mesh, morph_data = morph_data));
        } else {
            stop("Parameter 'label' must be a full fs.label instance or a path if 'binary' is TRUE.");
        }
    }
}



#' @title Create a coloredmesh from a mask.
#'
#' @inheritParams coloredmesh.from.morph.native
#'
#' @param mask logical vector, contains one logical value per vertex.
#'
#' @param surface_data optional surface mesh object, as returned by \code{\link[fsbrain]{subject.surface}}. If given, used instead of loading the surface data from disk (which users of this function may already have done). Defaults to NULL.
#'
#' @return coloredmesh. A named list with entries: "mesh" the \code{\link{tmesh3d}} mesh object. "col": the mesh colors. "render", logical, whether to render the mesh. "hemi": the hemisphere, one of 'lh' or 'rh'.
#'
#' @family mask functions
#' @family coloredmesh functions
#' @export
coloredmesh.from.mask <- function(subjects_dir, subject_id, mask, hemi, surface="white", surface_data=NULL, makecmap_options=list('colFn'=squash::rainbow2)) {

    if(!(hemi %in% c("lh", "rh"))) {
        stop(sprintf("Parameter 'hemi' must be one of 'lh' or 'rh' but is '%s'.\n", hemi));
    }

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

    mesh = fs.surface.to.tmesh3d(surface_data);

    morph_data_hl = hemilist.wrap(morph_like_data, hemi);
    cmr = common.makecmap.range(makecmap_options, lh_data = morph_data_hl$lh, rh_data = morph_data_hl$rh);
    map = cmr$map;
    col = cmr$collayer[[hemi]];

    return(fs.coloredmesh(mesh, col, hemi, metadata=list("src_data"=morph_like_data, "fs_mesh"=surface_data, "map"=map, "data_range"=range(morph_like_data, finite=TRUE), "makecmap_options"=makecmap_options)));
}


#' @title Print description of a brain coloredmesh (S3).
#'
#' @param x brain surface with class `fs.coloredmesh`.
#'
#' @param ... further arguments passed to or from other methods
#'
#' @export
print.fs.coloredmesh <- function(x, ...) {
    cat(sprintf("Brain coloredmesh with %d vertices and %d faces.\n", ncol(x$mesh$vb), ncol(x$mesh$it)));             # nocov start
    cat(sprintf(" * Hemi is '%s', will be rendered: %s.\n", x$hemi, !x$render));
    cat(sprintf(" * Contains %d color values, %d unique colors.\n", length(x$col), length(unique(x$col))));           # nocov end
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
#' @param mesh a `mesh3d` instance as returned by \code{\link{tmesh3d}} or an `fs.surface` brain surface mesh as returned by functions like \code{\link[fsbrain]{subject.surface}}.
#'
#' @param col vector of vertex colors for the mesh, one color per vertex. Expanded if exactly one color.
#'
#' @param hemi character string, one of 'lh' or 'rh'. This may be used by visualization functions to decide whether or not to show this mesh in a certain view.
#'
#' @param render logical, whether to render this mesh during visualization
#'
#' @param metadata optional, named list containing metadata
#'
#' @param add_normals logical, whether to compute normals and save them in the mesh.
#'
#' @return an `fs.coloredmesh` instance. The only fields one should use in client code are 'mesh', 'hemi' and 'col', all others are considered internal and may change without notice.
#'
#' @importFrom freesurferformats is.fs.surface
#' @importFrom rgl tmesh3d addNormals
#' @export
fs.coloredmesh <- function(mesh, col, hemi, render=TRUE, metadata=NULL, add_normals=FALSE) {
    if(freesurferformats::is.fs.surface(mesh)) {
        mesh = fs.surface.to.tmesh3d(mesh);
    }
    if(!inherits(mesh, "mesh3d")) {
        stop("Parameter 'mesh' must be a mesh3d or fs.surface instance.");
    }
    if(add_normals) {
        mesh = rgl::addNormals(mesh);
    }

    if(ncol(mesh$vb) != length(col)) {
        if(length(col) == 1L) {
            col = rep(col, ncol(mesh$vb));
        } else {
            warning(sprintf("The mesh3d instance from parameter 'mesh' has %d vertices, but %d colors passed in parameter 'col'.\n", ncol(mesh$vb), length(col)));
        }
    }
    if(!is.null(hemi)) {
        if(!(hemi %in% c("lh", "rh"))) {
            stop("Parameter 'hemi' must be 'lh', 'rh', or NULL.");
        }
    }
    if(! is.logical(render)) {
        stop("Parameter 'render' must be of type logical.");      # nocov
    }

    if(is.null(metadata)) {
        metadata=list();        # nocov
    }

    md_entries = names(metadata);
    for (mde in md_entries) {
        if(! mde %in% c("src_data", "fs_mesh", "data_range", "makecmap_options", "map", "cmap_fun")) {
            warning(sprintf("Untypical metadata entry '%s' found in colormesh metadata.\n", mde));
        }
    }

    cm = list("mesh"=mesh, "col"=col, "render"=render, "hemi"=hemi, "metadata"=metadata);
    class(cm) = c("fs.coloredmesh", class(cm));
    return(cm);
}

