# Visualization of thresholded volume data (clusters) as nested iso-surface shells, rendered inside
# one translucent anatomical mesh.
#
# A cluster is visualized as a set of nested iso-surfaces: the outermost shell is the iso-surface at
# the threshold (i.e., the cluster boundary), the inner ones sit at the higher iso-levels, up to the
# cluster peak. The shell colors encode the iso-level, the shells become more opaque towards the core,
# which makes the core visible through the outer shells (neither renderer supports per-vertex alpha,
# so the alpha value is a per-shell property).


#' @title Visualize clusters of a volume inside a translucent anatomical mesh.
#'
#' @description Visualize the clusters of a volume, e.g., the supra-threshold voxels of a statistical map, as 3D iso-surfaces inside a single semi-transparent anatomical mesh (the *context*, typically the cortex of the same subject). A cluster is rendered as a set of nested iso-surface shells: the outermost shell is the iso-surface at the threshold (the cluster boundary), the inner ones sit at the higher iso-levels, and the innermost shell is at the cluster peak. This way the shape of the cluster and the location of its peak are visible at the same time, and the voxel structure of the data does not show up as staircase artifacts.
#'
#'   The shells are colored with a diverging colormap that encodes the iso-level, i.e., they use exactly the colors that the colorbar shows, and they become more opaque towards the core of the cluster, so that the core is visible through the outer shells. Positive and negative clusters can be rendered together, they use the two halves of the diverging colormap.
#'
#'   To render the clusters without a context mesh, pass \code{context = NULL}. To show the location of the clusters within a specific subject, use the `context` parameter to select the surface (e.g., \code{"white"} or \code{"pial"}); the surfaces of the fsaverage template subject can be downloaded with \code{\link[fsbrain]{download_fsaverage}} if needed.
#'
#' @param subjects_dir string or `NULL`. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier. If `NULL` (the default), the subject directory is searched in the package cache, `SUBJECTS_DIR` and `FREESURFER_HOME`, see \code{\link[fsbrain]{find.subjectsdir.of}}. If the subject is the template subject 'fsaverage' and it cannot be found, it is downloaded into the package cache.
#'
#' @param subject_id string. The subject identifier, used to load the context mesh. Defaults to 'fsaverage', the template subject for which the surfaces can be downloaded automatically.
#'
#' @param volume a 3D numerical array (or an `fs.volume` instance) with the data, e.g., a statistical map. Values are interpreted as signed, i.e., the clusters are the regions in which the value is above `threshold` or below `-threshold`. See the note on the coordinate system.
#'
#' @param threshold single positive number or `NULL`. The cluster threshold, applied to the absolute voxel values. If `NULL` (the default), it is computed from the data as the `threshold_quantile` quantile of the absolute values of the non-zero voxels, and the value is reported unless `silent` is `TRUE`.
#'
#' @param threshold_quantile single value between 0 and 1, the quantile of the absolute non-zero voxel values used as the threshold if `threshold` is `NULL`. Defaults to 0.95.
#'
#' @param max_level single positive number or `NULL`, the highest iso-level, i.e., the level of the innermost shell of every cluster, in the units of the volume data. If `NULL` (the default), a separate value is computed for the positive and the negative clusters, namely the `max_level_quantile` quantile of the absolute values of the voxels beyond the threshold, so that a single extreme voxel does not dominate the visualization.
#'
#' @param max_level_quantile single value between 0 and 1, the quantile of the supra-threshold voxel values used to compute `max_level` if that is `NULL`. Defaults to 0.99.
#'
#' @param num_levels positive integer, the number of nested shells per cluster, i.e., per sign. The outermost shell is at the threshold, the innermost at `max_level`. Defaults to 10. Pass 1 to render a single opaque shell per sign, see the note on the visual style.
#'
#' @param positive logical, whether to render the clusters above `threshold`. Defaults to `TRUE`.
#'
#' @param negative logical, whether to render the clusters below `-threshold`. Defaults to `TRUE`. At least one of `positive` and `negative` must be `TRUE`.
#'
#' @param smoothing non-negative integer, the number of 3x3x3 box blur passes applied to the volume before the shells are extracted. This removes the staircase artifacts of the marching cubes algorithm and is what makes the clusters look like smooth blobs. Defaults to 2. Set to 0 to extract the shells from the raw volume.
#'
#' @param downsample positive integer, a factor by which the volume is subsampled before the shells are extracted. Values larger than 1 reduce the number of triangles (and thus the rendering time) considerably, at the cost of a less detailed surface, which can be useful because a cluster is rendered as `num_levels` meshes. The coordinates of the subsampled shells are corrected, so the result remains aligned with the original volume. Defaults to 1. Note that the volume is smoothed before subsampling if `smoothing` is larger than 0, otherwise the shells are prone to aliasing artifacts.
#'
#' @param frame positive integer, the frame (4th dimension) to use for a 4D volume. Defaults to 1.
#'
#' @param backend character string, the backend used to extract the iso-surfaces, one of 'auto' (the default: use the `Rvcg` package if it is installed, and fall back to `misc3d` otherwise), 'Rvcg' or 'misc3d'. `Rvcg` is recommended, it is faster, which matters here because one mesh per shell is extracted.
#'
#' @param context `NULL` or the definition of the context mesh to render the clusters in, typically a semi-transparent cortex of the same subject. Supported values are a character string (the surface name, e.g., 'white' or 'pial'), a named list of options for the context mesh (entries 'surface', 'color', 'alpha', 'style', 'subjects_dir' and 'subject_id', see \code{\link[fsbrain]{vis.subcortical.region.values}}), or a pre-built mesh (an `fs.coloredmesh` instance, or a hemilist of such instances).
#'
#' @param alpha_range numerical vector of length 2, the alpha value of the outermost shell (at the threshold) and of the innermost shell (at the cluster peak), each between 0 and 1. The values in between are interpolated, separately for the positive and the negative clusters. Defaults to `c(0.08, 1.0)`, i.e., the boundary of a cluster is much more transparent than its core. See the note on the visual style.
#'
#' @param makecmap_options named list of parameters to pass to \code{\link[squash]{makecmap}}. Must not include the unnamed first parameter, which is derived from the data. The entry 'range' can be used to fix the range of the colormap, otherwise a symmetric range which covers all rendered iso-levels is used. Defaults to \code{\link[fsbrain]{mkco.cluster}}.
#'
#' @param views list of character strings, the views to visualize. Available views are 'sd_<angle>' (a static view, see \code{\link[fsbrain]{vis.subject.morph.native}}), 't4'/'t9' (a 2x2 or 3x3 lightbox of views) and 'si'/'sr' (single interactive / rotating, requires the rgl renderer backend). Defaults to the 4 standard lateral and medial views. Pass `NULL` to skip the rendering entirely and only compute the meshes (which are also returned).
#'
#' @param rgloptions option list passed to \code{\link[rgl]{par3d}}. Defaults to the package default, see \code{\link[fsbrain]{rglo}}.
#'
#' @param rglactions named list, passed to the visualization functions, see \code{\link[fsbrain]{rglactions}}. The key 'no_vis' is useful to prepare the scene and add other data to it later. Note that saving the rendered scene to an image file is done with \code{\link[fsbrain]{export}}, which takes the return value of this function and supports merging several views into one image.
#'
#' @param silent logical, whether to suppress the console messages which report the threshold, the levels and the backend used. Defaults to `FALSE`.
#'
#' @return the list of renderable meshes, invisibly: the context meshes (if any) first, then the cluster shells, ordered from the outermost to the innermost shell of each sign. Passing this list to \code{\link[fsbrain]{export}} saves the scene as an image, including the colorbar of the cluster values.
#'
#' @section Coordinate system:
#'   The cluster shells are extracted in the voxel space of the volume and transformed to surface RAS with \code{\link[fsbrain]{index2ras_tkr}}, so they are aligned with surface renderings of the same subject. This requires the volume to be in the standard FreeSurfer orientation with 1 mm voxels (a *conformed* volume), which is the case for volumes read with \code{\link[fsbrain]{subject.volume}}; volumes in other spaces (like a 2 mm MNI template) have to be transformed to the subject space outside of fsbrain first.
#'
#' @section Visual style:
#'   A single iso-surface contains, by definition, the threshold value at all of its vertices, so a color gradient *within* one shell is not meaningful. The value information is therefore encoded in the *nesting*: each shell is drawn in the color of its own iso-level (the outer shell in the color of the threshold, the innermost one in the color of the peak), and the shells are semi-transparent, with the transparency decreasing towards the core (see `alpha_range`). This results in a soft glow which is brightest at the location of the maximum. Setting `num_levels = 1` instead renders one fully opaque shell per sign (the cluster boundary), which is the cleanest option if only the extent of the clusters matters. Note that the backfaces of the shells are drawn (`back = 'filled'`): with culled backfaces, the rgl renderer would composite the nested transparent shells in the wrong order, so the innermost shell would end up on top of the outer ones.
#'
#' @seealso \code{\link[fsbrain]{volvis.shells}} to visualize a whole volume (not just the clusters) as nested shells, \code{\link[fsbrain]{volvis.voxels}} for a voxel-based rendering, \code{\link[fsbrain]{vis.volume.on.surface}} to combine a volume with a morphometry-colored surface, and \code{\link[fsbrain]{vis.subcortical.region.values}} for the related atlas version with one value per region.
#'
#' @section Performance:
#'   A cluster is rendered as `num_levels` meshes, so the number of iso-surfaces that have to be extracted is `num_levels` times the number of signs (20 for the defaults). For a conformed volume of 256^3 voxels this takes about 30 seconds with the `Rvcg` backend, most of which is the marching cubes extraction. Setting `downsample = 2` reduces this to a few seconds (and the number of triangles by roughly a factor of 8), which is usually not visible for the smooth blobs that the default smoothing produces; it is a good choice for larger volumes, for small `num_levels` it matters less.
#'
#' @examples
#' \dontrun{
#'    fsbrain::download_optional_data();
#'    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
#'    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
#'
#'    # Use the brain volume of the demo subject as a fake statistical map: bright voxels are
#'    # 'positive', dark ones are 'negative'.
#'    brain = subject.volume(subjects_dir, 'subject1', 'brain');
#'    stat = (brain - 110) / 30;
#'
#'    # Render the clusters inside a semi-transparent cortex of the fsaverage template subject:
#'    cm = vis.volume.clusters(subjects_dir, 'fsaverage', stat, threshold = 1.5,
#'        rglactions = list('no_vis' = TRUE));
#'    export(cm, colorbar_legend = 'value', output_img = 'clusters.png');
#'
#'    # A single opaque shell per cluster, which only shows the cluster extent:
#'    cm_simple = vis.volume.clusters(subjects_dir, 'fsaverage', stat, threshold = 1.5,
#'        num_levels = 1, rglactions = list('no_vis' = TRUE));
#'
#'    # Without any context mesh, and with a fixed colormap range:
#'    cm_ctx = vis.volume.clusters(subjects_dir, 'fsaverage', stat, threshold = 1.5,
#'        context = NULL, makecmap_options = list('colFn' = squash::blueorange, 'n' = 100L,
#'            'symm' = TRUE, 'range' = c(-2.0, 2.0)), rglactions = list('no_vis' = TRUE));
#' }
#'
#' @family visualization functions
#' @family volume visualization
#'
#' @export
vis.volume.clusters <- function(subjects_dir = NULL, subject_id = "fsaverage", volume,
        threshold = NULL, threshold_quantile = 0.95, max_level = NULL, max_level_quantile = 0.99,
        num_levels = 10L, positive = TRUE, negative = TRUE, smoothing = 2L, downsample = 1L,
        frame = 1L, backend = "auto", context = list("surface" = "white", "alpha" = 0.09),
        alpha_range = c(0.08, 1.0), makecmap_options = mkco.cluster(),
        views = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"),
        rgloptions = rglo(), rglactions = list(), silent = FALSE) {

    volume = shell.volume.data(volume, frame = frame);
    backend = shell.backend(backend);

    if(! (is.numeric(num_levels) && length(num_levels) == 1L && num_levels >= 1L)) {
        stop("Parameter 'num_levels' must be a positive integer, the number of nested shells per cluster.\n");
    }
    if(! (is.logical(positive) && length(positive) == 1L && is.logical(negative) && length(negative) == 1L)) {
        stop("Parameters 'positive' and 'negative' must be single logical values.\n");
    }
    if(! (positive || negative)) {
        stop("At least one of the parameters 'positive' and 'negative' must be TRUE, otherwise there is nothing to visualize.\n");
    }
    if(! (is.numeric(smoothing) && length(smoothing) == 1L && smoothing >= 0L)) {
        stop("Parameter 'smoothing' must be a non-negative integer, the number of box blur passes applied to the volume before extracting the shells.\n");
    }
    if(! (is.numeric(downsample) && length(downsample) == 1L && downsample >= 1L)) {
        stop("Parameter 'downsample' must be a positive integer, the factor by which the volume is subsampled.\n");
    }
    if(! (is.numeric(alpha_range) && length(alpha_range) == 2L && all(is.finite(alpha_range)) && alpha_range[1L] >= 0.0 && alpha_range[1L] <= alpha_range[2L] && alpha_range[2L] <= 1.0)) {
        stop("Parameter 'alpha_range' must be a numerical vector of length 2 with ascending values between 0 and 1.\n");
    }
    if(downsample > 1L && smoothing == 0L) {
        warning(sprintf("The volume is subsampled (downsample = %d) without smoothing (smoothing = 0), the shells will show aliasing artifacts. Consider using smoothing = 1 or larger.\n", as.integer(downsample)));
    }

    threshold = volume.cluster.threshold(volume, threshold = threshold, threshold_quantile = threshold_quantile);
    if(! is.null(max_level) && is.numeric(max_level) && length(max_level) == 1L && is.finite(max_level) && max_level <= threshold) {
        stop(sprintf("Parameter 'max_level' must be larger than the threshold %g, otherwise there is no cluster core to visualize.\n", threshold));
    }

    # The volume is smoothed and/or subsampled before the shells are extracted. This reduces the
    # number of triangles a lot, and one cluster is rendered as several meshes, so it matters more
    # here than it does for a single iso-surface.
    work_volume = volume;
    if(smoothing > 0L) {
        work_volume = volume.boxblur(work_volume, passes = as.integer(smoothing));
    }
    if(downsample > 1L) {
        work_volume = volume.subsample(work_volume, factor = as.integer(downsample));
    }

    # The threshold is a property of the data as given by the user, so it is computed from the
    # original volume above. The iso-levels, in contrast, are computed from the volume that is
    # actually used for the extraction: smoothing lowers the peaks of the data, and a level that is
    # not in the range of that volume could not be extracted at all.
    levels = volume.cluster.levels(work_volume, threshold = threshold, num_levels = as.integer(num_levels),
        max_level = max_level, max_level_quantile = max_level_quantile, positive = positive, negative = negative);

    if(length(levels) == 0L) {
        stop(sprintf("No voxels beyond the threshold %g were found in the volume, there are no clusters to visualize. Lower the threshold, or pass it explicitly.\n", threshold));
    }

    if(! silent) {
        cat(sprintf("Visualizing %d cluster iso-level(s) as %d nested shell(s), using the '%s' backend.\n", length(levels), length(levels), backend));
        cat(sprintf(" * volume: %s voxels, threshold: %g, levels: %s\n", paste(dim(work_volume), collapse = "x"), threshold, paste(round(levels, 3), collapse = ", ")));
    }

    # One shared colormap for all shells of both signs, so that the color of a shell is exactly the
    # color that the colorbar shows for its iso-level. The range covers the peak of the clusters
    # (not only the rendered iso-levels), so that a shell at the threshold gets a color from the
    # middle of the colormap and the colorbar shows the range of the underlying data.
    extremes = volume.cluster.extremes(work_volume, threshold = threshold, max_level = max_level,
        max_level_quantile = max_level_quantile, positive = positive, negative = negative);
    colmap = volume.clusters.colormap(makecmap_options, levels, max_abs = max(c(threshold, extremes)));

    # The mesh vertices are 1-based R array indices, see index2ras_tkr().
    mesh_transform = index2ras_tkr() %*% volume.subsample.matrix(downsample);

    cluster_meshes = list();
    for(level_idx in seq_along(levels)) {
        level = levels[level_idx];
        mesh = shell.extract.mesh(work_volume, level = level, backend = backend);
        if(is.null(mesh)) {
            # Can happen for the innermost shells: smoothing lowers the peak of the data, so the
            # highest level may be out of range. Skipping it is better than stopping.
            if(! silent) {
                cat(sprintf(" * skipping iso-level %g, it is not within the range of the (smoothed) volume data.\n", level));
            }
            next;
        }
        mesh = apply.transform(mesh, mesh_transform);
        cmesh = fs.coloredmesh(mesh, colmap$colors[level_idx], hemi = NULL, render = TRUE, add_normals = TRUE,
            metadata = list("src_data" = level, "map" = colmap$map, "makecmap_options" = colmap$options,
                            "data_range" = range(levels, finite = TRUE)));
        # Backfaces are drawn on purpose: with culled backfaces, the rgl renderer composites the
        # nested transparent shells in the wrong order (the innermost shell on top of the outer ones).
        cmesh$style = list("alpha" = cluster.shell.alpha(level, threshold = threshold, levels = levels, alpha_range = alpha_range),
                           "lit" = TRUE, "front" = "filled", "back" = "filled",
                           "shininess" = 40, "specular" = "black");
        cluster_meshes[[length(cluster_meshes) + 1L]] = cmesh;
    }

    if(length(cluster_meshes) == 0L) {
        stop(sprintf("No cluster iso-surface could be extracted for threshold %g. Lower the threshold, lower 'max_level', or use less smoothing.\n", threshold));
    }

    # The optional context mesh (typically a semi-transparent cortex). It is rendered first, so that
    # the cluster shells are drawn on top of it, and it carries no colormap metadata, so that it does
    # not contribute to the colorbar of the cluster values.
    context_meshes = NULL;
    if(! is.null(context)) {
        if(is.null(subjects_dir)) {
            resolved_subjects_dir = context.mesh.resolve.subjects.dir(subject_id = subject_id, context = context);
            if(! is.null(resolved_subjects_dir)) {
                subjects_dir = resolved_subjects_dir;
            }
        }
        context_meshes = mesh.atlas.context.layer(subjects_dir, subject_id, context);
    }

    renderable = c();
    if(! is.null(context_meshes)) {
        renderable = c(renderable, unname(context_meshes));
    }
    renderable = c(renderable, cluster_meshes);

    if(! rglactions.has.key(rglactions, 'no_vis')) {
        # The style of each mesh is used, it carries the alpha value of the shell.
        brainviews(views, renderable, rgloptions = rgloptions, rglactions = rglactions, style = "from_mesh");
    }

    return(invisible(renderable));
}


#' @title Compute the cluster threshold of a volume.
#'
#' @description Computes the threshold from the data if none was given, namely the requested quantile of the absolute values of the non-zero voxels.
#'
#' @param volume a 3D numerical array.
#'
#' @param threshold single positive number or `NULL`, the threshold given by the user.
#'
#' @param threshold_quantile single value between 0 and 1, the quantile used if `threshold` is `NULL`.
#'
#' @return single positive number, the threshold.
#'
#' @keywords internal
volume.cluster.threshold <- function(volume, threshold = NULL, threshold_quantile = 0.95) {
    if(! is.null(threshold)) {
        if(! (is.numeric(threshold) && length(threshold) == 1L && is.finite(threshold) && threshold > 0.0)) {
            stop("Parameter 'threshold' must be a single positive number (the threshold on the absolute voxel values), or NULL to compute it from the data.\n");
        }
        return(as.numeric(threshold));
    }
    if(! (is.numeric(threshold_quantile) && length(threshold_quantile) == 1L && is.finite(threshold_quantile) && threshold_quantile > 0.0 && threshold_quantile < 1.0)) {
        stop("Parameter 'threshold_quantile' must be a single value between 0 and 1.\n");
    }
    foreground = abs(volume[is.finite(volume) & volume != 0.0]);
    if(length(foreground) < 1L) {
        stop("The volume contains no non-zero voxels, so no threshold can be computed from the data. Pass 'threshold' explicitly.\n");
    }
    return(as.numeric(stats::quantile(foreground, probs = threshold_quantile, names = FALSE)));
}


#' @title Compute the highest iso-level per sign.
#'
#' @description Computes the highest absolute iso-level for the positive and the negative clusters, i.e., the level of the innermost shell of a cluster. If `max_level` is `NULL`, the `max_level_quantile` quantile of the absolute values beyond the threshold is used, so that a single extreme voxel does not dominate the visualization.
#'
#' @param volume a 3D numerical array.
#'
#' @param threshold single positive number, the cluster threshold.
#'
#' @param max_level single positive number or `NULL`, the highest level given by the user.
#'
#' @param max_level_quantile single value between 0 and 1, the quantile used to compute the highest level if `max_level` is `NULL`.
#'
#' @param positive logical, whether to consider the positive clusters.
#'
#' @param negative logical, whether to consider the negative clusters.
#'
#' @return named numerical vector with the entries 'neg' and 'pos' (the highest absolute iso-level of the negative and of the positive clusters), or `NA` for a sign that has no voxels beyond the threshold (or was not requested).
#'
#' @keywords internal
volume.cluster.extremes <- function(volume, threshold, max_level = NULL, max_level_quantile = 0.99, positive = TRUE, negative = TRUE) {
    side.extreme = function(values) {
        finite_values = abs(values[is.finite(values)]);
        if(length(finite_values) < 1L) {
            return(NA_real_);
        }
        if(! is.null(max_level)) {
            return(as.numeric(max_level));
        }
        if(length(finite_values) < 2L) {
            return(as.numeric(finite_values[1L]));        # a single voxel, no quantile
        }
        return(as.numeric(stats::quantile(finite_values, probs = max_level_quantile, names = FALSE)));
    };
    return(c("neg" = if(negative) side.extreme(volume[volume < -threshold]) else NA_real_,
             "pos" = if(positive) side.extreme(volume[volume > threshold]) else NA_real_));
}


#' @title Compute the iso-levels of the cluster shells.
#'
#' @description Computes the iso-levels at which the shells of the clusters are extracted: `num_levels` levels from the threshold up to the highest level, for each requested sign, ordered from the outermost (the threshold) to the innermost (the highest level) shell.
#'
#' @param volume a 3D numerical array.
#'
#' @param threshold single positive number, the cluster threshold.
#'
#' @param num_levels positive integer, the number of levels per sign.
#'
#' @param max_level single positive number or `NULL`, the highest level. If `NULL`, it is computed per sign as the `max_level_quantile` quantile of the absolute values beyond the threshold.
#'
#' @param max_level_quantile single value between 0 and 1, the quantile used to compute `max_level` if that is `NULL`.
#'
#' @param positive logical, whether to compute levels for the positive side.
#'
#' @param negative logical, whether to compute levels for the negative side.
#'
#' @return numerical vector of iso-levels, the negative ones (if any) first, each ordered from the outermost to the innermost shell.
#'
#' @keywords internal
volume.cluster.levels <- function(volume, threshold, num_levels = 10L, max_level = NULL,
        max_level_quantile = 0.99, positive = TRUE, negative = TRUE) {

    if(! is.null(max_level) && ! (is.numeric(max_level) && length(max_level) == 1L && is.finite(max_level) && max_level > 0.0)) {
        stop("Parameter 'max_level' must be a single positive number (the highest iso-level), or NULL to compute it from the data.\n");
    }
    if(is.null(max_level) && ! (is.numeric(max_level_quantile) && length(max_level_quantile) == 1L && is.finite(max_level_quantile) && max_level_quantile > 0.0 && max_level_quantile < 1.0)) {
        stop("Parameter 'max_level_quantile' must be a single value between 0 and 1.\n");
    }

    extremes = volume.cluster.extremes(volume, threshold = threshold, max_level = max_level,
        max_level_quantile = max_level_quantile, positive = positive, negative = negative);

    # The levels have to be within the range of the volume, otherwise no iso-surface can be extracted.
    volume_range = range(volume, finite = TRUE);

    side.levels = function(side_extreme, side_limit) {
        if(is.na(side_extreme)) {
            return(NULL);
        }
        side_extreme = min(max(side_extreme, threshold), side_limit);
        return(unique(seq(threshold, side_extreme, length.out = as.integer(num_levels))));
    };

    levels = numeric(0);
    neg_levels = side.levels(extremes["neg"], abs(volume_range[1L]));
    if(! is.null(neg_levels)) {
        levels = c(levels, -neg_levels);
    }
    pos_levels = side.levels(extremes["pos"], volume_range[2L]);
    if(! is.null(pos_levels)) {
        levels = c(levels, pos_levels);
    }
    return(levels);
}


#' @title Compute the alpha value of a cluster shell.
#'
#' @description The shells of a cluster are more transparent the closer they are to the threshold, and fully opaque at the highest level of their sign.
#'
#' @param level single number, the iso-level of the shell.
#'
#' @param threshold single positive number, the cluster threshold.
#'
#' @param levels numerical vector of all iso-levels, see \code{\link[fsbrain]{volume.cluster.levels}}.
#'
#' @param alpha_range numerical vector of length 2, the alpha value of the outermost and the innermost shell.
#'
#' @return single number between 0 and 1, the alpha value of the shell.
#'
#' @keywords internal
cluster.shell.alpha <- function(level, threshold, levels, alpha_range = c(0.08, 1.0)) {
    side_levels = levels[sign(levels) == sign(level)];
    side_max = max(abs(side_levels));
    if(side_max <= threshold) {
        # Only a single shell on this side, it represents the cluster boundary, so it is opaque.
        return(alpha_range[2L]);
    }
    relative_level = (abs(level) - threshold) / (side_max - threshold);
    return(alpha_range[1L] + relative_level * (alpha_range[2L] - alpha_range[1L]));
}


#' @title Compute the shared colormap of the cluster shells.
#'
#' @description All shells of both signs share one colormap, so that the color of a shell is exactly the color that the colorbar shows for its iso-level. A symmetric range which covers all iso-levels is used unless the user requested a range via the 'range' entry of `makecmap_options`.
#'
#' @param makecmap_options named list, the colormap options, see \code{\link[squash]{makecmap}}.
#'
#' @param levels numerical vector of iso-levels.
#'
#' @param max_abs single positive number, the absolute value of the most extreme value that the colormap has to cover, i.e., half of the range of the colormap. Defaults to the highest absolute iso-level.
#'
#' @return named list with the entries 'map' (the `squash` colormap), 'colors' (a color per entry of `levels`), 'range' (the range of the colormap) and 'options' (`makecmap_options`, including the range, for the colorbar metadata of the meshes).
#'
#' @keywords internal
#' @importFrom squash makecmap cmap
volume.clusters.colormap <- function(makecmap_options, levels, max_abs = max(abs(levels))) {
    if(hasIn(makecmap_options, 'range')) {
        data_range = makecmap_options$range;
        if(! (is.numeric(data_range) && length(data_range) == 2L && all(is.finite(data_range)) && data_range[1L] < data_range[2L])) {
            stop("The 'range' entry of makecmap_options must be a numerical vector of length 2 with ascending values.\n");
        }
    } else {
        data_range = c(-max_abs, max_abs);
        makecmap_options$range = data_range;
    }

    # 'squash::makecmap' derives the map from the data it is given, and it requires at least as many
    # data values as requested colors, so a dense sequence over the requested range is used here.
    num_map_values = max(256L, if(is.null(makecmap_options$n)) 0L else as.integer(makecmap_options$n));
    map_options = makecmap_options;
    map_options$range = NULL;               # an fsbrain option, not a makecmap argument
    map = do.call(squash::makecmap, utils::modifyList(list(seq(data_range[1L], data_range[2L], length.out = num_map_values)), map_options));

    # Keep the colors within the range: squash::cmap errors for values outside of it.
    levels_in_range = pmin(pmax(levels, data_range[1L]), data_range[2L]);
    return(list("map" = map, "colors" = as.character(squash::cmap(levels_in_range, map = map)),
                "range" = data_range, "options" = makecmap_options));
}


#' @title Resolve the subjects dir for a context mesh.
#'
#' @description Determine the subjects dir from which the requested context mesh can be loaded, if the user did not pass one. For the template subject 'fsaverage' the surfaces are downloaded into the package cache if they are not available, for other subjects the standard search locations are checked, see \code{\link[fsbrain]{find.subjectsdir.of}}.
#'
#' @param subject_id string, the subject identifier.
#'
#' @param context the context mesh definition, as passed by the user, see \code{\link[fsbrain]{vis.volume.clusters}}. A subject id and subjects dir can be part of this definition, in which case they take precedence.
#'
#' @return string, the path to the subjects dir to use, or `NULL` if the context mesh does not have to be loaded from disk (e.g., because it was passed as a pre-built mesh).
#'
#' @keywords internal
context.mesh.resolve.subjects.dir <- function(subject_id = "fsaverage", context = NULL) {
    if(is.fs.coloredmesh(context)) {
        return(NULL);               # a pre-built mesh, no subjects dir needed
    }
    if(is.list(context) && ! freesurferformats::is.fs.volume(context)) {
        ctx_subjects_dir = getIn(context, "subjects_dir", default = NULL);
        if(! is.null(ctx_subjects_dir)) {
            return(ctx_subjects_dir);
        }
        if(is.fs.coloredmesh(getIn(context, "lh", default = NULL))) {
            return(NULL);           # a hemilist of pre-built meshes, no subjects dir needed
        }
        subject_id = getIn(context, "subject_id", default = subject_id);
    }
    if(identical(subject_id, "fsaverage")) {
        return(fsaverage.path(allow_fetch = TRUE));
    }
    return(find.subjectsdir.of(subject_id = subject_id, mustWork = TRUE));
}
