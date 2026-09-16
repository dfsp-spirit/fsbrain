# Connectome visualization: nodes and edges drawn inside (or without) a
# semi-transparent cortex.
#
# The edges are drawn as lines (see 'fs.coloredpaths'), the nodes as small
# spheres (see 'coloredmesh.from.spheres'), and the context is a regular brain
# surface rendered semi-transparently. All three are regular fsbrain
# renderables, so the result can be passed to export() and works with both
# renderer backends (rgl and scimesh).


#' @title Compute the centroid of every region of an atlas on a surface.
#'
#' @description Computes, for every region of an atlas (annotation), the mean position of the surface vertices that belong to the region. These centroids are the node positions of a connectome visualization, see \code{\link[fsbrain]{vis.connectome}}. This is the equivalent of the `_extract_centroids()` function of the Python package `yabplot`.
#'
#' @param subjects_dir character string. The FreeSurfer `SUBJECTS_DIR`, i.e., a directory containing the data for all subjects, each in a subdir named after the subject identifier. Can be `NULL` for a template subject (like `fs_LR_32` or `fsaverage`), in which case the standard locations (package cache, FreeSurfer installation) are searched.
#'
#' @param subject_id character string, the subject identifier. For a template space, this is the name of the template subject, e.g., 'fs_LR_32' or 'fsaverage'.
#'
#' @param atlas character string, the atlas name, e.g., 'schaefer400', 'brainnetome' or 'aal3'. Used to construct the name of the annotation file.
#'
#' @param surface character string, the name of the surface on which the centroids are computed. Defaults to 'midthickness' (the surface in the middle between white and pial matter), which is the surface usually used for connectome figures.
#'
#' @param hemi character string, one of 'lh', 'rh', or 'both'. Defaults to 'both'.
#'
#' @param exclude_regions vector of character strings, the names of atlas regions to exclude. Defaults to the medial wall and the unknown region, which are not real brain regions.
#'
#' @return data.frame with one row per region, the columns 'x', 'y', 'z' (the centroid coordinates), 'region' (the region name) and 'hemi' (the hemisphere). The row names are the region names, which are unique across hemispheres for the atlases that are distributed by fsbrain (see the example).
#'
#' @family connectome functions
#'
#' @examples
#' \dontrun{
#'   # For the fs_LR_32 template (HCP-style), download the data first:
#'   fsbrain::download_fs_LR_32_meshes();
#'   fsbrain::download_fs_LR_32_atlases();
#'   centroids = subject.region.centroids(subjects_dir = NULL, subject_id = "fs_LR_32",
#'     atlas = "schaefer400", surface = "midthickness");
#'   head(centroids);
#' }
#'
#' @export
subject.region.centroids <- function(subjects_dir, subject_id, atlas, surface = "midthickness", hemi = "both", exclude_regions = c("unknown", "medialwall", "")) {

    if(! hemi %in% c("lh", "rh", "both")) {
        stop("Parameter 'hemi' must be one of 'lh', 'rh', or 'both'.\n");
    }

    if(is.null(subjects_dir)) {
        subjects_dir = resolve.template.subjects.dir(subject_id, subjects_dir = NULL);
    } else if(! dir.exists(file.path(subjects_dir, subject_id))) {
        stop(sprintf("The subjects dir '%s' does not contain a subject named '%s'.\n", subjects_dir, subject_id));
    }

    hemi_list = if(hemi == "both") c("lh", "rh") else hemi;

    region_names = character(0L);
    coords = matrix(numeric(0), ncol = 3L);
    coord_hemi = character(0L);

    for(current_hemi in hemi_list) {
        annot = subject.annot(subjects_dir, subject_id, current_hemi, atlas);
        surface_mesh = subject.surface(subjects_dir, subject_id, surface = surface, hemi = current_hemi);

        if(length(annot$label_names) != nrow(surface_mesh$vertices)) {
            stop(sprintf("Annotation '%s' for hemi '%s' of subject '%s' has %d labels, but the surface '%s' has %d vertices. Are the atlas and the surface defined in the same space?\n", atlas, current_hemi, subject_id, length(annot$label_names), surface, nrow(surface_mesh$vertices)));
        }

        hemi_region_names = unique(annot$label_names);
        hemi_region_names = setdiff(hemi_region_names, exclude_regions);
        hemi_region_names = hemi_region_names[! is.na(hemi_region_names)];

        for(region_name in hemi_region_names) {
            vertex_indices = which(annot$label_names == region_name);
            if(length(vertex_indices) < 1L) {
                next;
            }
            centroid = colMeans(surface_mesh$vertices[vertex_indices, , drop = FALSE]);
            region_names = c(region_names, region_name);
            coords = rbind(coords, centroid);
            coord_hemi = c(coord_hemi, current_hemi);
        }
    }

    if(length(region_names) < 1L) {
        stop(sprintf("No regions found for atlas '%s' on subject '%s'. Check that the atlas is defined in the space of that subject.\n", atlas, subject_id));
    }

    centroids = data.frame("x"=coords[, 1], "y"=coords[, 2], "z"=coords[, 3], "region"=region_names, "hemi"=coord_hemi, stringsAsFactors = FALSE);
    rownames(centroids) = region_names;
    return(centroids);
}


#' @title Normalize a region name for matching.
#'
#' @description Removes all characters which are not letters or digits and lowercases the result, so that region names which differ only in their formatting (like 'LH_Vis_1' and 'lh vis 1') can be matched.
#'
#' @param x vector of character strings.
#'
#' @return vector of character strings, the normalized names.
#'
#' @keywords internal
normalize.region.names <- function(x) {
    return(tolower(gsub("[^a-zA-Z0-9]", "", x)));
}


#' @title Convert an edge list to a connectivity matrix.
#'
#' @param edge_list data.frame with the columns 'source', 'target' and 'weight'.
#'
#' @return named list with entries 'matrix' (the connectivity matrix) and 'names' (the node names).
#'
#' @keywords internal
connectivity.matrix.from.edge.list <- function(edge_list) {
    required_columns = c("source", "target", "weight");
    missing_columns = setdiff(required_columns, colnames(edge_list));
    if(length(missing_columns) > 0L) {
        stop(sprintf("When passing a data.frame as parameter 'connectivity_matrix', it must have the columns 'source', 'target' and 'weight'. Missing column(s): %s.\n", paste(missing_columns, collapse = ", ")));
    }

    node_names = unique(c(as.character(edge_list$source), as.character(edge_list$target)));
    num_nodes = length(node_names);
    mat = matrix(0.0, nrow = num_nodes, ncol = num_nodes, dimnames = list(node_names, node_names));

    row_idx = match(as.character(edge_list$source), node_names);
    col_idx = match(as.character(edge_list$target), node_names);
    mat[cbind(row_idx, col_idx)] = as.numeric(edge_list$weight);
    if(any(mat != t(mat))) {
        # The matrix is symmetric for the undirected case, and the upper triangle is used for the edges.
        mat = mat + t(mat);
    }

    return(list("matrix"=mat, "names"=node_names));
}


#' @title Map values to a color layer and the matching colorbar metadata.
#'
#' @param values numeric vector, the values to map to colors.
#'
#' @param makecmap_options named list of parameters to pass to \code{\link[squash]{makecmap}}, see \code{\link[fsbrain]{mkco.seq}}. Supported entries are 'colFn', 'n', 'symm' and 'col.na'.
#'
#' @return named list with entries 'colors' (the hex color strings for the values) and 'makecmap_options' (the input options, extended with the effective range, for use as colorbar metadata).
#'
#' @note The colors are computed from the color ramp that the colorbar uses for the same options, i.e., the values are binned into the interval boundaries of a ramp of 'n' colors over the value range, so the colorbar always matches the data. Values which are not finite (NA/NaN/Inf) get the color 'col.na'.
#'
#' @keywords internal
values.to.colorlayer <- function(values, makecmap_options) {
    if(length(values) < 1L) {
        return(list("colors"=character(0L), "makecmap_options"=makecmap_options));
    }

    num_col = if(is.null(makecmap_options$n)) 100L else max(1L, as.integer(makecmap_options$n));
    col_fn = if(is.null(makecmap_options$colFn)) cm.seq() else makecmap_options$colFn;
    col_na = if(is.null(makecmap_options$col.na)) "#FEFEFE" else makecmap_options$col.na;
    use_symmetric_range = isTRUE(makecmap_options$symm);

    ramp = col_fn(num_col);

    value_range = range(values, finite = TRUE);
    if(! all(is.finite(value_range)) || value_range[1L] == value_range[2L]) {
        # All values are identical (or none is finite): there is nothing to map, use the middle of the ramp.
        colors = rep(ramp[ceiling(num_col / 2.0)], length(values));
        colors[! is.finite(values)] = col_na;
        return(list("colors"=colors, "makecmap_options"=makecmap_options));
    }

    zlim = if(use_symmetric_range) symmrange(value_range) else value_range;

    bin_breaks = seq(zlim[1L], zlim[2L], length.out = num_col + 1L);
    bin_idx = findInterval(values, bin_breaks, all.inside = TRUE);
    colors = ramp[bin_idx];
    colors[! is.finite(values)] = col_na;

    # Store the effective range in the metadata, so that a colorbar which is drawn for this
    # renderable uses exactly the colors that were used for the data (this matters for diverging
    # colormaps, which may extend the range symmetrically).
    metadata_makecmap_options = makecmap_options;
    metadata_makecmap_options$range = zlim;
    metadata_makecmap_options$n = num_col;

    return(list("colors"=colors, "makecmap_options"=metadata_makecmap_options));
}


#' @title Visualize a connectivity matrix as a connectome on a brain surface.
#'
#' @description Draws the edges of a connectivity matrix as lines between region centroids, the nodes as spheres, and (optionally) a semi-transparent brain surface as context. This is the fsbrain equivalent of the `plot_connectome` function of the Python package `yabplot`. It can be used for any brain connectome, e.g., structural connectivity (tractography), functional connectivity (correlation), or any other square matrix that describes a relation between brain regions.
#'
#'   Note that this is a *visualization* function: it does not compute a connectivity matrix, and it does not check whether the data is a valid connectivity estimate.
#'
#' @param connectivity_matrix numeric square matrix, the connectivity matrix. The rows and columns are the nodes (brain regions). The names of the rows/columns (or the parameter `node_names`) define the nodes, see the parameter `node_coords`. Alternatively, an edge list as a data.frame with the columns 'source', 'target' and 'weight'.
#'
#' @param node_coords (n, 3) numeric matrix, the coordinates of the nodes (brain regions), in the same order as the matrix rows. If `NULL`, the nodes are looked up by name in the atlas given in parameter `atlas`, see the parameter `subjects_dir` and the function \code{\link[fsbrain]{subject.region.centroids}}. The row names of the matrix (or the parameter `node_names`) must then match the atlas region names.
#'
#' @param node_names vector of character strings, the node names. Only used if `node_coords` is given and the matrix has no row names. Defaults to NULL.
#'
#' @param subjects_dir character string or NULL. The FreeSurfer `SUBJECTS_DIR`, see \code{\link[fsbrain]{subject.region.centroids}}. Defaults to NULL, which searches the standard locations for the template subject `template_id`.
#'
#' @param template_id character string, the identifier of the template (or subject) whose atlas and surface define the nodes and the context surface. Defaults to 'fs_LR_32'. Ignored if `node_coords` is given *and* `context` is NULL.
#'
#' @param atlas character string or NULL, the atlas whose regions are the nodes, e.g., 'schaefer400'. Not required if `node_coords` is given.
#'
#' @param context named list or NULL, the semi-transparent brain surface drawn behind the connectome. The list can have the entries: 'surface' (character string, the surface to use, defaults to 'midthickness'), 'alpha' (numeric, the transparency of the surface, defaults to 0.08), and 'color' (character string, the color of the surface, defaults to '#B0B0B0'). Set to NULL to draw the connectome without any brain surface, which is much faster and gives a 'spring layout' style figure (the camera then frames the nodes).
#'
#' @param edge_threshold numeric or NULL, an absolute threshold on the edge weight. Edges with a lower magnitude are not drawn. Defaults to NULL (no absolute threshold).
#'
#' @param edge_threshold_quantile numeric or NULL, a quantile (in the range 0 to 1) of the edge magnitudes. Only edges with a magnitude above that quantile are drawn, i.e., the quantile selects the strongest edges. Defaults to 0.95, i.e., the strongest 5 percent of the edges are drawn. Set to NULL to use all edges.
#'
#' @param edge_absolute logical, whether the magnitude of the edge weights is used for thresholding and for the line widths. Defaults to TRUE, i.e., a strong negative edge is drawn like a strong positive one (but see the parameter `edge_negative`).
#'
#' @param edge_negative logical, whether to draw the negative edges instead of the positive ones. Defaults to FALSE, i.e., only positive edges are considered. To show both the positive and the negative edges of a signed matrix in one figure, call this function twice and combine the results (the second call can be rendered on top of the first).
#'
#' @param edge_width positive number, the line width in pixels. Defaults to 1.0 (a thin line, which is what hardware line rendering supports best).
#'
#' @param edge_width_range numeric vector of length 2 or NULL, the range of the line widths used when `edge_scale` is 'weight'. Defaults to c(0.5, 3.0).
#'
#' @param edge_scale character string, one of 'weight' (the line width encodes the edge weight) or 'none' (all edges have the width `edge_width`). Defaults to 'weight'.
#'
#' @param edge_color vector of hex color strings or NULL. A single color for all edges, or one color per edge. If NULL (the default), the edges are colored by their weight, using the colormap defined by `edge_makecmap_options`.
#'
#' @param edge_makecmap_options named list of parameters to pass to \code{\link[squash]{makecmap}}, see \code{\link[fsbrain]{mkco.div}}. Used to color the edges by weight and to draw the colorbar. Defaults to a diverging colormap which spans the weights of the drawn edges (note the 'symm' setting: only one sign of edges is drawn per call, so a symmetric range would waste half of the colormap).
#'
#' @param node_radius positive number, the radius of the node spheres, in the units of the surface coordinates (millimeters). Defaults to 2.0.
#'
#' @param node_radius_range numeric vector of length 2 or NULL, the range of the node radii used when `node_scale` is 'strength'. Defaults to NULL, which uses 0.5 and 1.5 times `node_radius`.
#'
#' @param node_scale character string, one of 'none' (all nodes have the radius `node_radius`) or 'strength' (the node radius encodes the node strength, i.e., the sum of the absolute weights of all its edges). Defaults to 'none'.
#'
#' @param node_color vector of hex color strings or NULL. A single color for all nodes, or one color per node. If NULL (the default), the nodes are colored by their strength (see `node_scale`), using the colormap defined by `node_makecmap_options`.
#'
#' @param node_makecmap_options named list of parameters to pass to \code{\link[squash]{makecmap}}, see \code{\link[fsbrain]{mkco.seq}}. Used to color the nodes by strength and to draw the colorbar. Defaults to `mkco.seq()`.
#'
#' @param views vector of character strings, the views to render, see \code{\link[fsbrain]{get.view.angle.names}}. Defaults to the 4 standard views.
#'
#' @param rgloptions option list passed to \code{\link[rgl]{par3d}}, see \code{\link[fsbrain]{rglo}}.
#'
#' @param rglactions named list. A list in which the names are from a set of pre-defined actions, see \code{\link[fsbrain]{rglactions}}.
#'
#' @param style a rendering style for the surface meshes, see \code{\link[fsbrain]{get.rglstyle}}. Note that the style of the context surface and of the nodes is not affected by this, only the style requested for the *edges* (which is of little relevance, as lines are drawn unlit by default).
#'
#' @param draw_colorbar logical or one of the character strings 'vertical' or 'horizontal', whether to draw a colorbar for the *edges*. Defaults to FALSE. Note that a colorbar is only drawn for the edges, not for the nodes: if you want to draw a colorbar for the node strengths, use \code{\link[fsbrain]{coloredmesh.plot.colorbar.separate}} on the nodes (the entry 'nodes' of the return value). Also note that the headless scimesh renderer backend does not support colorbars in this function, use \code{\link[fsbrain]{export}} instead (see the return value).
#'
#' @param silent logical, whether to suppress the status messages. Defaults to FALSE.
#'
#' @return named list of renderables, invisibly: the entries 'edges' (an `fs.coloredpaths` instance), 'nodes' (an `fs.coloredmesh` instance) and 'context_lh'/'context_rh' (the context surface, if requested). This list can be passed to \code{\link[fsbrain]{export}} to create a publication quality figure with a colorbar, which is also the recommended way to use the scimesh renderer backend.
#'
#' @family connectome functions
#'
#' @examples
#' \dontrun{
#'   # Use the fs_LR_32 template (see subject.region.centroids for the download commands).
#'   centroids = subject.region.centroids(subjects_dir = NULL, subject_id = "fs_LR_32",
#'     atlas = "schaefer400", surface = "midthickness");
#'   set.seed(42);
#'   mat = matrix(runif(nrow(centroids)^2), nrow = nrow(centroids),
#'     dimnames = list(rownames(centroids), rownames(centroids)));
#'   mat = (mat + t(mat)) / 2;
#'   diag(mat) = 0;
#'
#'   # Render the connectome in 3 views:
#'   cm = vis.connectome(mat, atlas = "schaefer400",
#'     views = c("sd_lateral_lh", "sd_dorsal", "sd_caudal"));
#'
#'   # Or create a publication quality image with a colorbar for the edges:
#'   export(cm, draw_colorbar = "horizontal",
#'     output_img = "connectome.png", colorbar_legend = "Edge weight");
#' }
#'
#' @export
vis.connectome <- function(connectivity_matrix, node_coords = NULL, node_names = NULL,
        subjects_dir = NULL, template_id = "fs_LR_32", atlas = NULL,
        context = list("surface" = "midthickness", "alpha" = 0.08, "color" = "#B0B0B0"),
        edge_threshold = NULL, edge_threshold_quantile = 0.95, edge_absolute = TRUE, edge_negative = FALSE,
        edge_width = 1.0, edge_width_range = c(0.5, 3.0), edge_scale = c("weight", "none"),
        edge_color = NULL, edge_makecmap_options = modifyList(mkco.div(), list("symm" = FALSE)),
        node_radius = 2.0, node_radius_range = NULL, node_scale = c("none", "strength"),
        node_color = NULL, node_makecmap_options = mkco.seq(),
        views = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"),
        rgloptions = rglo(), rglactions = list(), style = "default",
        draw_colorbar = FALSE, silent = FALSE) {

    if(is.data.frame(connectivity_matrix)) {
        edge_list_result = connectivity.matrix.from.edge.list(connectivity_matrix);
        connectivity_matrix = edge_list_result$matrix;
        if(is.null(node_names)) {
            node_names = edge_list_result$names;
        }
    }

    if(! is.matrix(connectivity_matrix) || ! is.numeric(connectivity_matrix) || nrow(connectivity_matrix) != ncol(connectivity_matrix)) {
        stop("Parameter 'connectivity_matrix' must be a square numeric matrix (or a data.frame edge list with the columns 'source', 'target' and 'weight').\n");
    }

    edge_scale = match.arg(edge_scale);
    node_scale = match.arg(node_scale);

    if(! (is.numeric(edge_width) && length(edge_width) == 1L && is.finite(edge_width) && edge_width > 0.0)) {
        stop("Parameter 'edge_width' must be a single positive number (the line width in pixels).\n");
    }
    if(! is.null(edge_width_range) && ! (is.numeric(edge_width_range) && length(edge_width_range) == 2L && all(is.finite(edge_width_range)) && edge_width_range[1L] > 0.0 && edge_width_range[1L] <= edge_width_range[2L])) {
        stop("Parameter 'edge_width_range' must be NULL, or a numeric vector of length 2 with increasing positive values.\n");
    }
    if(! (is.numeric(node_radius) && length(node_radius) == 1L && is.finite(node_radius) && node_radius > 0.0)) {
        stop("Parameter 'node_radius' must be a single positive number (the sphere radius in mm).\n");
    }
    if(! is.null(node_radius_range) && ! (is.numeric(node_radius_range) && length(node_radius_range) == 2L && all(is.finite(node_radius_range)) && node_radius_range[1L] > 0.0 && node_radius_range[1L] <= node_radius_range[2L])) {
        stop("Parameter 'node_radius_range' must be NULL, or a numeric vector of length 2 with increasing positive values.\n");
    }
    if(! is.null(edge_threshold_quantile) && ! (is.numeric(edge_threshold_quantile) && length(edge_threshold_quantile) == 1L && is.finite(edge_threshold_quantile) && edge_threshold_quantile >= 0.0 && edge_threshold_quantile <= 1.0)) {
        stop("Parameter 'edge_threshold_quantile' must be NULL, or a single number in the range 0 to 1.\n");
    }

    num_nodes = nrow(connectivity_matrix);

    # ── Node positions ────────────────────────────────────────────────────────
    if(is.null(node_coords)) {
        if(is.null(atlas)) {
            stop("You must pass either 'node_coords' (the coordinates of the nodes) or 'atlas' (the atlas whose region centroids are the nodes).\n");
        }
        if(! silent) {
            cat(sprintf("Computing region centroids for atlas '%s' on subject '%s'.\n", atlas, template_id));
        }
        context_surface = if(is.null(context)) "midthickness" else context$surface;
        centroids = subject.region.centroids(subjects_dir, template_id, atlas, surface = context_surface, hemi = "both");
        node_names = rownames(centroids);

        if(! is.null(dimnames(connectivity_matrix)[[1L]])) {
            matrix_names = dimnames(connectivity_matrix)[[1L]];
            match_idx = match(normalize.region.names(matrix_names), normalize.region.names(centroids$region));
            unmatched = matrix_names[is.na(match_idx)];
            if(length(unmatched) > 0L) {
                stop(sprintf("Found %d node name(s) in the connectivity matrix which are not regions of atlas '%s', e.g. '%s'. The region names of the atlas are needed (one can use the row names of the result of subject.region.centroids()).\n", length(unmatched), atlas, unmatched[1L]));
            }
            centroids = centroids[match_idx, , drop = FALSE];
        } else {
            if(num_nodes != nrow(centroids)) {
                stop(sprintf("The connectivity matrix has %d rows, but the atlas '%s' has %d regions. Either pass a matrix with %d rows, or set the row names of the matrix to the atlas region names.\n", num_nodes, atlas, nrow(centroids), nrow(centroids)));
            }
        }
        node_coords = as.matrix(centroids[, c("x", "y", "z")]);
        node_names = rownames(centroids);
    } else {
        node_coords = check.segment.points(node_coords, 'node_coords');
        if(nrow(node_coords) != num_nodes) {
            stop(sprintf("The connectivity matrix has %d rows, but 'node_coords' has %d rows. They must match.\n", num_nodes, nrow(node_coords)));
        }
        if(is.null(node_names)) {
            node_names = rownames(node_coords);
        }
    }

    if(is.null(node_names)) {
        node_names = dimnames(connectivity_matrix)[[1L]];
    }
    if(is.null(node_names)) {
        node_names = sprintf("node%03d", seq_len(num_nodes));
    }

    # ── Node strengths ────────────────────────────────────────────────────────
    node_strength = rowSums(abs(connectivity_matrix));

    # ── Edge selection ────────────────────────────────────────────────────────
    edge_idx = which(upper.tri(connectivity_matrix, diag = FALSE), arr.ind = TRUE);
    edge_weights = connectivity_matrix[edge_idx];
    edge_is_negative = edge_weights < 0.0;

    if(edge_negative) {
        keep = edge_is_negative;
    } else {
        keep = ! edge_is_negative;
    }
    edge_idx = edge_idx[keep, , drop = FALSE];
    edge_weights = edge_weights[keep];

    if(length(edge_weights) < 1L) {
        stop(sprintf("The connectivity matrix contains no %s edges, so there is nothing to draw. Set 'edge_negative' to %s to draw the %s edges.\n", ifelse(edge_negative, "negative", "positive"), as.character(! edge_negative), ifelse(edge_negative, "positive", "negative")));
    }

    edge_magnitude = if(edge_absolute) abs(edge_weights) else edge_weights;

    if(! is.null(edge_threshold_quantile)) {
        cutoff = stats::quantile(edge_magnitude, edge_threshold_quantile, names = FALSE);
        keep = edge_magnitude >= cutoff;
        edge_idx = edge_idx[keep, , drop = FALSE];
        edge_weights = edge_weights[keep];
        edge_magnitude = edge_magnitude[keep];
    }
    if(! is.null(edge_threshold)) {
        if(! (is.numeric(edge_threshold) && length(edge_threshold) == 1L && is.finite(edge_threshold) && edge_threshold >= 0.0)) {
            stop("Parameter 'edge_threshold' must be NULL, or a single non-negative number (an absolute threshold on the edge weight).\n");
        }
        keep = edge_magnitude >= edge_threshold;
        edge_idx = edge_idx[keep, , drop = FALSE];
        edge_weights = edge_weights[keep];
        edge_magnitude = edge_magnitude[keep];
    }

    if(length(edge_weights) < 1L) {
        stop("No edges are left after thresholding, so there is nothing to draw. Lower 'edge_threshold' or 'edge_threshold_quantile'.\n");
    }

    if(! silent) {
        cat(sprintf("Drawing %d of %d %s edges (quantile: %s, absolute threshold: %s).\n", length(edge_weights), num_nodes * (num_nodes - 1L) / 2L, ifelse(edge_negative, "negative", "positive"), ifelse(is.null(edge_threshold_quantile), "none", as.character(edge_threshold_quantile)), ifelse(is.null(edge_threshold), "none", as.character(edge_threshold))));
    }

    # ── Edge geometry ─────────────────────────────────────────────────────────
    edge_from = node_coords[edge_idx[, 1L], , drop = FALSE];
    edge_to = node_coords[edge_idx[, 2L], , drop = FALSE];

    if(edge_scale == "weight") {
        if(is.null(edge_width_range)) {
            edge_width_range = c(edge_width, edge_width);
        }
        edge_widths = values.to.range(edge_magnitude, edge_width_range);
    } else {
        edge_widths = rep(edge_width, length(edge_magnitude));
    }

    if(is.null(edge_color)) {
        edge_colorlayer = values.to.colorlayer(edge_weights, edge_makecmap_options);
        edge_colors = edge_colorlayer$colors;
        edge_metadata = list("src_data"=edge_weights, "data_range"=range(edge_weights, finite = TRUE), "makecmap_options"=edge_colorlayer$makecmap_options);
    } else {
        edge_colors = recycle(edge_color, length(edge_weights));
        edge_metadata = list("src_data"=edge_weights, "data_range"=range(edge_weights, finite = TRUE));
    }

    edges = fs.coloredpaths(edge_from, edge_to, col = edge_colors, width = edge_widths, metadata = edge_metadata);

    # ── Nodes ─────────────────────────────────────────────────────────────────
    if(node_scale == "strength") {
        if(is.null(node_radius_range)) {
            node_radius_range = c(0.5, 1.5) * node_radius;
        }
        node_radii = values.to.range(node_strength, node_radius_range);
    } else {
        node_radii = rep(node_radius, num_nodes);
    }

    if(is.null(node_color)) {
        node_colorlayer = values.to.colorlayer(node_strength, node_makecmap_options);
        node_colors = node_colorlayer$colors;
        node_metadata = list("src_data"=node_strength, "data_range"=range(node_strength, finite = TRUE), "makecmap_options"=node_colorlayer$makecmap_options);
    } else {
        node_colors = recycle(node_color, num_nodes);
        node_metadata = list("src_data"=node_strength, "data_range"=range(node_strength, finite = TRUE));
    }

    nodes = coloredmesh.from.spheres(node_coords, radii = node_radii, col = node_colors, metadata = node_metadata);

    # ── Context surface ───────────────────────────────────────────────────────
    renderables = list("edges"=edges, "nodes"=nodes);

    if(! is.null(context)) {
        unknown_context_entries = setdiff(names(context), c("surface", "alpha", "color"));
        if(length(unknown_context_entries) > 0L) {
            stop(sprintf("Unknown entry/entries in parameter 'context': %s. Supported entries are 'surface', 'alpha' and 'color'.\n", paste(unknown_context_entries, collapse = ", ")));
        }
        context_surface = if(is.null(context$surface)) "midthickness" else context$surface;
        context_alpha = if(is.null(context$alpha)) 0.08 else context$alpha;
        context_color = if(is.null(context$color)) "#B0B0B0" else context$color;

        if(! (is.numeric(context_alpha) && length(context_alpha) == 1L && is.finite(context_alpha) && context_alpha >= 0.0 && context_alpha <= 1.0)) {
            stop("The 'alpha' entry of parameter 'context' must be a single number in the range 0 to 1.\n");
        }

        surface_subjects_dir = subjects_dir;
        if(is.null(surface_subjects_dir)) {
            surface_subjects_dir = resolve.template.subjects.dir(template_id, subjects_dir = NULL);
        }

        # The context surface is rendered with a fixed style, so that the transparency requested
        # by the user is applied no matter which style is used for the other renderables.
        context_style = list("front"="filled", "back"="culled", "lit"=TRUE, "alpha"=context_alpha, "shininess"=50, "specular"="black");

        for(context_hemi in c("lh", "rh")) {
            renderables[[sprintf("context_%s", context_hemi)]] = coloredmesh.from.color(surface_subjects_dir, template_id, context_color, context_hemi, surface = context_surface, style = context_style);
        }
    }

    if(! silent) {
        cat(sprintf("Rendering %d view(s) with the %s renderer backend.\n", length(views), get.fsbrain.renderer.backend()));
    }

    brainviews(views, renderables, rgloptions = rgloptions, rglactions = rglactions, style = style, draw_colorbar = draw_colorbar);

    return(invisible(renderables));
}


#' @title Scale values into a given range.
#'
#' @description Linearly maps the values to the target range. Values which are all identical (or a single value) are mapped to the middle of the range.
#'
#' @param x numeric vector, the input values.
#'
#' @param target_range numeric vector of length 2, the target range.
#'
#' @return numeric vector of the same length as `x`, the scaled values.
#'
#' @keywords internal
values.to.range <- function(x, target_range) {
    value_range = range(x, finite = TRUE);
    if(! is.finite(value_range[1L]) || value_range[1L] == value_range[2L]) {
        return(rep(mean(target_range), length(x)));
    }
    fraction = (x - value_range[1L]) / (value_range[2L] - value_range[1L]);
    return(target_range[1L] + fraction * (target_range[2L] - target_range[1L]));
}
