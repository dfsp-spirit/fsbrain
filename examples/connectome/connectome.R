#!/usr/bin/env Rscript
#
# connectome.R -- Visualize a brain connectome (a connectivity matrix drawn as
#                 nodes and edges) on the cortical surface.
#
# This is an example script that comes with 'fsbrain': https://github.com/dfsp-spirit/fsbrain
#
# The function 'vis.connectome' draws the edges of a connectivity matrix as lines
# between the centroids of the regions of an atlas, the nodes as spheres, and a
# semi-transparent cortical surface as context. This is the R equivalent of the
# 'plot_connectome' function of the Python package 'yabplot'.
#
# The connectivity matrix can come from anywhere: structural connectivity from
# tractography (e.g., streamlines between regions), functional connectivity (a
# correlation matrix), or any other square matrix that describes a relation
# between brain regions. This demo uses a synthetic matrix (edge weights decay
# with the distance between the regions, plus noise), unless you pass your own
# matrix with '--matrix'. A real matrix file is simply a CSV with a header row
# and row names, i.e., one row and one column per region, and the region names
# as row/column names, e.g.:
#
#     ,7Networks_LH_Default_pCunPCC_7,7Networks_LH_SomMot_36,...
#     7Networks_LH_Default_pCunPCC_7,0.0,0.31,...
#     7Networks_LH_SomMot_36,0.31,0.0,...
#     ...
#
# The region names must be regions of the atlas given with '--atlas' (the row
# names of the result of 'fsbrain::subject.region.centroids()'), and the nodes
# must be defined in the same space as the surface, which is the case for the
# fs_LR_32 template that this example uses.
#
# Dependencies:
#   Requires the 'fsbrain' package. The fs_LR_32 template meshes and atlases are
#   downloaded into the package cache on demand (they are not part of FreeSurfer
#   and not subject to the FreeSurfer license). For the headless PNG rendering
#   (the default), the 'scimesh' package is required as well.
#
# USAGE: ./connectome.R <output_dir> [--renderer <rgl|scimesh>] [--atlas <name>]
#                                 [--template <id>] [--matrix <file.csv>]
#                                 [--quantile <q>] [--nodes-by <none|strength>]
#
# OPTIONS:
#   --renderer <backend> : the renderer backend to use for image export, either 'rgl'
#                          (needs a working display) or 'scimesh' (headless software
#                          renderer that writes PNG images; requires the scimesh package).
#   --atlas <name>       : the atlas that defines the nodes, one of the atlases
#                          available for the fs_LR_32 template, e.g. 'schaefer400'
#                          (default), 'schaefer100', 'schaefer200', 'schaefer300',
#                          'schaefer1000', 'brainnetome' or 'aal3'.
#   --template <id>      : the template (or subject) that defines the region centroids
#                          and the context surface (default: 'fs_LR_32').
#   --matrix <file.csv>  : a CSV file with the connectivity matrix, see above. If
#                          omitted, a synthetic matrix is generated.
#   --quantile <q>       : the quantile of edge magnitudes to draw, i.e., only the
#                          strongest edges are rendered (default: 0.95, i.e., the
#                          strongest 5 percent of the edges).
#   --nodes-by <s>       : how to scale the node spheres, 'none' (all nodes have the
#                          same size, the default) or 'strength' (larger nodes for
#                          regions with stronger connections).
#
# Written by Tim Schaefer

library("fsbrain");

args = commandArgs(trailingOnly = TRUE);

# --- Parse command line arguments ---------------------------------------------

output_dir = ".";
renderer = NULL;
atlas = "schaefer400";
template_id = "fs_LR_32";
matrix_file = NULL;
edge_threshold_quantile = 0.95;
node_scale = "none";

arg_idx = 1L;
while(arg_idx <= length(args)) {
    arg = args[arg_idx];
    if(arg == "--renderer") {
        arg_idx = arg_idx + 1L;
        renderer = args[arg_idx];
    } else if(arg == "--atlas") {
        arg_idx = arg_idx + 1L;
        atlas = args[arg_idx];
    } else if(arg == "--template") {
        arg_idx = arg_idx + 1L;
        template_id = args[arg_idx];
    } else if(arg == "--matrix") {
        arg_idx = arg_idx + 1L;
        matrix_file = args[arg_idx];
    } else if(arg == "--quantile") {
        arg_idx = arg_idx + 1L;
        edge_threshold_quantile = as.numeric(args[arg_idx]);
    } else if(arg == "--nodes-by") {
        arg_idx = arg_idx + 1L;
        node_scale = args[arg_idx];
    } else {
        output_dir = arg;
    }
    arg_idx = arg_idx + 1L;
}

if(! is.null(renderer)) {
    options(fsbrain.renderer_backend = renderer);
}
cat(sprintf("Renderer backend: %s\n", fsbrain::get.fsbrain.renderer.backend()));

# --- Data: the template meshes and the atlas ----------------------------------

# The fs_LR_32 template is the HCP-style surface space with 32k vertices per hemisphere. The
# meshes and the atlases are downloaded into the package cache (this is a no-op if they are
# already there).
if(template_id == "fs_LR_32" && ! file.exists(file.path(fsbrain::get_optional_data_filepath("subjects_dir"), "fs_LR_32", "surf", "lh.midthickness"))) {
    cat("Downloading the fs_LR_32 template meshes and atlases.\n");
    fsbrain::download_fs_LR_32_meshes();
    fsbrain::download_fs_LR_32_atlases();
}

# The node positions are the centroids of the atlas regions, computed on the midthickness
# surface (the surface in the middle between white matter and pial surface).
centroids = fsbrain::subject.region.centroids(subjects_dir = NULL, subject_id = template_id, atlas = atlas, surface = "midthickness");
num_nodes = nrow(centroids);
cat(sprintf("Atlas '%s' on template '%s': %d regions (%d per hemisphere).\n", atlas, template_id, num_nodes, sum(centroids$hemi == "lh")));

# --- Data: the connectivity matrix --------------------------------------------

if(is.null(matrix_file)) {
    cat("Generating a synthetic connectivity matrix (edge weights decay with the distance between regions).\n");
    set.seed(42L);
    node_distances = as.matrix(stats::dist(centroids[, c("x", "y", "z")]));
    connectivity = exp(-node_distances / 40.0) + matrix(stats::runif(num_nodes * num_nodes, 0.0, 0.15), nrow = num_nodes);
    connectivity = (connectivity + t(connectivity)) / 2.0;   # symmetric
    diag(connectivity) = 0.0;
} else {
    cat(sprintf("Reading connectivity matrix from file '%s'.\n", matrix_file));
    connectivity = as.matrix(utils::read.csv(matrix_file, row.names = 1L, check.names = FALSE));
    if(nrow(connectivity) != num_nodes && ! is.null(rownames(connectivity))) {
        # The matrix defines its own node names: match them against the atlas regions.
        match_idx = match(rownames(connectivity), rownames(centroids));
        if(any(is.na(match_idx))) {
            stop(sprintf("The matrix has %d row(s), but atlas '%s' has %d region(s), and %d of the matrix row names are not regions of the atlas (e.g. '%s').\n",
                nrow(connectivity), atlas, num_nodes, sum(is.na(match_idx)), rownames(connectivity)[which(is.na(match_idx))[1L]]));
        }
        # vis.connectome() re-orders the centroids by name, so nothing else is needed here.
    }
}

cat(sprintf("Connectivity matrix: %d x %d, weights in range [%.3f, %.3f].\n", nrow(connectivity), ncol(connectivity), min(connectivity), max(connectivity)));

# --- Render the connectome ----------------------------------------------------

# 'views = NULL' only builds the renderables (the edges, the nodes and the context surface)
# and does not render anything. The returned list is a regular fsbrain renderable list, so it
# can be passed to 'export()' (which also handles the colorbar) or to any 'vis.*' function.
cm = fsbrain::vis.connectome(connectivity, atlas = atlas, template_id = template_id,
    context = list("surface" = "midthickness", "alpha" = 0.08, "color" = "#B0B0B0"),
    edge_threshold_quantile = edge_threshold_quantile, edge_scale = "weight",
    node_scale = node_scale, node_radius = 2.5,
    views = NULL);

# Figure 1: the standard 4 views (both hemispheres, lateral and medial), no colorbar.
img_views = file.path(output_dir, "connectome_views.png");
fsbrain::export(cm, view_angles = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"),
    draw_colorbar = FALSE, output_img = img_views, silent = TRUE);
cat(sprintf("Wrote '%s'.\n", img_views));

# Figure 2: two views, with a colorbar for the edge weights. Note that the colorbar shows the
# edges (they are the first renderable of the list returned by vis.connectome).
img_colorbar = file.path(output_dir, "connectome_edge_colorbar.png");
fsbrain::export(cm, view_angles = c("sd_lateral_lh", "sd_dorsal"),
    draw_colorbar = "horizontal", colorbar_legend = "Connection strength",
    output_img = img_colorbar, silent = TRUE);
cat(sprintf("Wrote '%s'.\n", img_colorbar));

# Figure 3: without the context surface. This is much faster and gives the classic 'spring
# layout' look of a connectome figure, with the camera framing the nodes.
cm_no_context = fsbrain::vis.connectome(connectivity, atlas = atlas, template_id = template_id,
    context = NULL, edge_threshold_quantile = edge_threshold_quantile, edge_scale = "weight",
    node_scale = node_scale, node_radius = 1.0, views = NULL);
img_no_context = file.path(output_dir, "connectome_no_context.png");
fsbrain::export(cm_no_context, view_angles = c("sd_lateral_lh", "sd_dorsal", "sd_caudal"),
    draw_colorbar = FALSE, output_img = img_no_context, silent = TRUE);
cat(sprintf("Wrote '%s'.\n", img_no_context));

cat("Done. See the function documentation of 'vis.connectome' and 'subject.region.centroids' for the details.\n");
