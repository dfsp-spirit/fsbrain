# Tests for the connectome visualization functions (R/connectome.R):
# subject.region.centroids() and vis.connectome().
#
# Most tests use a tiny synthetic subject, so they are offline and do not
# require any downloaded data: the 'atlas' has 2 regions per hemisphere (4
# nodes in total: lhA, lhB, rhA, rhB), defined on a cube surface which is also
# available as the 'midthickness' context surface.

# Helper: create a minimal subjects dir with a synthetic 4-region atlas.
create.synthetic.connectome.subject <- function() {
    subjects_dir = tempfile("fsbrain_connectome_subjects_dir_");
    subject_id = "testsubject";
    subject_dir = file.path(subjects_dir, subject_id);
    dir.create(file.path(subject_dir, "surf"), recursive = TRUE);
    dir.create(file.path(subject_dir, "label"), recursive = TRUE);

    cube = freesurferformats::read.fs.surface(system.file("extdata", "cube.ply", package = "fsbrain", mustWork = TRUE));
    cube$vertices = cube$vertices * 50.0;   # scale the unit cube to a brain-like size
    num_vertices = nrow(cube$vertices);

    # Two regions per hemisphere, 4 vertices each (the cube has 8 vertices).
    region_indices = integer(num_vertices);
    region_indices[1:4] = 2L;               # first region
    region_indices[5:8] = 3L;               # second region

    for(hemi in c("lh", "rh")) {
        region_names = if(hemi == "lh") c("unknown", "lhA", "lhB") else c("unknown", "rhA", "rhB");
        colortable = data.frame(
            "struct_name" = region_names,
            "r" = c(0L, 255L, 0L), "g" = c(0L, 0L, 0L), "b" = c(0L, 0L, 255L), "a" = 0L,
            "struct_index" = seq.int(0L, 2L), stringsAsFactors = FALSE);

        freesurferformats::write.fs.surface(file.path(subject_dir, "surf", sprintf("%s.white", hemi)), cube$vertices, cube$faces);
        freesurferformats::write.fs.surface(file.path(subject_dir, "surf", sprintf("%s.midthickness", hemi)), cube$vertices, cube$faces);
        freesurferformats::write.fs.annot(file.path(subject_dir, "label", sprintf("%s.testatlas.annot", hemi)),
            num_vertices = num_vertices, colortable = colortable,
            labels_as_indices_into_colortable = region_indices);
    }

    return(list("subjects_dir" = subjects_dir, "subject_id" = subject_id));
}


# Helper: a small symmetric connectivity matrix for the 4 synthetic nodes.
get.demo.connectivity.matrix <- function(node_names = c("lhA", "lhB", "rhA", "rhB"), values = NULL) {
    num_nodes = length(node_names);
    if(is.null(values)) {
        values = seq(0.05, 0.95, length.out = num_nodes * num_nodes);
    }
    mat = matrix(values, nrow = num_nodes, ncol = num_nodes, dimnames = list(node_names, node_names));
    mat = (mat + t(mat)) / 2.0;
    diag(mat) = 0.0;
    return(mat);
}


# Helper: recover the per-node radii from a sphere cloud mesh (one sphere per node, subdiv 2).
coloredmesh.node.radii <- function(cmesh, subdivisions = 2L) {
    num_verts_per_sphere = 162L;   # 10 * 4^2 + 2
    num_nodes = ncol(cmesh$mesh$vb) / num_verts_per_sphere;
    vertices = t(cmesh$mesh$vb[1:3, ]);
    radii = numeric(num_nodes);
    for(node_idx in seq_len(num_nodes)) {
        idx = seq.int((node_idx - 1L) * num_verts_per_sphere + 1L, node_idx * num_verts_per_sphere);
        sphere_center = colMeans(vertices[idx, ]);
        radii[node_idx] = mean(sqrt(rowSums(sweep(vertices[idx, ], 2L, sphere_center)^2)));
    }
    return(radii);
}


test_that("subject.region.centroids computes one centroid per atlas region", {
    sj = create.synthetic.connectome.subject();
    centroids = subject.region.centroids(sj$subjects_dir, sj$subject_id, "testatlas", surface = "midthickness");

    expect_equal(nrow(centroids), 4L);
    expect_true(all(c("x", "y", "z", "region", "hemi") %in% colnames(centroids)));
    expect_equal(sort(rownames(centroids)), c("lhA", "lhB", "rhA", "rhB"));
    expect_false(any(grepl("unknown", rownames(centroids))));   # the excluded region

    # The centroid of a region is the mean of the coordinates of its vertices.
    cube = freesurferformats::read.fs.surface(system.file("extdata", "cube.ply", package = "fsbrain", mustWork = TRUE));
    cube$vertices = cube$vertices * 50.0;
    expect_equal(as.numeric(centroids["lhA", c("x", "y", "z")]), colMeans(cube$vertices[1:4, ]), tolerance = 1e-10);
    expect_equal(as.numeric(centroids["rhB", c("x", "y", "z")]), colMeans(cube$vertices[5:8, ]), tolerance = 1e-10);

    # The hemisphere is reported.
    expect_equal(centroids["lhA", "hemi"], "lh");
    expect_equal(centroids["rhA", "hemi"], "rh");

    # A single hemisphere can be requested.
    centroids_lh = subject.region.centroids(sj$subjects_dir, sj$subject_id, "testatlas", surface = "midthickness", hemi = "lh");
    expect_equal(sort(rownames(centroids_lh)), c("lhA", "lhB"));
});


test_that("subject.region.centroids validates its input", {
    sj = create.synthetic.connectome.subject();
    expect_error(subject.region.centroids(sj$subjects_dir, sj$subject_id, "testatlas", hemi = "up"));
    expect_error(subject.region.centroids(sj$subjects_dir, "nosuchsubject", "testatlas"));
});


test_that("normalize.region.names makes region names comparable", {
    expect_equal(normalize.region.names(c("LH_Vis_1", "lh vis 1", "Lh-Vis-1")), rep("lhvis1", 3L));
    expect_equal(normalize.region.names("7Networks_LH_Default_pCunPCC_7"), "7networkslhdefaultpcunpcc7");
});


test_that("connectivity.matrix.from.edge.list builds a symmetric matrix", {
    edge_list = data.frame("source" = c("A", "B"), "target" = c("B", "C"), "weight" = c(0.5, 0.25));
    res = connectivity.matrix.from.edge.list(edge_list);
    expect_equal(res$names, c("A", "B", "C"));
    expect_equal(dim(res$matrix), c(3L, 3L));
    expect_equal(res$matrix["A", "B"], 0.5);
    expect_equal(res$matrix["B", "A"], 0.5);
    expect_equal(res$matrix["B", "C"], 0.25);
    expect_equal(res$matrix["A", "C"], 0.0);

    expect_error(connectivity.matrix.from.edge.list(data.frame("from" = "A", "to" = "B", "w" = 1.0)));
});


test_that("values.to.colorlayer maps values to colors and returns colorbar metadata", {
    values = c(0.1, 0.5, 0.9);
    res = values.to.colorlayer(values, mkco.seq());
    expect_equal(length(res$colors), 3L);
    expect_true(all(grepl("^#", res$colors)));
    expect_equal(res$makecmap_options$range, range(values), tolerance = 1e-6);

    # The colors are ordered: the smallest value gets the first ramp color, the largest the last.
    ramp = cm.seq()(100L);
    expect_equal(res$colors[1L], ramp[1L]);
    expect_equal(res$colors[3L], ramp[100L]);

    # The number of colors of the ramp is stored, so that a colorbar uses the same ramp.
    expect_equal(res$makecmap_options$n, 100L);

    # A mirrored (diverging) range is honored.
    res_symm = values.to.colorlayer(values, mkco.div());
    expect_equal(res_symm$makecmap_options$range, c(-0.9, 0.9), tolerance = 1e-6);

    # All values identical: no crash, all colors equal.
    res2 = values.to.colorlayer(rep(1.0, 3L), mkco.seq());
    expect_equal(length(unique(res2$colors)), 1L);

    # Non-finite values get the 'col.na' color.
    res3 = values.to.colorlayer(c(0.1, NA_real_, 0.9), mkco.seq());
    expect_equal(res3$colors[2L], "#FEFEFE");

    # Empty input.
    res4 = values.to.colorlayer(numeric(0L), mkco.seq());
    expect_equal(length(res4$colors), 0L);
});


test_that("values.to.range maps values into the target range", {
    expect_equal(values.to.range(c(1, 2, 3), c(0, 1)), c(0, 0.5, 1));
    expect_equal(values.to.range(c(5, 5), c(1, 3)), c(2, 2));
    expect_equal(values.to.range(c(10, 20), c(2, 2)), c(2, 2));
});


test_that("vis.connectome builds edges, nodes and context from an atlas and a matrix", {
    skip_if_rgl_required();
    sj = create.synthetic.connectome.subject();
    mat = get.demo.connectivity.matrix();

    cm = vis.connectome(mat, subjects_dir = sj$subjects_dir, template_id = sj$subject_id,
        atlas = "testatlas", views = NULL, edge_threshold_quantile = NULL, silent = TRUE);

    expect_equal(names(cm), c("edges", "nodes", "context_lh", "context_rh"));
    expect_true(is.fs.coloredpaths(cm$edges));
    expect_true(is.fs.coloredmesh(cm$nodes));
    expect_true(is.fs.coloredmesh(cm$context_lh));
    expect_equal(cm$context_lh$hemi, "lh");

    # All 6 edges of the 4-node graph are drawn, and the node positions are the centroids.
    expect_equal(nrow(cm$edges$from), 6L);
    centroids = subject.region.centroids(sj$subjects_dir, sj$subject_id, "testatlas");
    expect_equal(ncol(cm$nodes$mesh$vb), 4L * 162L);   # one sphere (subdiv 2) per node

    # The renderables can be transformed and are recognized as renderable.
    expect_true(all(vapply(cm, fsbrain.renderable, logical(1L))));
});


test_that("vis.connectome maps edge weights to widths and colors", {
    skip_if_rgl_required();
    sj = create.synthetic.connectome.subject();
    mat = get.demo.connectivity.matrix();

    cm = vis.connectome(mat, subjects_dir = sj$subjects_dir, template_id = sj$subject_id,
        atlas = "testatlas", views = NULL, edge_threshold_quantile = NULL,
        edge_scale = "weight", edge_width_range = c(1.0, 5.0),
        node_scale = "strength", node_radius_range = c(2.0, 8.0), silent = TRUE);

    # Edge widths span the requested range.
    expect_equal(range(cm$edges$width), c(1.0, 5.0), tolerance = 1e-10);

    # The width is proportional to the edge weight (strongest edge -> widest line).
    strongest = which.max(abs(mat[upper.tri(mat)]));
    expect_equal(cm$edges$width[strongest], 5.0, tolerance = 1e-10);

    # Colors are mapped from a colormap, and the metadata describes it (for the colorbar).
    expect_equal(length(unique(cm$edges$col)) > 1L, TRUE);
    expect_false(is.null(cm$edges$metadata$makecmap_options));
    expect_equal(cm$edges$metadata$src_data, mat[upper.tri(mat)]);
    expect_equal(cm$edges$metadata$data_range, range(mat[upper.tri(mat)]), tolerance = 1e-10);

    # The colorbar of the scene is the one of the edges (they are the first renderable).
    expect_true(can.plot.colorbar.from.coloredmeshes(cm));
    expect_equal(coloredmeshes.get.md(cm, 'src_data'), cm$edges$metadata$src_data);

    # Nodes are scaled by strength: the node with the largest row sum gets the largest radius.
    node_strength = rowSums(abs(mat));
    node_radii = coloredmesh.node.radii(cm$nodes);
    expect_equal(length(unique(round(node_radii, 6L))), 4L);
    expect_equal(node_radii[which.max(node_strength)], max(node_radii), tolerance = 1e-10);
});


test_that("vis.connectome thresholding selects the strongest edges", {
    skip_if_rgl_required();
    sj = create.synthetic.connectome.subject();
    mat = get.demo.connectivity.matrix();

    cm_all = vis.connectome(mat, subjects_dir = sj$subjects_dir, template_id = sj$subject_id,
        atlas = "testatlas", views = NULL, edge_threshold_quantile = NULL, silent = TRUE);
    expect_equal(nrow(cm_all$edges$from), 6L);

    # The quantile keeps the edges at or above the quantile of the edge magnitudes.
    cm_half = vis.connectome(mat, subjects_dir = sj$subjects_dir, template_id = sj$subject_id,
        atlas = "testatlas", views = NULL, edge_threshold_quantile = 0.5, silent = TRUE);
    magnitudes = mat[upper.tri(mat)];
    expect_equal(nrow(cm_half$edges$from), sum(magnitudes >= stats::quantile(magnitudes, 0.5, names = FALSE)));
    expect_true(nrow(cm_half$edges$from) < 6L);

    # An absolute threshold keeps the edges above it.
    cm_abs = vis.connectome(mat, subjects_dir = sj$subjects_dir, template_id = sj$subject_id,
        atlas = "testatlas", views = NULL, edge_threshold_quantile = NULL,
        edge_threshold = 0.5, silent = TRUE);
    expect_equal(nrow(cm_abs$edges$from), sum(upper.tri(mat) & mat >= 0.5));

    # Thresholding everything away is an error.
    expect_error(vis.connectome(mat, subjects_dir = sj$subjects_dir, template_id = sj$subject_id,
        atlas = "testatlas", views = NULL, edge_threshold_quantile = NULL,
        edge_threshold = 100.0, silent = TRUE), "No edges are left");
});


test_that("vis.connectome draws only positive edges by default, and negative ones on request", {
    skip_if_rgl_required();
    sj = create.synthetic.connectome.subject();
    mat = get.demo.connectivity.matrix();
    mat[1, 2] = mat[2, 1] = -0.5;   # make one edge negative

    cm_pos = vis.connectome(mat, subjects_dir = sj$subjects_dir, template_id = sj$subject_id,
        atlas = "testatlas", views = NULL, edge_threshold_quantile = NULL, silent = TRUE);
    expect_equal(nrow(cm_pos$edges$from), 5L);
    expect_true(all(cm_pos$edges$metadata$src_data > 0.0));

    cm_neg = vis.connectome(mat, subjects_dir = sj$subjects_dir, template_id = sj$subject_id,
        atlas = "testatlas", views = NULL, edge_threshold_quantile = NULL,
        edge_negative = TRUE, silent = TRUE);
    expect_equal(nrow(cm_neg$edges$from), 1L);
    expect_true(cm_neg$edges$metadata$src_data < 0.0);
});


test_that("vis.connectome matches matrix row names to atlas region names", {
    skip_if_rgl_required();
    sj = create.synthetic.connectome.subject();
    mat = get.demo.connectivity.matrix();

    # The case-sensitive match works.
    cm = vis.connectome(mat, subjects_dir = sj$subjects_dir, template_id = sj$subject_id,
        atlas = "testatlas", views = NULL, edge_threshold_quantile = NULL, silent = TRUE);
    expect_equal(nrow(cm$edges$from), 6L);

    # Matching is case insensitive.
    dimnames(mat) = list(c("LHA", "LHB", "RHA", "RHB"), c("LHA", "LHB", "RHA", "RHB"));
    cm2 = vis.connectome(mat, subjects_dir = sj$subjects_dir, template_id = sj$subject_id,
        atlas = "testatlas", views = NULL, edge_threshold_quantile = NULL, silent = TRUE);
    expect_equal(nrow(cm2$edges$from), 6L);

    # An unknown region name is an error which mentions the unknown name.
    dimnames(mat) = list(c("lhA", "lhB", "rhA", "nope"), c("lhA", "lhB", "rhA", "nope"));
    expect_error(vis.connectome(mat, subjects_dir = sj$subjects_dir, template_id = sj$subject_id,
        atlas = "testatlas", views = NULL, silent = TRUE), "nope");
});


test_that("vis.connectome accepts explicit node coordinates and an edge list", {
    skip_if_rgl_required();
    coords = matrix(c(0, 0, 0, 10, 0, 0, 0, 10, 0), ncol = 3L, byrow = TRUE);
    rownames(coords) = c("A", "B", "C");
    mat = matrix(c(0, 0.5, 0.1, 0.5, 0, 0.9, 0.1, 0.9, 0), nrow = 3L, dimnames = list(c("A", "B", "C"), c("A", "B", "C")));

    cm = vis.connectome(mat, node_coords = coords, context = NULL, views = NULL,
        edge_threshold_quantile = NULL, silent = TRUE);
    expect_equal(names(cm), c("edges", "nodes"));

    # The node positions are used as-is.
    expect_equal(cm$edges$from[1L, ], coords[1L, ], ignore_attributes = TRUE);

    # An edge list works as well.
    edge_list = data.frame("source" = c("A", "B", "C"), "target" = c("B", "C", "A"), "weight" = c(0.4, 0.7, 0.2));
    cm_el = vis.connectome(edge_list, node_coords = coords, context = NULL, views = NULL,
        edge_threshold_quantile = NULL, silent = TRUE);
    expect_equal(nrow(cm_el$edges$from), 3L);
});


test_that("vis.connectome validates its input", {
    coords = matrix(c(0, 0, 0, 10, 0, 0), ncol = 3L, byrow = TRUE);
    mat = matrix(c(0, 0.5, 0.5, 0), nrow = 2L);

    expect_error(vis.connectome(matrix(1:6, nrow = 2L), node_coords = coords, views = NULL), "square numeric matrix");
    expect_error(vis.connectome(mat, views = NULL, silent = TRUE), "node_coords");   # no atlas either
    expect_error(vis.connectome(mat, node_coords = rbind(coords, coords), views = NULL), "must match");
    expect_error(vis.connectome(mat, node_coords = coords, context = list("nope" = 1), views = NULL), "Unknown entry");
    expect_error(vis.connectome(mat, node_coords = coords, context = list("alpha" = 2.0), views = NULL), "alpha");
    expect_error(vis.connectome(mat, node_coords = coords, edge_threshold_quantile = 1.5, views = NULL), "edge_threshold_quantile");
    expect_error(vis.connectome(mat, node_coords = coords, edge_width = -1.0, views = NULL), "edge_width");
    expect_error(vis.connectome(mat, node_coords = coords, node_radius = 0.0, views = NULL), "node_radius");
});


test_that("vis.connectome renders with the rgl backend", {
    skip_if_rgl_required();
    coords = matrix(c(-10, 0, 0, 10, 0, 0, 0, 10, 0), ncol = 3L, byrow = TRUE);
    mat = matrix(c(0, 0.5, 0.1, 0.5, 0, 0.9, 0.1, 0.9, 0), nrow = 3L);

    expect_silent(vis.connectome(mat, node_coords = coords, context = NULL,
        views = c("sd_lateral_lh"), rgloptions = list("windowRect" = c(0, 0, 200, 200)), silent = TRUE));
    close.all.rgl.windows();
});


test_that("vis.connectome renders with the scimesh backend and exports with a colorbar", {
    skip_if_not_installed("scimesh");
    coords = matrix(c(-10, 0, 0, 10, 0, 0, 0, 10, 0), ncol = 3L, byrow = TRUE);
    mat = matrix(c(0, 0.5, 0.1, 0.5, 0, 0.9, 0.1, 0.9, 0), nrow = 3L);
    out_img = tempfile(fileext = ".png");

    withr::local_options(list(fsbrain.renderer_backend = "scimesh"));
    withr::local_dir(tempdir());

    cm = vis.connectome(mat, node_coords = coords, context = NULL,
        views = c("sd_lateral_lh"), silent = TRUE);

    expect_silent(export(cm, draw_colorbar = "horizontal", output_img = out_img, silent = TRUE));
    expect_true(file.exists(out_img));
    expect_true(file.size(out_img) > 1000L);
});


test_that("vis.connectome works with a real template atlas if the data is available", {
    skip_if_rgl_required();
    template_dir = tryCatch(resolve.template.subjects.dir("fs_LR_32"), error = function(e) NULL);
    skip_if(is.null(template_dir), "The fs_LR_32 template is not available.");
    skip_if(! file.exists(file.path(template_dir, "fs_LR_32", "label", "lh.schaefer400.annot")), "The fs_LR_32 schaefer400 atlas is not available.");

    centroids = subject.region.centroids(NULL, "fs_LR_32", "schaefer400", surface = "midthickness");
    expect_equal(nrow(centroids), 400L);
    expect_equal(length(unique(rownames(centroids))), 400L);
    expect_equal(sum(centroids$hemi == "lh"), 200L);

    mat = get.demo.connectivity.matrix(rownames(centroids), values = seq(0.0, 1.0, length.out = 400L * 400L));
    cm = vis.connectome(mat, atlas = "schaefer400", views = c("sd_lateral_lh"), silent = TRUE);
    expect_true(is.fs.coloredpaths(cm$edges));
    expect_true(nrow(cm$edges$from) > 100L);
});
