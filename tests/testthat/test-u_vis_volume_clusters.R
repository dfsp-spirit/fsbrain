# Tests for vis.volume.clusters(). They use a synthetic volume with two Gaussian blobs (one positive,
# one negative), so they do not need any downloaded data: the iso-surface of a blob at a given level
# is a sphere of a known radius around a known voxel.

# A volume with a positive blob centered at R index (14, 20, 20) and a negative one at (27, 20, 20).
cluster.test.volume = function(dim = 40L, sigma = 6.0, pos_peak = 6.0, neg_peak = -5.0) {
    grid = expand.grid(i = seq_len(dim), j = seq_len(dim), k = seq_len(dim));
    pos_dist = sqrt((grid$i - 14.0)^2 + (grid$j - 20.0)^2 + (grid$k - 20.0)^2);
    neg_dist = sqrt((grid$i - 27.0)^2 + (grid$j - 20.0)^2 + (grid$k - 20.0)^2);
    vol = pos_peak * exp(-pos_dist^2 / (2.0 * sigma^2)) + neg_peak * exp(-neg_dist^2 / (2.0 * sigma^2));
    vol[abs(vol) < 0.2] = 0.0;     # mark the background, so that the automatic threshold ignores it
    return(array(vol, dim = c(dim, dim, dim)));
}

# Surface RAS position of an R array index, i.e., what a cluster centered at that index has to be
# rendered at.
index.to.ras = function(r_index) {
    return(as.numeric((fsbrain:::index2ras_tkr() %*% c(r_index, 1.0))[1:3]));
}

# Mean vertex position of a coloredmesh, in surface RAS.
mesh.center = function(cmesh) {
    return(colMeans(t(cmesh$mesh$vb[1:3, , drop = FALSE])));
}


test_that("The cluster threshold is taken from the data or from the user", {
    vol = cluster.test.volume();

    # An explicit threshold is returned as given.
    expect_equal(fsbrain:::volume.cluster.threshold(vol, threshold = 2.5), 2.5);

    # Without a threshold, the requested quantile of the absolute values of the non-zero voxels is used.
    expected = as.numeric(stats::quantile(abs(vol[vol != 0.0]), probs = 0.9, names = FALSE));
    expect_equal(fsbrain:::volume.cluster.threshold(vol, threshold_quantile = 0.9), expected);

    # The default quantile is larger than the 0.9 one.
    expect_gt(fsbrain:::volume.cluster.threshold(vol), fsbrain:::volume.cluster.threshold(vol, threshold_quantile = 0.9));

    # Error handling.
    expect_error(fsbrain:::volume.cluster.threshold(vol, threshold = -1.0), "positive");
    expect_error(fsbrain:::volume.cluster.threshold(vol, threshold = c(1.0, 2.0)), "positive");
    expect_error(fsbrain:::volume.cluster.threshold(vol, threshold_quantile = 1.5), "between 0 and 1");
    expect_error(fsbrain:::volume.cluster.threshold(array(0.0, dim = c(3L, 3L, 3L))), "no non-zero voxels");
})


test_that("The iso-levels of the cluster shells are computed per sign", {
    vol = cluster.test.volume();
    threshold = 1.0;

    # Both signs, 4 levels each: the levels are ordered from the outside (the threshold) to the
    # inside (the highest level), and the negative levels come first.
    levels = fsbrain:::volume.cluster.levels(vol, threshold = threshold, num_levels = 4L);
    expect_length(levels, 8L);
    expect_equal(levels[1L], -threshold);
    expect_equal(levels[5L], threshold);
    expect_true(all(diff(abs(levels[1:4])) > 0));
    expect_true(all(diff(abs(levels[5:8])) > 0));
    expect_true(all(levels[1:4] < 0.0));
    expect_true(all(levels[5:8] > 0.0));
    # The outermost level is the threshold, and the innermost one is within the range of the data.
    expect_lt(max(abs(levels)), 6.0);
    expect_lte(max(levels), max(vol[vol > threshold]));
    expect_gte(min(levels), min(vol[vol < -threshold]));

    # A single sign.
    pos_levels = fsbrain:::volume.cluster.levels(vol, threshold = threshold, num_levels = 3L, negative = FALSE);
    expect_length(pos_levels, 3L);
    expect_true(all(pos_levels > 0.0));
    neg_levels = fsbrain:::volume.cluster.levels(vol, threshold = threshold, num_levels = 3L, positive = FALSE);
    expect_length(neg_levels, 3L);
    expect_true(all(neg_levels < 0.0));

    # An explicit highest level is used for both signs.
    fixed = fsbrain:::volume.cluster.levels(vol, threshold = threshold, num_levels = 2L, max_level = 4.0);
    expect_equal(fixed, c(-threshold, -4.0, threshold, 4.0));

    # The number of levels can be 1, in which case the only level is the threshold.
    expect_equal(fsbrain:::volume.cluster.levels(vol, threshold = threshold, num_levels = 1L), c(-threshold, threshold));

    # Error handling.
    expect_error(fsbrain:::volume.cluster.levels(vol, threshold = threshold, max_level = -2.0), "positive");
    expect_error(fsbrain:::volume.cluster.levels(vol, threshold = threshold, max_level_quantile = 0.0), "between 0 and 1");
})


test_that("The alpha value of a shell grows towards the cluster core", {
    levels = c(-3.0, -4.0, -5.0, 3.0, 4.0, 5.0);
    alphas = sapply(levels, function(level) { fsbrain:::cluster.shell.alpha(level, threshold = 3.0, levels = levels, alpha_range = c(0.1, 1.0)) });
    expect_equal(alphas[1L], 0.1);
    expect_equal(alphas[3L], 1.0);
    expect_equal(alphas[4L], 0.1);
    expect_equal(alphas[6L], 1.0);
    expect_true(all(diff(alphas[1:3]) > 0));
    expect_true(all(diff(alphas[4:6]) > 0));

    # A single shell per sign represents the cluster boundary, so it is opaque.
    expect_equal(fsbrain:::cluster.shell.alpha(3.0, threshold = 3.0, levels = c(-3.0, 3.0), alpha_range = c(0.1, 1.0)), 1.0);
})


test_that("The shell colors come from one shared colormap", {
    levels = c(-5.0, -3.0, 3.0, 5.0);
    colmap = fsbrain:::volume.clusters.colormap(mkco.cluster(), levels);
    expect_length(colmap$colors, 4L);
    expect_equal(colmap$range, c(-5.0, 5.0));
    # The colormap is symmetric: the colors of a level and its negative are on the two sides of the
    # neutral middle color.
    expect_true(all(colmap$colors[1:2] != colmap$colors[3:4]));
    expect_equal(unique(colmap$colors[1:2]), colmap$colors[1:2]);   # hmm, distinct colors per level

    # A user-requested range is honored, and it is kept for the colorbar.
    fixed = fsbrain:::volume.clusters.colormap(list("colFn" = cm.div(), "n" = 10L, "symm" = TRUE, "range" = c(-10.0, 10.0)), levels);
    expect_equal(fixed$range, c(-10.0, 10.0));
    expect_equal(fixed$options$range, c(-10.0, 10.0));
    expect_error(fsbrain:::volume.clusters.colormap(list("colFn" = cm.div(), "range" = c(5.0, 1.0)), levels), "ascending");
})


test_that("vis.volume.clusters returns one nested shell per iso-level", {
    vol = cluster.test.volume();
    threshold = 1.5;

    # No context mesh and no rendering, so that no subject data is needed.
    cm = vis.volume.clusters(volume = vol, threshold = threshold, num_levels = 4L, context = NULL,
        views = NULL, silent = TRUE);

    # 4 levels for each of the 2 signs.
    expect_length(cm, 8L);
    expect_true(all(sapply(cm, is.fs.coloredmesh)));
    expect_true(all(sapply(cm, function(x) { is.null(x$hemi); })));   # volume meshes are not hemi-specific

    # The alphas increase towards the core of each cluster, which is opaque, and the alpha ramp is
    # applied separately per sign.
    alphas = sapply(cm, function(x) { x$style$alpha });
    expect_equal(alphas[4L], 1.0);
    expect_equal(alphas[8L], 1.0);
    expect_true(all(diff(alphas[1:4]) > 0));
    expect_true(all(diff(alphas[5:8]) > 0));
    expect_equal(alphas[1L], alphas[5L]);
    # The backfaces are drawn, otherwise the nested transparent shells are composited in the wrong order.
    expect_true(all(sapply(cm, function(x) { identical(x$style$back, "filled"); })));

    # A single color per shell, taken from the shared colormap.
    expect_true(all(sapply(cm, function(x) { length(unique(x$col)) == 1L; })));
    expect_equal(sapply(cm, function(x) { unique(x$col)[1L]; }), fsbrain:::volume.clusters.colormap(mkco.cluster(), sapply(cm, function(x) { x$metadata$src_data }))$colors);

    # The shells are nested: for a blob, a higher iso-level is a smaller sphere. The positive shells
    # are the last ones in the returned list.
    pos_center = index.to.ras(c(14.0, 20.0, 20.0));
    radii = sapply(cm[5:8], function(x) { max(sqrt(rowSums(sweep(t(x$mesh$vb[1:3, , drop = FALSE]), 2L, pos_center, "-")^2))) });
    expect_true(all(diff(radii) < 0.0));
})


test_that("The cluster shells are aligned with the surfaces", {
    vol = cluster.test.volume();
    # No smoothing, so that the iso-surface is exactly where the data crosses the level.
    cm = vis.volume.clusters(volume = vol, threshold = 1.0, num_levels = 3L, smoothing = 0L,
        context = NULL, views = NULL, silent = TRUE);

    # The positive blob is centered at R index (14, 20, 20) and the negative one at (27, 20, 20). The
    # transformation from R array indices to surface RAS is checked here, i.e., an offset of a single
    # voxel (1 mm) would be detected.
    pos_expected = index.to.ras(c(14.0, 20.0, 20.0));
    neg_expected = index.to.ras(c(27.0, 20.0, 20.0));
    for(level_idx in 1:3) {
        expect_equal(mesh.center(cm[[level_idx]]), neg_expected, tolerance = 0.5);
        expect_equal(mesh.center(cm[[3L + level_idx]]), pos_expected, tolerance = 0.5);
    }

    # Within one cluster, the shells are nested: the innermost one is the smallest.
    radii = sapply(cm[4:6], function(x) { max(sqrt(rowSums(sweep(t(x$mesh$vb[1:3, , drop = FALSE]), 2L, pos_expected, "-")^2))) });
    expect_true(all(diff(radii) < 0.0));
})


test_that("vis.volume.clusters supports a single shell per cluster", {
    vol = cluster.test.volume();
    cm = vis.volume.clusters(volume = vol, threshold = 1.5, num_levels = 1L, context = NULL, views = NULL, silent = TRUE);
    expect_length(cm, 2L);
    # A single shell is the cluster boundary, so it is fully opaque.
    expect_equal(sapply(cm, function(x) { x$style$alpha }), c(1.0, 1.0));

    # The colormap covers the peak of the clusters and not only the rendered iso-levels, so that a
    # shell at the threshold uses a color from the middle of the colormap (and not the most extreme
    # color of the palette), and the colorbar shows the range of the underlying data.
    col_range = cm[[1L]]$metadata$makecmap_options$range;
    expect_gt(col_range[2L], 1.5);
    expect_lt(col_range[2L], 6.0);
    expect_equal(col_range, c(-col_range[2L], col_range[2L]));
    # The same range is used for any number of levels.
    cm_multi = vis.volume.clusters(volume = vol, threshold = 1.5, num_levels = 5L, context = NULL, views = NULL, silent = TRUE);
    expect_equal(cm_multi[[1L]]$metadata$makecmap_options$range, col_range);
})


test_that("vis.volume.clusters can compute the threshold and the levels from the data", {
    vol = cluster.test.volume();
    expect_silent(cm <- vis.volume.clusters(volume = vol, context = NULL, views = NULL, num_levels = 2L, silent = TRUE));
    expect_true(length(cm) >= 2L);
    expect_output(vis.volume.clusters(volume = vol, context = NULL, views = NULL, num_levels = 2L), "threshold");
})


test_that("vis.volume.clusters reports invalid parameters", {
    vol = cluster.test.volume();

    expect_error(vis.volume.clusters(volume = "notavolume", context = NULL, views = NULL), "volume");
    expect_error(vis.volume.clusters(volume = vol, threshold = 1.0, num_levels = 0L, context = NULL, views = NULL), "num_levels");
    expect_error(vis.volume.clusters(volume = vol, threshold = 1.0, positive = FALSE, negative = FALSE, context = NULL, views = NULL), "nothing to visualize");
    expect_error(vis.volume.clusters(volume = vol, threshold = 1.0, smoothing = -1L, context = NULL, views = NULL), "smoothing");
    expect_error(vis.volume.clusters(volume = vol, threshold = 1.0, alpha_range = c(0.9, 0.1), context = NULL, views = NULL), "alpha_range");
    expect_error(vis.volume.clusters(volume = vol, threshold = 1.0, max_level = 0.5, context = NULL, views = NULL), "max_level");
    expect_error(vis.volume.clusters(volume = vol, threshold = 1.0, backend = "nosuchpkg", context = NULL, views = NULL), "backend");
    # The threshold is beyond the data range.
    expect_error(vis.volume.clusters(volume = vol, threshold = 100.0, context = NULL, views = NULL), "no clusters to visualize");
    # A 4D volume with an invalid frame.
    expect_error(vis.volume.clusters(volume = array(vol, dim = c(dim(vol), 2L)), threshold = 1.0, frame = 3L, context = NULL, views = NULL), "frame");
    # Subsampling without smoothing warns.
    expect_warning(vis.volume.clusters(volume = vol, threshold = 1.0, downsample = 2L, smoothing = 0L, context = NULL, views = NULL, silent = TRUE), "aliasing");
})


test_that("vis.volume.clusters renders the clusters inside a pre-built context mesh", {
    skip_if_not_installed("Rvcg");
    vol = cluster.test.volume();

    # A context mesh can be passed directly, in which case no subject data is needed.
    ctx_lh = fs.coloredmesh(rgl::icosahedron3d(), "#B0B0B0", hemi = "lh");
    ctx_lh$style = list("alpha" = 0.2);
    ctx_rh = fs.coloredmesh(rgl::icosahedron3d(), "#B0B0B0", hemi = "rh");
    ctx_rh$style = list("alpha" = 0.2);
    context = list("lh" = ctx_lh, "rh" = ctx_rh);
    cm = vis.volume.clusters(volume = vol, threshold = 1.5, num_levels = 2L, context = context, views = NULL, silent = TRUE);

    # The two context meshes come first, then the cluster shells.
    expect_length(cm, 6L);
    expect_equal(cm[[1L]]$hemi, "lh");
    expect_equal(cm[[2L]]$hemi, "rh");
    expect_true(all(sapply(cm[3:6], function(x) { is.null(x$hemi); })));
    # The context mesh keeps its own style, and it does not contribute to the colorbar.
    expect_equal(cm[[1L]]$style$alpha, 0.2);
    expect_null(coloredmeshes.get.md(list(cm[[1L]]), 'makecmap_options'));
    expect_false(is.null(coloredmeshes.get.md(cm[3:6], 'makecmap_options')));

    # Mixing hemisphere-specific context meshes with the non-hemispheric cluster shells is supported,
    # so the renderers have to sort them into the views without complaining: a context mesh is shown
    # in the views of its own hemisphere, while the cluster shells (which are not hemisphere-specific)
    # are shown in the views of both hemispheres.
    sorted = NULL;
    expect_silent(sorted <- fsbrain:::get.sorted.cmeshes(cm));
    num_shells = length(cm) - 2L;       # all renderables but the two context meshes
    expect_length(sorted$lh, num_shells + 1L);   # the left context mesh and all cluster shells
    expect_length(sorted$rh, num_shells + 1L);   # the right context mesh and all cluster shells
    expect_equal(sorted$lh[[1L]]$hemi, "lh");
    expect_equal(sorted$rh[[1L]]$hemi, "rh");
    # A list which contains nothing but hemisphere meshes is still reported, that is the case the
    # warning was originally added for.
    expect_warning(fsbrain:::get.sorted.cmeshes(list(ctx_lh, ctx_rh)), "old style of passing coloredmeshes");

    # Rendering the scene to an image is only checked if a renderer backend is available which can
    # write images in this environment (the headless scimesh backend; the rgl backend needs a
    # working display/OpenGL, see the tests of the other vis functions).
    if(requireNamespace("scimesh", quietly = TRUE) && requireNamespace("magick", quietly = TRUE)) {
        old_backend = getOption("fsbrain.renderer_backend");
        options(fsbrain.renderer_backend = "scimesh");
        out_img = tempfile(fileext = ".png");
        expect_silent(export(cm, view_angles = c("sd_lateral_lh"), colorbar_legend = "value", output_img = out_img, silent = TRUE));
        options(fsbrain.renderer_backend = old_backend);
        expect_true(file.exists(out_img));
        expect_gt(file.size(out_img), 0L);
        file.remove(out_img);
    }
})


test_that("vis.volume.clusters renders the clusters inside a surface of a subject", {
    skip_if_not_installed("Rvcg");
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir", mustWork = FALSE);
    skip_if_not(nchar(subjects_dir) > 0L && file.exists(file.path(subjects_dir, "fsaverage", "surf", "lh.white")),
        "The surfaces of the fsaverage subject are not available, skipping the context surface test.");
    skip_if_rgl_window_required();

    # A small volume in the space of the template subject, with clusters in the center.
    sdim = 60L;
    vol = cluster.test.volume(dim = sdim, sigma = 5.0);
    cm = vis.volume.clusters(subjects_dir, "fsaverage", vol, threshold = 1.5, num_levels = 2L,
        downsample = 2L, views = "sd_lateral_lh", silent = TRUE);

    expect_length(cm, 6L);
    expect_equal(cm[[1L]]$hemi, "lh");
    expect_equal(cm[[2L]]$hemi, "rh");
    expect_true(all(sapply(cm[3:6], function(x) { is.null(x$hemi); })));

    # The context mesh is rendered semi-transparently (the default of the context definition), and an
    # alpha value given by the user takes precedence.
    expect_gt(cm[[1L]]$style$alpha, 0.0);
    expect_lt(cm[[1L]]$style$alpha, 0.5);
    cm_alpha = vis.volume.clusters(subjects_dir, "fsaverage", vol, threshold = 1.5, num_levels = 1L,
        downsample = 2L, context = list("surface" = "white", "alpha" = 0.13), views = NULL, silent = TRUE);
    expect_equal(cm_alpha[[1L]]$style$alpha, 0.13);
    expect_equal(cm_alpha[[2L]]$style$alpha, 0.13);
})
