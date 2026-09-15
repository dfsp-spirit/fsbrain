# Tests for volvis.shells(). They use a synthetic sphere volume, so they do not need any downloaded
# data: the iso-surface of the level 'radius - distance from the center' is a sphere of a known
# radius, centered at a known voxel.

# A volume whose iso-surface at level 'radius - r' is a sphere of radius 'r' around 'center'.
sphere.volume = function(dim = 40L, radius = 12.0, center = NULL) {
    if(is.null(center)) {
        center = rep(dim / 2.0, 3L);
    }
    grid = expand.grid(i = seq_len(dim), j = seq_len(dim), k = seq_len(dim));
    dist = sqrt((grid$i - center[1L])^2 + (grid$j - center[2L])^2 + (grid$k - center[3L])^2);
    return(array(radius - dist, dim = c(dim, dim, dim)));
}

# Vertex coordinates of a mesh, as an Nx3 matrix, in the space the mesh is in.
mesh.vertices = function(cmesh) {
    return(t(cmesh$mesh$vb[1:3, , drop = FALSE]));
}

# The RAS position of a voxel of the (non-subsampled) volume, used to check the alignment.
voxel.to.ras = function(voxel_coords) {
    return(as.numeric((vox2ras_tkr() %*% c(voxel_coords, 1.0))[1:3]));
}

# Mean agreement between the stored per-vertex normals of a mesh and the geometric normals computed
# from the winding of its faces. A value near 1 means the normals match the faces (outward), a value
# near -1 means they point inward, which makes the surface render black when lighting is enabled.
normals.agreement = function(mesh) {
    v = t(mesh$vb[1:3, , drop = FALSE]);
    f = mesh$it;
    v1 = v[f[1L, ], , drop = FALSE]; v2 = v[f[2L, ], , drop = FALSE]; v3 = v[f[3L, ], , drop = FALSE];
    ab = v2 - v1; ac = v3 - v1;
    face_normals = cbind(ab[, 2L] * ac[, 3L] - ab[, 3L] * ac[, 2L],
                         ab[, 3L] * ac[, 1L] - ab[, 1L] * ac[, 3L],
                         ab[, 1L] * ac[, 2L] - ab[, 2L] * ac[, 1L]);
    vertex_normals = t(mesh$normals[1:3, , drop = FALSE]);
    face_vertex_normals = (vertex_normals[f[1L, ], ] + vertex_normals[f[2L, ], ] + vertex_normals[f[3L, ], ]) / 3.0;
    normalize = function(x) { return(x / sqrt(rowSums(x^2))); };
    return(mean(rowSums(normalize(face_normals) * normalize(face_vertex_normals)), na.rm = TRUE));
}


test_that("The iso-levels of the shells are computed automatically", {
    vol = sphere.volume();
    fg_quantiles = as.numeric(stats::quantile(vol[vol != 0], probs = c(0.2, 0.95), names = FALSE));

    # 'quantile' (the default): the outermost shell is at the 20 percent quantile of the data.
    levels = fsbrain:::shell.levels(vol, levels = NULL, num_levels = 5L, level_type = "quantile", level_range = c(0.2, 0.95));
    expect_length(levels, 5L);
    expect_true(all(diff(levels) > 0));   # ascending, the outermost shell first
    expect_equal(levels[1L], fg_quantiles[1L], tolerance = 1e-6);

    # Other settings: number of levels and the range from which they are taken.
    expect_length(fsbrain:::shell.levels(vol, num_levels = 3L), 3L);
    expect_true(min(fsbrain:::shell.levels(vol, num_levels = 3L, level_range = c(0.5, 0.6))) > fg_quantiles[1L]);
    linear = fsbrain:::shell.levels(vol, num_levels = 4L, level_type = "linear", level_range = c(0.0, 1.0));
    expect_equal(linear[1L], min(vol[vol != 0]), tolerance = 1e-6);
    expect_equal(linear[4L], max(vol[vol != 0]), tolerance = 1e-6);

    # User-given levels are sorted, so the first entry is always the outermost shell.
    expect_equal(fsbrain:::shell.levels(vol, levels = c(5, 3, 4)), c(3, 4, 5));

    # error handling
    expect_error(fsbrain:::shell.levels(vol, level_type = "dunno"), "level_type");
    expect_error(fsbrain:::shell.levels(vol, num_levels = 0L), "num_levels");
    expect_error(fsbrain:::shell.levels(vol, level_range = c(0.5, 0.4)), "level_range");
    expect_error(fsbrain:::shell.levels(vol, level_type = "quantile", level_range = c(0.5, 1.5)), "quantile");
    expect_error(fsbrain:::shell.levels(vol, levels = c(NA_real_)), "finite");
    expect_error(fsbrain:::shell.levels(array(0, dim = c(4, 4, 4))), "no foreground voxels");
})


test_that("Smoothing and subsampling the volume work as expected", {
    vol = sphere.volume(dim = 32L);

    # Box blur keeps the dimensions, and only the border is affected (clamped edges).
    blurred = fsbrain:::volume.boxblur(vol, passes = 1L);
    expect_equal(dim(blurred), dim(vol));
    expect_true(max(abs(blurred - vol)) > 0);
    expect_equal(fsbrain:::volume.boxblur(vol, passes = 0L), vol);

    # Subsampling reduces the dimensions of the volume.
    sub = fsbrain:::volume.subsample(vol, factor = 2L);
    expect_equal(dim(sub), c(16L, 16L, 16L));
    expect_equal(fsbrain:::volume.subsample(vol, factor = 1L), vol);

    # The subsample matrix maps subsampled voxel indices back to the original volume: index i of the
    # subsampled volume is the center of the original voxels i*f-f+1 ... i*f.
    m = fsbrain:::volume.subsample.matrix(2L);
    expect_equal(as.numeric((m %*% c(1, 1, 1, 1))[1:3]), c(1.5, 1.5, 1.5));
    expect_equal(as.numeric((m %*% c(16, 16, 16, 1))[1:3]), c(31.5, 31.5, 31.5));
    expect_equal(fsbrain:::volume.subsample.matrix(1L), diag(4L));
})


test_that("Both iso-surface backends produce spatially aligned shells", {
    skip_if_not_installed("misc3d");
    vol = sphere.volume(dim = 40L, radius = 12.0);
    level = 6.0;   # -> sphere of radius 6

    for(backend in c("misc3d", "Rvcg")) {
        skip_if_not_installed(backend);
        mesh = fsbrain:::shell.extract.mesh(vol, level = level, backend = backend);

        # The mesh is in 1-based voxel space (no matter the backend) and has normals for shading.
        verts = t(mesh$vb[1:3, , drop = FALSE]);
        expect_true(! is.null(mesh$normals));
        expect_equal(colMeans(verts), rep(20.0, 3L), tolerance = 0.5);          # centered in the volume
        expect_equal(apply(verts, 2L, function(x) diff(range(x))), rep(12.0, 3L), tolerance = 1.0);   # diameter

        # The mesh is backwards-compatible with the vox2ras_tkr transform: the center of the sphere
        # has to end up at the RAS position of the center voxel. This also catches backends which
        # return coordinates in their own convention (0-based, flipped axes, ...).
        ras = apply.transform(mesh, vox2ras_tkr());
        expect_equal(colMeans(mesh.vertices(fs.coloredmesh(ras, "#FF0000", hemi = NULL))), voxel.to.ras(c(20, 20, 20)), tolerance = 0.5);

        # The stored normals must stay consistent with the winding of the faces after the transform:
        # otherwise the surface renders black (with lighting enabled) instead of colored.
        expect_gt(normals.agreement(mesh), 0.9);
        expect_gt(normals.agreement(ras), 0.9);

        # The level is not in the range of the data: no shell.
        expect_null(fsbrain:::shell.extract.mesh(vol, level = 1000.0, backend = backend));
    }
})


test_that("A volume can be visualized as nested, semi-transparent shells", {
    skip_if_not_installed("Rvcg");
    vol = sphere.volume(dim = 40L, radius = 12.0);

    shells = volvis.shells(vol, levels = c(2.0, 5.0, 8.0), views = NULL, silent = TRUE);

    expect_length(shells, 3L);
    for(cmesh in shells) {
        expect_true(is.fs.coloredmesh(cmesh));
        expect_true(inherits(cmesh$mesh, "mesh3d"));
        expect_length(unique(cmesh$col), 1L);          # a single color per shell
        expect_true(cmesh$style$alpha > 0.0 && cmesh$style$alpha <= 1.0);
        expect_equal(cmesh$style$back, "filled");      # required for correct composition in rgl
    }

    # The alphas are an increasing ramp, and the innermost shell is opaque: the outer shells are
    # more transparent than the inner ones.
    alphas = sapply(shells, function(cmesh) cmesh$style$alpha);
    expect_true(all(diff(alphas) > 0));
    expect_equal(alphas[3L], 1.0);
    expect_equal(alphas, c(0.05, 0.30, 1.0));   # the default 'grey_context' palette

    # The default palette renders the context shells in grey and the innermost one in a warm color.
    expect_equal(unique(shells[[1L]]$col), "#9E9E9E");
    expect_equal(unique(shells[[2L]]$col), "#9E9E9E");
    expect_equal(unique(shells[[3L]]$col), "#D94801");

    # The shells are nested: a higher iso-level is a smaller sphere.
    extents = sapply(shells, function(cmesh) diff(range(mesh.vertices(cmesh)[, 1L])));
    expect_true(all(diff(extents) < 0));
    # Radius 12 - level, in voxel units, and the shells are centered at the RAS position of the
    # center voxel of the volume.
    expect_equal(extents, 2.0 * (12.0 - c(2.0, 5.0, 8.0)), tolerance = 1.5);
    for(cmesh in shells) {
        expect_equal(colMeans(mesh.vertices(cmesh)), voxel.to.ras(c(20, 20, 20)), tolerance = 0.5);
    }

    # Colors and alphas can be set explicitly.
    shells2 = volvis.shells(vol, levels = c(2.0, 5.0), views = NULL, silent = TRUE,
        colors = c("#FF0000", "#00FF00"), alphas = c(0.25, 0.75));
    expect_equal(unique(shells2[[1L]]$col), "#FF0000");
    expect_equal(unique(shells2[[2L]]$col), "#00FF00");
    expect_equal(sapply(shells2, function(cmesh) cmesh$style$alpha), c(0.25, 0.75));

    # The console output can be silenced or requested.
    expect_silent(volvis.shells(vol, levels = 5.0, views = NULL, silent = TRUE));
    expect_output(volvis.shells(vol, levels = 5.0, views = NULL, silent = FALSE));
})


test_that("The palettes define the colors and the transparency of the shells", {
    # 'grey_context' (the default): grey context shells, one warm and opaque core.
    gc4 = fsbrain:::shell.palette("grey_context", num_shells = 4L);
    expect_equal(gc4$colors, c("#9E9E9E", "#9E9E9E", "#9E9E9E", "#D94801"));
    expect_equal(gc4$alphas, c(0.05, 0.175, 0.30, 1.0));
    expect_equal(gc4$alphas[4L], 1.0);                          # the core is always opaque
    expect_equal(fsbrain:::shell.palette("grey_context", num_shells = 1L)$colors, "#D94801");
    expect_equal(fsbrain:::shell.palette("grey_context", num_shells = 1L)$alphas, 1.0);

    # 'sequential': a single hue, more opaque towards the inside.
    seq4 = fsbrain:::shell.palette("sequential", num_shells = 4L);
    expect_length(seq4$colors, 4L);
    expect_length(unique(seq4$colors), 4L);
    expect_true(all(diff(seq4$alphas) > 0));
    expect_equal(seq4$alphas[4L], 0.95);

    # 'viridis': the colorful ramp, still available.
    vir4 = fsbrain:::shell.palette("viridis", num_shells = 4L);
    expect_equal(vir4$colors, viridis::viridis(4L));
    expect_equal(vir4$alphas, seq(0.1, 0.8, length.out = 4L));

    # The alpha range can be set explicitly, and the 'grey_context' core stays opaque.
    expect_equal(fsbrain:::shell.palette("grey_context", num_shells = 3L, alpha_range = c(0.2, 0.4))$alphas, c(0.2, 0.4, 1.0));
    expect_equal(fsbrain:::shell.palette("sequential", num_shells = 2L, alpha_range = c(0.1, 0.6))$alphas, c(0.1, 0.6));

    # The palette can be selected in the main function, and explicit colors and alphas still win.
    shells = volvis.shells(sphere.volume(dim = 32L), levels = c(2.0, 5.0, 8.0), palette = "viridis", views = NULL, silent = TRUE);
    expect_equal(unique(shells[[1L]]$col), viridis::viridis(3L)[1L]);
    shells2 = volvis.shells(sphere.volume(dim = 32L), levels = c(2.0, 5.0), palette = "grey_context",
        colors = c("#FF0000", "#0000FF"), alphas = c(0.75, 0.25), views = NULL, silent = TRUE);
    expect_equal(unique(shells2[[1L]]$col), "#FF0000");
    expect_equal(sapply(shells2, function(cmesh) cmesh$style$alpha), c(0.75, 0.25));

    # error handling
    expect_error(fsbrain:::shell.palette("dunno", num_shells = 3L), "palette");
    expect_error(fsbrain:::shell.palette("sequential", num_shells = 3L, alpha_range = c(0.5, 2.0)), "alpha_range");
    expect_error(fsbrain:::shell.palette("sequential", num_shells = -1L), "num_shells");
    expect_error(volvis.shells(sphere.volume(dim = 32L), levels = 5.0, palette = "dunno", views = NULL), "palette");
    expect_error(volvis.shells(sphere.volume(dim = 32L), levels = 5.0, alpha_range = c(0.5, 0.4), views = NULL), "alpha_range");
})


test_that("Shells can be cut open and subsampled", {
    skip_if_not_installed("Rvcg");
    vol = sphere.volume(dim = 40L, radius = 12.0);

    full = volvis.shells(vol, levels = 6.0, views = NULL, silent = TRUE)[[1L]];
    cut = volvis.shells(vol, levels = 6.0, cut_away = "right", cut_fraction = 0.5, views = NULL, silent = TRUE)[[1L]];

    # The remaining half of the cut shell is on the 'kept' side of the cut plane and has less faces.
    full_verts = mesh.vertices(full);
    cut_verts = mesh.vertices(cut);
    cut_position = min(full_verts[, 1L]) + 0.5 * diff(range(full_verts[, 1L]));
    expect_true(max(cut_verts[, 1L]) <= cut_position + 1e-6);
    expect_lt(ncol(cut$mesh$it), ncol(full$mesh$it));
    expect_lt(ncol(cut$mesh$vb), ncol(full$mesh$vb));   # unused vertices are dropped
    # The other dimensions are not affected.
    expect_equal(diff(range(cut_verts[, 3L])), diff(range(full_verts[, 3L])), tolerance = 1e-6);

    # Subsampling reduces the number of faces, but keeps the spatial extent and the alignment.
    sub = volvis.shells(vol, levels = 6.0, downsample = 4L, views = NULL, silent = TRUE)[[1L]];
    expect_lt(ncol(sub$mesh$it), ncol(full$mesh$it));
    expect_equal(diff(range(mesh.vertices(sub)[, 1L])), diff(range(full_verts[, 1L])), tolerance = 1.0);
    expect_equal(colMeans(mesh.vertices(sub)), colMeans(full_verts), tolerance = 0.5);

    # Smoothing is optional, and a warning is emitted for subsampling without smoothing.
    expect_silent(volvis.shells(vol, levels = 6.0, smoothing = 0L, views = NULL, silent = TRUE));
    expect_warning(volvis.shells(vol, levels = 6.0, downsample = 2L, smoothing = 0L, views = NULL, silent = TRUE), "aliasing");
})


test_that("volvis.shells reports invalid parameters", {
    vol = sphere.volume(dim = 32L);

    expect_error(volvis.shells("notavolume", views = NULL), "volume");
    expect_error(volvis.shells(vol, views = NULL, backend = "nosuchpkg"), "backend");
    expect_error(volvis.shells(vol, views = NULL, smoothing = -1L), "smoothing");
    expect_error(volvis.shells(vol, views = NULL, downsample = 0.5), "downsample");
    expect_error(volvis.shells(vol, views = NULL, cut_fraction = 2.0), "cut_fraction");
    expect_error(volvis.shells(vol, levels = 5.0, cut_away = "dunno", views = NULL), "cut_away");
    expect_error(volvis.shells(vol, levels = 5000.0, views = NULL), "iso-level");
    expect_error(volvis.shells(vol, levels = c(1.0, 2.0), colors = c("#FF0000"), views = NULL), "one color per shell");
    expect_error(volvis.shells(vol, levels = c(1.0, 2.0), alphas = c(0.5), views = NULL), "per shell");
    expect_error(volvis.shells(vol, levels = c(1.0, 2.0), alpha_range = c(0.5, 2.0), views = NULL), "alpha_range");

    # A 4D volume: only a single valid frame can be selected.
    vol4d = array(vol, dim = c(dim(vol), 2L));
    expect_silent(volvis.shells(vol4d, levels = 5.0, frame = 2L, views = NULL, silent = TRUE));
    expect_error(volvis.shells(vol4d, levels = 5.0, frame = 3L, views = NULL), "frame");
    expect_error(volvis.shells(vol4d, levels = 5.0, frame = "all", views = NULL), "frame");
})


test_that("Shells are rendered on request and returned either way", {
    skip_if_not_installed("Rvcg");
    skip_if_rgl_window_required();
    vol = sphere.volume(dim = 40L, radius = 12.0);

    shells = volvis.shells(vol, levels = c(2.0, 5.0), downsample = 2L, views = "sd_lateral_lh", silent = TRUE);
    expect_length(shells, 2L);
    expect_true(all(sapply(shells, is.fs.coloredmesh)));
})


test_that("Shells can be rendered with the scimesh backend", {
    skip_if_not_installed("Rvcg");
    skip_if_not_installed("scimesh");
    vol = sphere.volume(dim = 40L, radius = 12.0);
    out_img = tempfile(fileext = ".png");

    withr::local_options(list(fsbrain.renderer_backend = "scimesh"));
    withr::local_dir(tempdir());   # the scimesh backend writes the rendered views to the working dir
    shells = volvis.shells(vol, levels = c(2.0, 5.0), downsample = 2L, views = "sd_lateral_lh", silent = TRUE);

    expect_true(is.fs.coloredmesh(shells[[1L]]));
    expect_true(file.exists("fsbrain_views_scimesh.png"));
    expect_gt(file.size("fsbrain_views_scimesh.png"), 0L);
    file.remove("fsbrain_views_scimesh.png");
})
