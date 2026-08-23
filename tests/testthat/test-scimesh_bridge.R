# Tests for the scimesh renderer backend bridge and the shared plain-R
# geometry transform helpers.

test_that("rotation.matrix matches rgl::rotationMatrix.", {
    angles <- c(0, pi / 2, pi, 0.3, -1.2);
    axes <- list(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(1, 1, 1), c(0.3, -2, 0.5));
    for (a in angles) {
        for (ax in axes) {
            m1 <- rotation.matrix(a, ax[1], ax[2], ax[3]);
            m2 <- rgl::rotationMatrix(a, ax[1], ax[2], ax[3]);
            expect_equal(m1, m2, tolerance = 1e-12);
        }
    }
});


test_that("transform_renderable rotates matrices and mesh3d objects (incl. normals).", {
    M <- rotation.matrix(pi / 2, 0, 0, 1);
    pts <- rbind(c(1, 2, 3), c(4, 5, 6), c(0, 0, 1));
    expect_equal(transform_renderable(pts, M), rgl::rotate3d(pts, pi / 2, 0, 0, 1), tolerance = 1e-12);

    m <- rgl::tetrahedron3d();
    rot <- transform_renderable(m, M);
    expect_equal(rot$vb, rgl::rotate3d(m, pi / 2, 0, 0, 1)$vb, tolerance = 1e-12);

    m2 <- rgl::tetrahedron3d();
    m2$normals <- m2$vb;
    m2$normals[4, ] <- 0;
    rot2 <- transform_renderable(m2, M);
    expect_equal(rot2$normals[1:3, ], t(M[1:3, 1:3]) %*% m2$normals[1:3, ], tolerance = 1e-12);
});


test_that("color_to_rgba converts hex and named colors.", {
    expect_equal(color_to_rgba("#FF0000"), c(1, 0, 0, 1));
    expect_equal(color_to_rgba("#FF0000FF"), c(1, 0, 0, 1));
    expect_equal(color_to_rgba("white"), c(1, 1, 1, 1));
});


test_that("color_to_rgba handles NA as fully transparent.", {
    expect_equal(color_to_rgba(NA_character_), c(0, 0, 0, 0));
});


test_that("apply.style.alpha extracts alpha from style parameters.", {
    expect_equal(apply.style.alpha(list(alpha = 0.4)), 0.4);
    expect_equal(apply.style.alpha(list(shininess = 50)), 1.0);
});


test_that("view.angle.to.hemi.filter maps views to hemispheres.", {
    expect_equal(view.angle.to.hemi.filter("lateral_lh"), "lh");
    expect_equal(view.angle.to.hemi.filter("sd_medial_rh"), "rh");
    expect_equal(view.angle.to.hemi.filter("dorsal"), "both");
    expect_equal(view.angle.to.hemi.filter("caudal"), "both");
});


test_that("get.rglstyle.parameters resolves 'from_mesh' correctly.", {
    r <- structure(list(style = "glass"), class = "fs.coloredmesh");
    expect_equal(get.rglstyle.parameters(r, "from_mesh")$alpha, 0.4);

    r2 <- structure(list(), class = "fs.coloredmesh");
    expect_equal(get.rglstyle.parameters(r2, "from_mesh")$shininess, 50);
});


test_that("get.fsbrain.renderer.backend defaults to 'rgl'.", {
    old <- getOption("fsbrain.renderer_backend");
    on.exit(options(fsbrain.renderer_backend = old));
    options(fsbrain.renderer_backend = NULL);
    expect_equal(get.fsbrain.renderer.backend(), "rgl");
});


test_that("get.fsbrain.scimesh.output.dims defaults to 1920x1080.", {
    old <- getOption("fsbrain.scimesh.output_dims");
    on.exit(options(fsbrain.scimesh.output_dims = old));
    options(fsbrain.scimesh.output_dims = NULL);
    expect_equal(get.fsbrain.scimesh.output.dims(), c(1920L, 1080L));
});


test_that("coloredmesh_to_scimesh applies per-mesh alpha from 'from_mesh'.", {
    testthat::skip_if_not_installed("scimesh");
    mesh <- rgl::tetrahedron3d();
    cm <- structure(list(mesh = mesh, col = "#FF0000", render = TRUE,
                         style = list(alpha = 0.4)), class = "fs.coloredmesh");
    sm <- coloredmesh_to_scimesh(cm, "from_mesh");
    expect_equal(as.numeric(sm$colors[1, "A"]), 0.4);
});


test_that("scimesh can render a synthetic scene headlessly.", {
    testthat::skip_if_not_installed("scimesh");
    mesh <- rgl::tetrahedron3d();
    cm <- structure(list(mesh = mesh, col = "#FF0000", render = TRUE), class = "fs.coloredmesh");
    scene <- coloredmeshes_to_scimesh(cm);
    expect_equal(length(scene), 1L);

    cam <- scimesh::camera(eye = c(0, 0, 5), center = c(0, 0, 0), projection = "orthographic");
    opts <- scimesh::render_options(width = 32L, height = 32L, background_color = c(1, 1, 1, 1));
    img <- scimesh::render_scene(scene, cam, opts);
    out <- tempfile(fileext = ".png");
    scimesh::write_png(img, out);
    expect_true(file.exists(out));
    expect_gt(file.size(out), 0);
});


# --- Pure unit tests (no scimesh required) ------------------------------------

test_that("hex_colors_to_rgba_matrix produces Nx4 RGBA matrices.", {
    m <- hex_colors_to_rgba_matrix(c("#FF0000", "#00FF00", "#0000FF"));
    expect_equal(dim(m), c(3L, 4L));
    expect_equal(colnames(m), c("R", "G", "B", "A"));
    expect_equal(unname(m[1, ]), c(1, 0, 0, 1));
    expect_equal(unname(m[2, ]), c(0, 1, 0, 1));
    expect_equal(unname(m[3, ]), c(0, 0, 1, 1));
});


test_that("hex_colors_to_rgba_matrix maps NA colors to fully transparent rows.", {
    m <- hex_colors_to_rgba_matrix(c("#FF0000", NA_character_, "#00FF00"));
    expect_equal(unname(m[2, ]), c(0, 0, 0, 0));
});


test_that("hex_colors_to_rgba_matrix errors on non-character input.", {
    expect_error(hex_colors_to_rgba_matrix(c(1, 2, 3)));
});


test_that("filter_scene_by_view selects hemispheres from a hemilist scene.", {
    scene <- list(lh = "mesh_lh", rh = "mesh_rh");
    expect_equal(filter_scene_by_view(scene, "both"), list("mesh_lh", "mesh_rh"));
    expect_equal(filter_scene_by_view(scene, "lh"), list("mesh_lh"));
    expect_equal(filter_scene_by_view(scene, "rh"), list("mesh_rh"));
});


test_that("filter_scene_by_view handles empty and non-hemilist scenes.", {
    expect_equal(filter_scene_by_view(list(), "both"), list());
    expect_equal(filter_scene_by_view(list(lh = "mesh_lh"), "rh"), list());
    unnamed <- list("a", "b");
    expect_equal(filter_scene_by_view(unnamed, "both"), unnamed);
});


test_that("transform_coords applies 4x4 transforms to coordinates.", {
    M <- rotation.matrix(pi / 2, 0, 0, 1);
    pts <- rbind(c(1, 2, 3), c(4, 5, 6), c(0, 0, 1));
    expect_equal(transform_coords(pts, M), rgl::rotate3d(pts, pi / 2, 0, 0, 1), tolerance = 1e-12);

    expect_equal(transform_coords(NULL, M), NULL);
    expect_equal(transform_coords(c(1, 0, 0), M),
                 rgl::rotate3d(matrix(c(1, 0, 0), ncol = 3L), pi / 2, 0, 0, 1),
                 tolerance = 1e-12);

    expect_error(transform_coords(cbind(1, 2), M));   # not an Nx3 matrix
});


test_that("view.angle.to.hemi.filter errors on invalid angles.", {
    expect_error(view.angle.to.hemi.filter("not_a_view"));
});


# --- scimesh-gated unit tests (headless, no interactive plotting) -------------

test_that("view_angle_to_scimesh_camera maps views to cameras and hemisphere filters.", {
    testthat::skip_if_not_installed("scimesh");
    mesh <- rgl::tetrahedron3d();
    cm <- structure(list(mesh = mesh, col = "#FF0000", render = TRUE), class = "fs.coloredmesh");
    scene <- coloredmeshes_to_scimesh(list(lh = cm, rh = cm));

    expected <- c("lateral_lh" = "lh", "medial_lh" = "lh", "lateral_rh" = "rh",
                  "medial_rh" = "rh", "dorsal" = "both", "ventral" = "both",
                  "rostral" = "both", "caudal" = "both");
    for (v in names(expected)) {
        res <- view_angle_to_scimesh_camera(scene, v);
        expect_equal(res$hemi_filter, expected[[v]]);
        expect_true(all(c("eye", "center", "up", "projection", "fov") %in% names(res$camera)));
    }
});


test_that("view_angle_to_scimesh_camera errors on invalid views and empty scenes.", {
    testthat::skip_if_not_installed("scimesh");
    mesh <- rgl::tetrahedron3d();
    cm <- structure(list(mesh = mesh, col = "#FF0000", render = TRUE), class = "fs.coloredmesh");
    scene <- coloredmeshes_to_scimesh(list(lh = cm, rh = cm));
    expect_error(view_angle_to_scimesh_camera(scene, "not_a_view"));
    expect_error(view_angle_to_scimesh_camera(list(), "dorsal"));
});


test_that("view_angle_to_scimesh_camera frames with the bounding sphere (Option B, margin 1.0).", {
    testthat::skip_if_not_installed("scimesh");
    mesh <- rgl::tetrahedron3d();
    cm <- structure(list(mesh = mesh, col = "#FF0000", render = TRUE), class = "fs.coloredmesh");
    scene <- coloredmeshes_to_scimesh(list(lh = cm, rh = cm));

    for (v in c("lateral_lh", "dorsal", "ventral", "caudal")) {
        res <- view_angle_to_scimesh_camera(scene, v);
        dist <- sqrt(sum((res$camera$eye - res$camera$center)^2));

        # The scimesh orthographic frustum half-height equals |eye - center|, so
        # framing with dist == bounding-sphere radius (no extra margin) makes the
        # scimesh framing identical to rgl's orthographic auto-fit. See
        # TODO_FSBRAIN_RGL_CAM.md (Step 2, Option B).
        hemi_meshes <- filter_scene_by_view(scene, res$hemi_filter);
        if (length(hemi_meshes) == 0L) {
            hemi_meshes <- filter_scene_by_view(scene, "both");
        }
        expected_radius <- bounding_sphere(lapply(hemi_meshes, function(m) m$vertices))$radius;
        expect_equal(dist, expected_radius, tolerance = 1e-12,
                     info = sprintf("View '%s': camera distance must equal the bounding-sphere radius.", v));

        # The eye must lie along the view direction from the center.
        dir <- res$camera$eye - res$camera$center;
        expect_equal(sqrt(sum(dir^2)), dist, tolerance = 1e-12);
    }
});


test_that("fsbrain_style_to_scimesh_options maps styles to render options.", {
    testthat::skip_if_not_installed("scimesh");

    opts_default <- fsbrain_style_to_scimesh_options("default");
    expect_false(isTRUE(opts_default$wireframe));
    expect_true(isTRUE(opts_default$backface_culling));
    expect_equal(opts_default$width, 800L);
    expect_equal(opts_default$height, 600L);
    expect_equal(opts_default$background_color, c(1, 1, 1, 1));
    expect_equal(opts_default$specular_color, c(0, 0, 0, 1));
    expect_equal(opts_default$shininess, 50);

    opts_edges <- fsbrain_style_to_scimesh_options("edges");
    expect_true(isTRUE(opts_edges$wireframe));

    opts_custom <- fsbrain_style_to_scimesh_options("default", bg_rgba = c(0, 0, 0, 1), width = 100L, height = 50L);
    expect_equal(opts_custom$width, 100L);
    expect_equal(opts_custom$height, 50L);
    expect_equal(opts_custom$background_color, c(0, 0, 0, 1));
});


test_that("highlight_points_to_scimesh creates sphere meshes per point.", {
    testthat::skip_if_not_installed("scimesh");

    expect_equal(highlight_points_to_scimesh(list()), list());

    rglactions <- list("highlight_points" = list("coords" = rbind(c(0, 0, 0), c(1, 1, 1)),
                                                 "color" = "#FF0000", "radius" = 2,
                                                 "hemi" = c("lh", "lh")));
    expect_equal(length(highlight_points_to_scimesh(rglactions, "both")), 2L);
    expect_equal(length(highlight_points_to_scimesh(rglactions, "lh")), 2L);
    expect_equal(length(highlight_points_to_scimesh(rglactions, "rh")), 0L);

    # Defaults: a bare vector of coordinates gets the default color and radius.
    bare <- list("highlight_points" = list("coords" = c(0, 0, 0)));
    expect_equal(length(highlight_points_to_scimesh(bare, "both")), 1L);
});


test_that("coloredmeshes_to_scimesh handles hemilists, render flags, and unnamed lists.", {
    testthat::skip_if_not_installed("scimesh");
    mesh <- rgl::tetrahedron3d();
    cm_on <- structure(list(mesh = mesh, col = "#FF0000", render = TRUE), class = "fs.coloredmesh");
    cm_off <- structure(list(mesh = mesh, col = "#00FF00", render = FALSE), class = "fs.coloredmesh");

    # render=FALSE meshes are skipped.
    scene <- coloredmeshes_to_scimesh(list(lh = cm_off, rh = cm_on));
    expect_equal(names(scene), "rh");

    # Empty lists yield empty scenes.
    expect_equal(coloredmeshes_to_scimesh(list()), list());

    # A single coloredmesh is wrapped under 'single'.
    expect_equal(names(coloredmeshes_to_scimesh(cm_on)), "single");

    # Unnamed lists are flattened into a plain scene list.
    expect_equal(length(coloredmeshes_to_scimesh(list(cm_on, cm_on))), 2L);
});
