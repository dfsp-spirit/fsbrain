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


test_that("apply.transform rotates matrices and mesh3d objects (incl. normals).", {
    M <- rotation.matrix(pi / 2, 0, 0, 1);
    pts <- rbind(c(1, 2, 3), c(4, 5, 6), c(0, 0, 1));
    expected <- t(t(M[1:3, 1:3] %*% t(pts)) + M[1:3, 4]);
    expect_equal(transform.renderable(pts, M), expected, tolerance = 1e-12);

    m <- rgl::tetrahedron3d();
    m$normals <- m$vb;
    m$normals[4, ] <- 0;
    rot <- transform.renderable(m, M);
    expect_equal(rot$vb, M %*% m$vb, tolerance = 1e-12);
    expect_equal(rot$normals[1:3, ], M[1:3, 1:3] %*% m$normals[1:3, ], tolerance = 1e-12);
});


test_that("color_to_rgba converts hex and named colors.", {
    expect_equal(color_to_rgba("#FF0000"), c(1, 0, 0, 1));
    expect_equal(color_to_rgba("#FF0000FF"), c(1, 0, 0, 1));
    expect_equal(color_to_rgba("white"), c(1, 1, 1, 1));
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
