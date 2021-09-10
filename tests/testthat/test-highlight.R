

test_that("We can highlight vertices using 3D spheres using a single surface.", {
    testthat::skip_on_cran();

    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'fsaverage';

    surface = subject.surface(subjects_dir, subject_id, surface = "white", hemi = "lh");
    source_verts = c(500, 32258, 150000);
    vertex_colors = c("#FF0000", "#00FF00", "#0000FF");

    vis.color.on.subject(subjects_dir, subject_id, color_both = "#FFFFFF", views = "si");
    highlight.vertices.spheres(surface, source_verts, color = vertex_colors, radius = 5);

    testthat::expect_equal(1L, 1L); # only prevent test skipping for now.
})


test_that("We can highlight vertices using geodesic single-color patches and 3D spheres using a hemilist of surfaces.", {

    ## Note: This approach uses two separate function calls for overlay visualization and the spheres,
    ##       which only works in "si" view. One can now use the 'highlight_points' rlgaction, which is
    ##       way more flexible and works with several views and the export function.

    testthat::skip_on_cran();

    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'fsaverage';

    surfaces = subject.surface(subjects_dir, subject_id, surface = "white", hemi = "both");
    source_verts = c(500, 32258, 150000, 250000, 320000);
    patch_colors = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF");

    overlay_hemilist = geod.patches.color.overlay(surfaces, vertex = source_verts, color = patch_colors);

    # Visualize
    vis.color.on.subject(subjects_dir, subject_id, color_both = overlay_hemilist, views = "si", surface = "white", style = "default");
    highlight.vertices.spheres(surfaces, source_verts, color = patch_colors, radius = 4);

    testthat::expect_equal(1L, 1L); # only prevent test skipping for now.
})


test_that("We can highlight vertices using geodesic distance morphdata patches.", {

    ## Note: This approach uses morphdata (the distance to sthe respective source vertex, or NA for all verts outside the patches)
    ##       instead of a color overlay with pre-defined per-vertex-patch colors for visualization. Thus, it gives you access to
    ##       and visualizes the distances themselves.

    testthat::skip_on_cran();

    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'fsaverage';

    surfaces = subject.surface(subjects_dir, subject_id, surface = "white", hemi = "both");
    source_verts = c(500, 32258, 150000, 250000, 320000);
    patch_colors = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF");

    overlay_hemilist = geod.patches.color.overlay(surfaces, vertex = source_verts, color = patch_colors);

    # Visualize
    vis.color.on.subject(subjects_dir, subject_id, color_both = overlay_hemilist, views = "si", surface = "white", style = "default");

    testthat::expect_equal(1L, 1L); # only prevent test skipping for now.
})


test_that("We can highlight vertices using the high-level function for spheres and patches.", {

    testthat::skip_on_cran();

    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'fsaverage';

    vertices = c(500, 32258, 150000, 250000, 320000);
    highlight.vertices.on.subject.spheres(subjects_dir, subject_id, vertices = vertices);

    testthat::expect_equal(1L, 1L); # only prevent test skipping for now.
})


test_that("We can highlight directly adjacent vertices using the high-level function for spheres and patches.", {

    testthat::skip_on_cran();

    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);

    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'fsaverage';

    vertices = c(32258, 171); # These are directly adjacent
    highlight.vertices.on.subject.spheres(subjects_dir, subject_id, vertices = vertices);

    testthat::expect_equal(1L, 1L); # only prevent test skipping for now.
})


test_that("We can compute the hemisphere string for vertices given an integer.", {
    # test in order with integer
    hemi_strings = vertex.hemis(100L, vertices = c(50, 1, 99, 100, 101, 300));
    expected = c("lh", "lh", "lh", "lh", "rh", "rh");
    testthat::expect_equal(hemi_strings, expected);

    # test unordered with integer
    hemi_strings = vertex.hemis(100L, vertices = c(300, 50, 101, 99, 100, 1, 1000));
    expected = c("rh", "lh", "rh", "lh", "lh", "lh", "rh");
    testthat::expect_equal(hemi_strings, expected);
})


test_that("We can compute the hemisphere string for vertices given the surfaces.", {
    testthat::skip_on_cran();
    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);

    surfaces = subject.surface(fsaverage.path(), "fsaverage");

    hemi_strings = vertex.hemis(surfaces, vertices = c(50, 1, 300000, 163842, 163843));
    expected = c("lh", "lh", "rh", "lh", "rh");
    testthat::expect_equal(hemi_strings, expected);
})


test_that("We can compute vertex coordinates over a single hemisphere.", {
    testthat::skip_on_cran();
    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);

    surfaces = subject.surface(fsaverage.path(), "fsaverage");

    # Check that return value is always matrix, even for single coords.
    testthat::expect_true(is.matrix(vertex.coords(surfaces$lh, vertices = 1L)));
    testthat::expect_true(is.matrix(vertex.coords(surfaces$rh, vertices = c(1L, 3L))));

    # Check correct coord values for single hemi: lh
    testthat::expect_equal(vertex.coords(surfaces$lh, vertices = 1L), matrix(surfaces$lh$vertices[1L, ], ncol = 3, byrow = TRUE));
    testthat::expect_equal(vertex.coords(surfaces$lh, vertices = c(1L, 50L)), surfaces$lh$vertices[c(1L,50L), ]);

    # Check correct coord values for single hemi: rh
    testthat::expect_equal(vertex.coords(surfaces$rh, vertices = 1L), matrix(surfaces$rh$vertices[1L, ], ncol = 3, byrow = TRUE));
    testthat::expect_equal(vertex.coords(surfaces$rh, vertices = c(1L, 50L)), surfaces$rh$vertices[c(1L,50L), ]);

    # Check correct coord values for single hemi: rh, but unordered
    testthat::expect_equal(vertex.coords(surfaces$rh, vertices = c(50L, 1L)), surfaces$rh$vertices[c(50L, 1L), ]);

    # Out of bounds indices lead to warning (and filtering)
    testthat::expect_error(vertex.coords(surfaces$rh, vertices = c(50L, 500000L)));
})


test_that("We can compute vertex coordinates over both hemispheres.", {
    testthat::skip_on_cran();
    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);

    surfaces = subject.surface(fsaverage.path(), "fsaverage");
    nv_lh = 163842L;

    # Check that return value is always matrix, even for single coords.
    testthat::expect_true(is.matrix(vertex.coords(surfaces, vertices = 1L)));
    testthat::expect_true(is.matrix(vertex.coords(surfaces, vertices = c(1L, 3L))));

    # Check correct coord values for single hemi: lh
    testthat::expect_equal(vertex.coords(surfaces$lh, vertices = 1L), matrix(surfaces$lh$vertices[1L, ], ncol = 3, byrow = TRUE));
    testthat::expect_equal(vertex.coords(surfaces$lh, vertices = c(1L, 50L)), surfaces$lh$vertices[c(1L,50L), ]);
    # ... and that these are the same when passing a hemilist:
    testthat::expect_equal(vertex.coords(surfaces, vertices = 1L), matrix(surfaces$lh$vertices[1L, ], ncol = 3, byrow = TRUE));
    testthat::expect_equal(vertex.coords(surfaces, vertices = c(1L, 50L)), surfaces$lh$vertices[c(1L,50L), ]);

    # Check correct coord values for the right hemi when passing a hemilist
    testthat::expect_equal(vertex.coords(surfaces, vertices = (1L+nv_lh)), matrix(surfaces$rh$vertices[1L, ], ncol = 3, byrow = TRUE));
    testthat::expect_equal(vertex.coords(surfaces, vertices = c((1L+nv_lh), (50L+nv_lh))), surfaces$rh$vertices[c(1L,50L), ]);

    # Check correct coord values for single hemi: rh, but unordered
    testthat::expect_equal(vertex.coords(surfaces, vertices = c((50L+nv_lh), (1L+nv_lh))), surfaces$rh$vertices[c(50L, 1L), ]);

    # Out of bounds indices lead to error
    testthat::expect_error(vertex.coords(surfaces, vertices = c(50L, 500000L)));
})


testthat::test_that("Vertex coordinate computation returns the results in the input order", {
    testthat::skip_on_cran();
    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    surfaces = subject.surface(fsaverage.path(), "fsaverage");

    vertices = c(1L, 50L, 300000L, 500L);
    vertices_ordered = sort(vertices); # c(1L, 50L, 500L, 300000L);

    # The coords must be in the input order, i.e., they should differ if passed in different orders.
    testthat::expect_equal(vertex.coords(surfaces, vertices)[1,], vertex.coords(surfaces, vertices_ordered)[1,]);
    testthat::expect_equal(vertex.coords(surfaces, vertices)[2,], vertex.coords(surfaces, vertices_ordered)[2,]);
    testthat::expect_equal(vertex.coords(surfaces, vertices)[3,], vertex.coords(surfaces, vertices_ordered)[4,]);
})


testthat::test_that("Per hemi vertex indices can be computed using a hemilist of surfaces", {
    testthat::skip_on_cran();
    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    surfaces = subject.surface(fsaverage.path(), "fsaverage");

    vertices = c(1L, 50L, 300000L, 500L, 163843L, 163842L);
    per_hemi = fsbrain:::per.hemi.vertex.indices(surfaces, vertices);

    # Check proper length of the return values: the lh and rh entries must sum to the query vertex count.
    testthat::expect_equal(sum(length(per_hemi$vertices$lh), length(per_hemi$vertices$rh)), length(vertices));
    testthat::expect_equal(sum(length(per_hemi$query_indices$lh), length(per_hemi$query_indices$rh)), length(vertices));

    testthat::expect_equal(per_hemi$query_indices$lh, c(1, 2, 4, 6));
    testthat::expect_equal(per_hemi$query_indices$rh, c(3, 5));

    nv_lh = 163842L;

    testthat::expect_equal(per_hemi$vertices$lh, c(1, 50, 500, 163842));
    testthat::expect_equal(per_hemi$vertices$rh, (c(300000, 163843)-nv_lh));

    testthat::expect_equal(per_hemi$vertices_hemi, c("lh", "lh", "rh", "lh", "rh", "lh"));

})




