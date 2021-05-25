

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

