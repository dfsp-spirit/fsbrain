
test_that("The geodesic neighborhood of a vertex can be computed", {
    testthat::skip_on_cran();

    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'fsaverage';

    lh_surf = subject.surface(subjects_dir, subject_id, surface = "white", hemi = "lh");
    source_vertex = 32258;  # on precentral gyrus / central sulcus wall
    neighbors = geod.vert.neighborhood(lh_surf, source_vertex, max_distance = 5.0);

    testthat::expect_equal(length(neighbors), 130L);

    # Visualize neighborhood (red area at precentral gyrus / central sulcus, left hemi).
    highlight.vertices.on.subject(subjects_dir, subject_id, verts_lh = neighbors, views = "si", k = 0L);
})


test_that("The geodesic color overlay for several vertices over a full brain can be computed", {
    testthat::skip_on_cran();

    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'fsaverage';

    lh_surf = subject.surface(subjects_dir, subject_id, surface = "white", hemi = "lh");
    rh_surf = subject.surface(subjects_dir, subject_id, surface = "white", hemi = "rh");
    surf_hemilist = list('lh'=lh_surf, 'rh'=rh_surf);
    source_verts = c(500, 32258, 150000, 250000, 320000);
    patch_colors = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF");

    overlay_hemilist = geod.patches.color.overlay(surf_hemilist, vertex = source_verts, color = patch_colors);

    # Visualize
    vis.color.on.subject(subjects_dir, subject_id, color_both = overlay_hemilist, views = "si", surface = "white");

    testthat::expect_equal(1L, 1L); # only prevent test skipping for now.
})


geod.patches.color.overlay
