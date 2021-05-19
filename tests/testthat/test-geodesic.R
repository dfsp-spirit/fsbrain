
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
