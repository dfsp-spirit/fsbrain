
test_that("The geodesic neighborhood of a vertex can be computed", {
    testthat::skip_on_cran();

    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'fsaverage';

    lh_surf = subject.surface(subjects_dir, subject_id, surface = "white", hemi = "lh");
    source_vertex = 32258;  # on precentral gyrus / central sulcus wall
    neighbors = geod.vert.neighborhood(lh_surf, source_vertex, max_distance = 5.0)$vertices;

    testthat::expect_equal(length(neighbors), 130L);

    # Visualize neighborhood (red area at precentral gyrus / central sulcus, left hemi).
    highlight.vertices.on.subject(subjects_dir, subject_id, verts_lh = neighbors, views = "si");
})


test_that("The geodesic color overlay for several vertices over a full brain can be computed", {
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
    vis.color.on.subject(subjects_dir, subject_id, color_both = overlay_hemilist, views = "si", surface = "white");
    highlight.vertices.spheres(surfaces, source_verts);

    testthat::expect_equal(1L, 1L); # only prevent test skipping for now.
})


test_that("The geodesic per-vertex distance data for several vertices over a full brain can be computed", {
    testthat::skip_on_cran();

    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'fsaverage';

    surfaces = subject.surface(subjects_dir, subject_id, surface = "white", hemi = "both");
    source_verts = c(500, 32258, 150000, 250000, 320000);

    morph_data = geod.patches.pervertexdata(surfaces, source_verts, max_distance = 25.0);


    # Visualize
    vis.data.on.subject(subjects_dir, subject_id, morph_data_lh = morph_data$lh, morph_data_rh = morph_data$rh);
    highlight.vertices.spheres(surfaces, source_verts);

    testthat::expect_equal(1L, 1L); # only prevent test skipping for now.
})


test_that("We can render publication-ready vertex highlight figures with geodesic per-vertex distance morph data.", {
    testthat::skip_on_cran();

    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'fsaverage';

    surfaces = subject.surface(subjects_dir, subject_id, surface = "white", hemi = "both");
    source_verts = c(500, 32258, 150000, 250000, 320000);

    morph_data = geod.patches.pervertexdata(surfaces, source_verts, max_distance = 25.0);

    coords = vertex.coords(surfaces, source_verts);
    point_hemi = vertex.hemis(surfaces, source_verts); # compute the hemispheres for the vertices/points.
    sphere_colors = c('#FF0000'); # One can also pass a vector with one color per source_vert if different colors are needed.
    rglactions = list('highlight_points'=list('coords'=coords, 'color'=sphere_colors, 'radius'=3, 'hemi'=point_hemi));

    # Visualize
    cm = vis.data.on.subject(subjects_dir, subject_id, morph_data_lh = morph_data$lh, morph_data_rh = morph_data$rh, rglactions = rglactions, style = "glass2");
    export(cm, rglactions = rglactions, style = "glass2", horizontal = NULL);

    testthat::expect_equal(1L, 1L); # only prevent test skipping for now.
})


