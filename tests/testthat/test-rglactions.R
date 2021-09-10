
test_that("Hemis can be shifted apart using rglactions for non-overlapping rendering.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    vis.subject.morph.native(subjects_dir, subject_id, 'thickness', rglactions = list('shift_hemis_apart'=TRUE));
    expect_equal(1L , 1L);
})


test_that("Points can be highlighted with 3D spheres.", {
    testthat::skip_on_cran();

    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    testthat::skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "fsaverage";

    surfaces = subject.surface(subjects_dir, subject_id, surface = "white", hemi = "both");
    vertices = c(50L, 70000L, 150000L, 300000L);
    point_coords = vertex.coords(surfaces, vertices);
    point_hemi = vertex.hemis(surfaces, vertices);
    colors = c('#FFFF00', '#FFFF00', '#FFFF00', '#FF0000');
    rglactions = list('highlight_points'=list('coords'=point_coords, 'color'=colors, 'radius'=5, 'hemi'=point_hemi));
    cm = vis.subject.morph.native(subjects_dir, subject_id, 'curv', rglactions = rglactions, views = "si");
    #export(cm);
    expect_equal(1L , 1L);
})


test_that("The limit function can be created.", {
    lf = limit_fun(2, 3);
    data = c(1.0,2.5,2.9, 3.5);
    data_lim = lf(data);
    expect_equal(length(data), length(data_lim));
})


test_that("The limit_na function can be created.", {
    lf = limit_fun_na(2, 3);
    data = c(1.0,2.5,2.9, 3.5);
    data_lim = lf(data);
    expect_equal(length(data), length(data_lim));
})


test_that("The demo rglactions list for screenshot can be created", {
    rgla = rglactions();
    testthat::expect_true(is.list(rgla));
})

