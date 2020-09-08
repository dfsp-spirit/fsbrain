

test_that("A coloredmesh can be created from native space morph data", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    cm = coloredmesh.from.morph.native(subjects_dir, 'subject1', 'thickness', 'lh');

    # errors
    expect_error(coloredmesh.from.morph.native(subjects_dir, 'subject1', 'thickness', 'nosuchhemi')); # invalid hemi
})


test_that("A coloredmesh can be created from color data", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    cm = coloredmeshes.from.color(subjects_dir, 'subject1', color_data="blue", hemi='lh');

    # errors
    expect_error(cm = coloredmeshes.from.color(subjects_dir, 'subject1', color_data="blue", hemi='nosuchhemi')); # invalid hemi
    expect_error(cm = coloredmeshes.from.color(subjects_dir, 'subject1', color_data=c("blue", 'red'), hemi='nosuchhemi')); # number of colors not 1 and not number of verts
})


test_that("A coloredmesh can be created from standard space morph data", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    fsbrain::download_fsaverage3(accept_freesurfer_license = TRUE);
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    cm = coloredmesh.from.morph.standard(subjects_dir, 'subject1', 'thickness', hemi='lh', fwhm='10');

    # Test with a custom template subject.
    cm_fsaverage3 = coloredmesh.from.morph.standard(subjects_dir, 'subject1', 'thickness', hemi='lh', fwhm='0', template_subject = 'fsaverage3');

    # errors
    expect_error(cm = coloredmesh.from.morph.standard(subjects_dir, 'subject1', 'thickness', hemi='nosuchhemi', fwhm='10')); # invalid hemi
})


test_that("A coloredmesh can be created from arbitrary pre-loaded data", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    num_verts_subject1_lh = 149244L;
    morph_data = rnorm(num_verts_subject1_lh, 3.0, 1.0);
    cm = coloredmesh.from.morphdata(subjects_dir, 'subject1', morph_data=morph_data, hemi='lh');

    # errors and warnings
    expect_error(coloredmesh.from.morphdata(subjects_dir, 'subject1', morph_data = morph_data, hemi='nosuchhemi')); # invalid hemi
    morph_data_broken = rnorm((num_verts_subject1_lh + 300L), 3.0, 1.0);
    expect_warning(coloredmesh.from.morphdata(subjects_dir, 'subject1', morph_data = morph_data_broken, hemi='lh')); # morph data count does not match
})


test_that("A coloredmesh can be created from a label", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");


    label = c(1L, 24L, 100L, 40000L);
    cm = coloredmesh.from.label(subjects_dir, 'subject1', label = label, hemi='lh');

    # errors and warnings
    expect_error(coloredmesh.from.label(subjects_dir, 'subject1', label = label, hemi='nosuchhemi')); # invalid hemi
})
