
test_that("Visualizing a single image for a group of subjects works for native space morph data.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subjects_list = c('subject1', 'subject2');
    output_img = tempfile(fileext = ".png");

    vis.group.morph.native(subjects_dir, subjects_list, 'thickness', output_img = output_img, num_per_row = 5L);
    vis.group.morph.native(subjects_dir, subjects_list, 'thickness', output_img = output_img, num_per_row = 1L);

    gd = group.morph.native(subjects_dir, subjects_list, 'thickness', hemi='both');
    vis.data.on.group.native(subjects_dir, subjects_list, gd, output_img = output_img);

    expect_true(file.exists(output_img));
    unlink(output_img); # cleanup
})


test_that("Visualizing a single image for a group of subjects works for standard space morph data.", {
    testthat::skip_on_cran(); # skip: leads to memory errors ('cannot allocate vector of size XX MB') on CRAN.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subjects_list = c('subject1', 'subject2');
    output_img = tempfile(fileext = ".png");

    vis.group.morph.standard(subjects_dir, subjects_list, 'thickness', output_img = output_img, num_per_row = 5L);
    vis.group.morph.standard(subjects_dir, subjects_list, 'thickness', output_img = output_img, num_per_row = 1L);

    gd = group.morph.standard(subjects_dir, subjects_list, 'thickness');
    if(dir.exists(file.path(subjects_dir, 'fsaverage'))) {
        vis.data.on.group.standard(subjects_dir, 'fsaverage', gd, output_img = output_img);
    }

    expect_true(file.exists(output_img));
    unlink(output_img); # cleanup
})


test_that("Visualizing a single image for a group of subjects works for atlas data.", {
    testthat::skip_on_cran(); # skip: leads to memory errors ('cannot allocate vector of size XX MB') on CRAN.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subjects_list = c('subject1', 'subject2');
    output_img = tempfile(fileext = ".png");

    vis.group.annot(subjects_dir, subjects_list, 'aparc', output_img = output_img);

    expect_true(file.exists(output_img));
    unlink(output_img); # cleanup
})
