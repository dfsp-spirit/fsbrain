test_that("A volume for a single subject can be loaded", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    brain = subject.volume(subjects_dir, subject_id, 'brain');

    expect_equal(dim(brain), c(256, 256, 256));
})
