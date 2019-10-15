test_that("Loading of native space whole brain morph data on subject level works", {
    nitools::download_optional_data();
    subjects_dir = nitools::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    data = subject.morph.native(subjects_dir, "subject1", "thickness", "lh");

    num_verts_subject1_lh = 149244;
    expect_equal(class(data), "numeric");
    expect_equal(length(data), num_verts_subject1_lh);
})


test_that("Loading of native space whole brain morph data on subject level works for both hemis", {
    nitools::download_optional_data();
    subjects_dir = nitools::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    data = subject.morph.native(subjects_dir, "subject1", "thickness", "both");

    num_verts_subject1_lh = 149244;
    num_verts_subject1_rh = 153333;
    expect_equal(class(data), "numeric");
    expect_equal(length(data), num_verts_subject1_lh + num_verts_subject1_rh);
})


test_that("Standard space morphometry data can be read on subject level", {
    nitools::download_optional_data();
    subjects_dir = nitools::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    data = subject.morph.standard(subjects_dir, "subject1", "thickness", "lh", fwhm='10', template_subject='fsaverage');

    num_verts_fsaverage = 163842
    expect_equal(class(data), "numeric")
    expect_equal(length(data), num_verts_fsaverage)
})


test_that("Standard space morphometry data can be read on subject level for both hemis", {
    nitools::download_optional_data();
    subjects_dir = nitools::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    data = subject.morph.standard(subjects_dir, "subject1", "thickness", "both", fwhm='10', template_subject='fsaverage');

    num_verts_fsaverage = 163842
    num_verts_fsaverage_both_hemis = num_verts_fsaverage * 2
    expect_equal(class(data), "numeric")
    expect_equal(length(data), num_verts_fsaverage_both_hemis)
})
