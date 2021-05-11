test_that("Concatination of native space measures works for a single hemisphere", {
    testthat::skip_on_cran();
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip on travis

    num_verts_subject1_lh = 149244;
    num_verts_subject1_rh = 153333;

    subjects_list = c("subject1", "subject2");
    measures = c("area", "thickness");
    mc = group.concat.measures.native(subjects_dir, subjects_list, measures, "lh");
    expect_equal(nrow(mc), num_verts_subject1_lh * 2 * 1);   # 2 subjects with 1 hemi each
    expect_equal(ncol(mc), 2);  # 2 measures
    expect_true("area" %in% colnames(mc));
    expect_true("thickness" %in% colnames(mc));
})

test_that("Concatination of native space measures works for both hemispheres", {
    testthat::skip_on_cran();
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip on travis

    num_verts_subject1_lh = 149244;
    num_verts_subject1_rh = 153333;
    num_verts_subject1_both = num_verts_subject1_lh + num_verts_subject1_rh;

    subjects_list = c("subject1", "subject2");
    measures = c("area", "thickness");
    mc = group.concat.measures.native(subjects_dir, subjects_list, measures, "both");
    expect_equal(nrow(mc), num_verts_subject1_both * 2);   # 2 subjects with 1 hemi each
    expect_equal(ncol(mc), 2);  # 2 measures
    expect_true("area" %in% colnames(mc));
    expect_true("thickness" %in% colnames(mc));
})


test_that("Concatination of standard space measures works for a single hemisphere", {
    testthat::skip_on_cran();
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip on travis

    num_verts_fsaverage_per_hemi = 163842;

    subjects_list = c("subject1", "subject2");
    measures = c("area", "thickness");
    mc = group.concat.measures.standard(subjects_dir, subjects_list, measures, "lh", fwhm="10");
    expect_equal(nrow(mc), num_verts_fsaverage_per_hemi * 2 * 1);  # 2 subjects with 1 hemi each
    expect_equal(ncol(mc), 2); # 2 measures
    expect_true("thickness_fwhm10" %in% colnames(mc));
})

test_that("Concatination of standard space measures works for both hemispheres", {
    testthat::skip_on_cran();
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip on travis

    num_verts_fsaverage_per_hemi = 163842;

    subjects_list = c("subject1", "subject2");
    measures = c("area", "thickness");
    mc = group.concat.measures.standard(subjects_dir, subjects_list, measures, "both", fwhm="10");
    expect_equal(nrow(mc), num_verts_fsaverage_per_hemi * 2 * 2);  # 2 subjects with 2 hemis each
    expect_equal(ncol(mc), 2); # 2 measures
    expect_true("area_fwhm10" %in% colnames(mc));
    expect_true("thickness_fwhm10" %in% colnames(mc));
})
