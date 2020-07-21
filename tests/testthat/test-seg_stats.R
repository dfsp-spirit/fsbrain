

test_that("QC: outlier checks can be identified based on a data.frame with region-based values.", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");
    subjects_list = c('subject1', 'subject2');

    df = group.agg.atlas.native(subjects_dir, subjects_list, 'thickness', 'both', 'aparc');
    qc_res = qc.from.regionwise.df(df);
})


test_that("QC: outlier checks can be computed based on an atlas for both hemis.", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");
    subjects_list = c('subject1', 'subject2');

    qc_res_hl = qc.for.group(subjects_dir, subjects_list, 'thickness', 'aparc');
})
