
test_that("Hemis can be shifted apart using rglactions for non-overlapping rendering.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    vis.subject.morph.native(subjects_dir, subject_id, 'thickness', rglactions = list('shift_hemis_apart'=TRUE));
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

