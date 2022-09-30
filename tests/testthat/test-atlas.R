test_that("A label can be extracted from a region of a loaded annotation", {
    testthat::skip_on_cran();
    testthat::skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    testthat::skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    num_verts_bankssts_subject1_lh = 1722;
    num_verts_subject1_lh = 149244;
    num_verts_subject1_lh_nonbankssts = num_verts_subject1_lh - num_verts_bankssts_subject1_lh;

    annotdata = subject.annot(subjects_dir, "subject1", "lh", "aparc");

    # Test retrieving label from a valid region
    label = label.from.annotdata(annotdata, "bankssts");
    testthat::expect_equal(length(label), num_verts_bankssts_subject1_lh);

    # Test retrieving an INVERTED label from a valid region
    label = label.from.annotdata(annotdata, "bankssts", invert=TRUE);
    testthat::expect_equal(length(label), num_verts_subject1_lh_nonbankssts);

    # Test retrieving label from an invalid region
    testthat::expect_error(label.from.annotdata(annotdata, "nosuchregioninatlas"));

    # Test retrieving label from an invalid region, allowing invalid regions
    label = label.from.annotdata(annotdata, "nosuchregioninatlas", error_on_invalid_region=FALSE)
    testthat::expect_equal(length(label), 0);    # The label should be empty
})


test_that("A label can be extracted from a region of an annotation file", {
    testthat::skip_on_cran();
    testthat::skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    testthat::skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    num_verts_bankssts_subject1_lh = 1722;

    label = subject.label.from.annot(subjects_dir, 'subject1', 'lh', 'aparc', 'bankssts');
    testthat::expect_equal(length(label), num_verts_bankssts_subject1_lh);
})


test_that("Labels can be extracted from a region of an annotation file for a group of subjects", {
    testthat::skip_on_cran();
    testthat::skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subjects_list = c('subject1', 'subject2');
    testthat::skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    num_verts_bankssts_subject1_lh = 1722;
    num_verts_bankssts_subject2_lh = 1722;    # subject2 is a copy of 1

    labels = group.label.from.annot(subjects_dir, subjects_list, 'lh', 'aparc', 'bankssts');
    testthat::expect_equal(length(labels), 2);
    testthat::expect_equal(length(labels$subject1), num_verts_bankssts_subject1_lh);
    testthat::expect_equal(length(labels$subject2), num_verts_bankssts_subject2_lh);
})


test_that("Labels can be merged into an annotation", {
    label1 = c(46666, 46777);
    label2 = c(99888, 99889);
    label_vertices = list("region1"=label1, "region2"=label2);
    colortable_df = data.frame("struct_index"=seq(0, 2), "struct_name"=c("unknown", "region1", "region2"), "r"=c(255L, 255L, 0L), "g"=c(255L, 0L, 255L), "b"=c(255L, 0L, 0L), "a"=c(0L, 0L, 0L));
    annot = label.to.annot(label_vertices, 100000, colortable_df);

    testthat::expect_equal(length(annot$vertices), 100000);
    testthat::expect_equal(length(annot$label_codes), 100000);
    testthat::expect_equal(length(annot$label_names), 100000);
    testthat::expect_equal(length(annot$hex_colors_rgb), 100000);

    testthat::expect_equal(nrow(annot$colortable_df), 3);
    testthat::expect_equal(nrow(annot$colortable$table), 3);
})
