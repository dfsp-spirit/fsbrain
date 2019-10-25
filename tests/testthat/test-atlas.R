test_that("A label can be extracted from a region of a loaded annotation", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    num_verts_bankssts_subject1_lh = 1722;
    num_verts_subject1_lh = 149244;
    num_verts_subject1_lh_nonbankssts = num_verts_subject1_lh - num_verts_bankssts_subject1_lh;

    annotdata = subject.annot(subjects_dir, "subject1", "lh", "aparc");

    # Test retrieving label from a valid region
    label = label.from.annotdata(annotdata, "bankssts");
    expect_equal(length(label), num_verts_bankssts_subject1_lh);

    # Test retrieving an INVERTED label from a valid region
    label = label.from.annotdata(annotdata, "bankssts", invert=TRUE);
    expect_equal(length(label), num_verts_subject1_lh_nonbankssts);

    # Test retrieving label from an invalid region
    expect_error(label.from.annotdata(annotdata, "nosuchregioninatlas"));

    # Test retrieving label from an invalid region, allowing invalid regions
    label = label.from.annotdata(annotdata, "nosuchregioninatlas", error_on_invalid_region=FALSE)
    expect_equal(length(label), 0);    # The label should be empty
})


test_that("A label can be extracted from a region of an annotation file", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    num_verts_bankssts_subject1_lh = 1722;

    label = subject.label.from.annot(subjects_dir, 'subject1', 'lh', 'aparc', 'bankssts');
    expect_equal(length(label), num_verts_bankssts_subject1_lh);
})


test_that("Labels can be extracted from a region of an annotation file for a group of subjects", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subjects_list = c('subject1', 'subject2');
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    num_verts_bankssts_subject1_lh = 1722;
    num_verts_bankssts_subject2_lh = 1722;    # subject2 is a copy of 1

    labels = group.label.from.annot(subjects_dir, subjects_list, 'lh', 'aparc', 'bankssts');
    expect_equal(length(labels), 2);
    expect_equal(length(labels$subject1), num_verts_bankssts_subject1_lh);
    expect_equal(length(labels$subject2), num_verts_bankssts_subject2_lh);
})
