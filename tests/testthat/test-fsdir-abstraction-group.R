test_that("Loading of native space whole brain morph data on group level works", {
    nitools::download_optional_data();
    subjects_dir = nitools::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subjects_list = c("subject1", "subject2");
    data = group.morph.native(subjects_dir, subjects_list, "thickness", "lh");

    num_verts_subject1_lh = 149244;
    num_verts_subject2_lh = 149244;   # subject2 is a copy of subject1

    expect_equal(class(data), "list");
    expect_equal(length(data), 2);
    expect_equal(names(data)[1], "subject1");
    expect_equal(names(data)[2], "subject2");

    expect_equal(class(data$subject1), "numeric");
    expect_equal(class(data$subject2), "numeric");
    expect_equal(length(data$subject1), num_verts_subject1_lh);
    expect_equal(length(data$subject2), num_verts_subject2_lh);
})
