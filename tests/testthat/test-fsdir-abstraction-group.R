test_that("Loading of native space whole brain morph data on group level works", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
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

    expect_equal(sum(is.na(data$subject1)), 0);   # medial wall is not masked
    expect_equal(sum(is.na(data$subject2)), 0);
})


test_that("Loading of native space whole brain morph data restricted to the cortex on group level works", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subjects_list = c("subject1", "subject2");
    data = group.morph.native(subjects_dir, subjects_list, "thickness", "lh", cortex_only=TRUE);

    num_verts_subject1_lh = 149244;
    known_vertex_count_cortex_label = 140891;
    num_vertices_medial_wall = num_verts_subject1_lh - known_vertex_count_cortex_label;
    num_verts_subject2_lh = 149244;   # subject2 is a copy of subject1

    expect_equal(class(data), "list");
    expect_equal(length(data), 2);
    expect_equal(names(data)[1], "subject1");
    expect_equal(names(data)[2], "subject2");

    expect_equal(class(data$subject1), "numeric");
    expect_equal(class(data$subject2), "numeric");
    expect_equal(length(data$subject1), num_verts_subject1_lh);
    expect_equal(length(data$subject2), num_verts_subject2_lh);

    expect_equal(sum(is.na(data$subject1)), num_vertices_medial_wall);  # medial wall is masked
    expect_equal(sum(is.na(data$subject2)), num_vertices_medial_wall);
})


test_that("Loading of standard space whole brain morph data on group level works", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subjects_list = c("subject1", "subject2");
    data = group.morph.standard(subjects_dir, subjects_list, "thickness", "lh", fwhm='10');

    num_verts_fsaverage = 163842;

    expect_equal(class(data), "list");
    expect_equal(length(data), 2);
    expect_equal(names(data)[1], "subject1");
    expect_equal(names(data)[2], "subject2");

    expect_equal(class(data$subject1), "numeric");
    expect_equal(class(data$subject2), "numeric");
    expect_equal(length(data$subject1), num_verts_fsaverage);
    expect_equal(length(data$subject2), num_verts_fsaverage);
})


test_that("Label data can be read on group level", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");
    subjects_list = c("subject1", "subject2");

    grouplabel = group.label(subjects_dir, subjects_list, "cortex.label", hemi='lh');
    known_vertex_count_label = 140891

    # Test that the number of entries is correct, and that metadata matches data
    expect_equal(length(grouplabel), 2);
    expect_true(is.list(grouplabel));
    expect_equal(class(grouplabel$subject1), "integer");
    expect_equal(class(grouplabel$subject2), "integer");
    expect_equal(length(grouplabel$subject1), known_vertex_count_label);
    expect_equal(length(grouplabel$subject2), known_vertex_count_label);
})

test_that("Annotation data can be read on group level", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");
    subjects_list = c("subject1", "subject2");

    groupannot = group.annot(subjects_dir, subjects_list, 'lh', 'aparc');
    num_verts_subject1_lh = 149244;
    num_verts_subject2_lh = 149244;   # its a copy of 1

    # Test that the number of entries is correct, and that metadata matches data
    expect_equal(length(groupannot), 2);
    expect_true(is.list(groupannot));

    expect_equal(class(groupannot$subject1$vertices), "integer");
    expect_equal(class(groupannot$subject1$label_names), "character");
    expect_equal(class(groupannot$subject1$label_codes), "integer");
    expect_equal(length(groupannot$subject1$vertices), num_verts_subject1_lh);
    expect_equal(length(groupannot$subject1$label_codes), num_verts_subject1_lh);
    expect_equal(length(groupannot$subject1$label_names), num_verts_subject1_lh);

    expect_equal(class(groupannot$subject2$vertices), "integer");
    expect_equal(class(groupannot$subject2$label_names), "character");
    expect_equal(class(groupannot$subject2$label_codes), "integer");
    expect_equal(length(groupannot$subject2$vertices), num_verts_subject2_lh);
    expect_equal(length(groupannot$subject2$label_codes), num_verts_subject2_lh);
    expect_equal(length(groupannot$subject2$label_names), num_verts_subject2_lh);
})


