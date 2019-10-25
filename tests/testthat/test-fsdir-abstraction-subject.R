test_that("Loading of native space whole brain morph data on subject level works", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    data = subject.morph.native(subjects_dir, "subject1", "thickness", "lh");

    num_verts_subject1_lh = 149244;
    expect_equal(class(data), "numeric");
    expect_equal(length(data), num_verts_subject1_lh);
})


test_that("Loading of native space whole brain morph data on subject level works for both hemis", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    data = subject.morph.native(subjects_dir, "subject1", "thickness", "both");

    num_verts_subject1_lh = 149244;
    num_verts_subject1_rh = 153333;
    expect_equal(class(data), "numeric");
    expect_equal(length(data), num_verts_subject1_lh + num_verts_subject1_rh);
})


test_that("Standard space morphometry data can be read on subject level", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    data = subject.morph.standard(subjects_dir, "subject1", "thickness", "lh", fwhm='10', template_subject='fsaverage');

    num_verts_fsaverage = 163842;
    expect_equal(class(data), "numeric");
    expect_equal(length(data), num_verts_fsaverage);
})


test_that("Standard space morphometry data can be read on subject level for both hemis", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    data = subject.morph.standard(subjects_dir, "subject1", "thickness", "both", fwhm='10', template_subject='fsaverage');

    num_verts_fsaverage = 163842;
    num_verts_fsaverage_both_hemis = num_verts_fsaverage * 2;
    expect_equal(class(data), "numeric");
    expect_equal(length(data), num_verts_fsaverage_both_hemis);
})


test_that("Label data can be read on subject level", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    label = subject.label(subjects_dir, "subject1", "cortex.label", hemi='lh');

    known_vertex_count_label = 140891;

    # Test that the number of entries is correct, and that metadata matches data
    expect_equal(length(label), known_vertex_count_label);
    expect_equal(class(label), "integer");
    expect_true(is.vector(label));
})


test_that("Surface data can be read on subject level", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    num_verts_subject1_lh = 149244;
    known_face_count = 298484;

    surface = subject.surface(subjects_dir, "subject1", "white", "lh");

    expect_equal(nrow(surface$vertices), num_verts_subject1_lh);
    expect_equal(nrow(surface$faces), known_face_count);
})


