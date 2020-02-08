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
    expect_equal(sum(is.na(data)), 0);   # No data was masked, i.e., this includes the medial wall data.
})


test_that("Loading of native space whole brain morph data on subject level, limited to the cortex, works", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    data = subject.morph.native(subjects_dir, "subject1", "thickness", "lh", cortex_only = TRUE);

    known_vertex_count_cortex_label = 140891;
    num_verts_subject1_lh = 149244;
    num_vertices_medial_wall = num_verts_subject1_lh - known_vertex_count_cortex_label;

    expect_equal(class(data), "numeric");
    expect_equal(length(data), num_verts_subject1_lh);
    expect_equal(sum(is.na(data)), num_vertices_medial_wall);
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


test_that("A plain or inverted new mask can be created for single hemisphere data", {
    labels = list(c(4,6), c(8,11, 4));

    expected_mask_inverted = c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE);
    expected_mask = !expected_mask_inverted;

    mask = mask.from.labeldata.for.hemi(labels, 11);
    expect_equal(mask, expected_mask);

    mask_inv = mask.from.labeldata.for.hemi(labels, 11, invert_labels=TRUE);
    expect_equal(mask_inv, expected_mask_inverted);
})


test_that("Creating a mask and specifying too few vertices fails", {
    labels = list(c(4,6), c(8,11, 4));
    expect_error(mask = mask.from.labeldata.for.hemi(labels, 9));   # 9 is bad if the highest index in the labels is 11
})


test_that("An existing mask can be modified by applying more labels", {
    labels = list(c(1,2), c(6,7));

    existing_mask = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE);

    edited_mask = mask.from.labeldata.for.hemi(labels, 10, existing_mask = existing_mask);
    expect_equal(edited_mask, c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE));
})


test_that("An existing mask can be modified by applying more inverted labels", {
    labels = list(c(1,2), c(6,7));

    existing_mask = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE);

    edited_mask = mask.from.labeldata.for.hemi(labels, 10, existing_mask = existing_mask, invert_labels=TRUE);
    expect_equal(edited_mask, c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE));
})


test_that("We can build a mask from an atlas region and edit it", {
    fsbrain::download_optional_data();

    # Define the data to use:
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';
    surface = 'white';
    hemi = 'both';
    atlas = 'aparc';
    region = 'bankssts';

    # Create a mask from a region of an annotation:
    lh_annot = subject.annot(subjects_dir, subject_id, 'lh', atlas);
    rh_annot = subject.annot(subjects_dir, subject_id, 'rh', atlas);
    lh_label = label.from.annotdata(lh_annot, region);
    rh_label = label.from.annotdata(rh_annot, region);
    lh_mask = mask.from.labeldata.for.hemi(lh_label, length(lh_annot$vertices));
    rh_mask = mask.from.labeldata.for.hemi(rh_label, length(rh_annot$vertices));

    # Edit the mask: add the vertices from another region to it:
    region2 = 'medialorbitofrontal';
    lh_label2 = label.from.annotdata(lh_annot, region2);
    rh_label2 = label.from.annotdata(rh_annot, region2);
    lh_mask2 = mask.from.labeldata.for.hemi(lh_label2, length(lh_annot$vertices), existing_mask = lh_mask);
    rh_mask2 = mask.from.labeldata.for.hemi(rh_label2, length(rh_annot$vertices), existing_mask = rh_mask);
    expect_equal(1L, 1L); # force to run the test (tests without any asserts are skipped)
})


test_that("We can compute the medial mask for a subject", {
    fsbrain::download_optional_data();

    # Define the data to use:
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';

    num_verts_subject1_lh = 149244;
    num_verts_subject1_rh = 153333;

    num_cortex_verts_subject1_lh = length(subject.label(subjects_dir, subject_id, "cortex", "lh"));
    num_cortex_verts_subject1_rh = length(subject.label(subjects_dir, subject_id, "cortex", "rh"));

    mask = subject.mask(subjects_dir, subject_id);
    expect_equal(length(mask$lh), num_verts_subject1_lh);
    expect_equal(length(mask$rh), num_verts_subject1_rh);

    expect_equal(sum(mask$lh), num_cortex_verts_subject1_lh); # number of cortex vertices
    expect_equal(sum(mask$rh), num_cortex_verts_subject1_rh); # number of cortex vertices

    # ## has been fixed there already, but will only be in the next release 0.1.8.
    # lh_mask_file = tempfile(fileext = ".mgz");
    # freesurferformats::write.fs.morph(lh_mask_file, as.integer(mask$lh));
    #
    # lh_mask_reread = freesurferformats::read.fs.morph(lh_mask_file);
    # expect_equal(length(lh_mask_reread), num_verts_subject1_lh);
    # expect_equal(sum(lh_mask_reread), num_cortex_verts_subject1_lh); # number of cortex vertices
})



