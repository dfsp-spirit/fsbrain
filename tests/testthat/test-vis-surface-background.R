# Tests for surface color layers.

test_that("A mean curvature color layer can be loaded", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    bgcol_lh = background.mean.curvature(subjects_dir, "subject1", "lh");
    bgcol_rh = background.mean.curvature(subjects_dir, "subject1", "rh");

    num_verts_subject1_lh = 149244;
    num_verts_subject1_rh = 153333;
    expect_equal(length(bgcol_lh), num_verts_subject1_lh);
    expect_equal(length(bgcol_rh), num_verts_subject1_rh);

    vis.color.on.subject(subjects_dir, 'subject1', bgcol_lh, bgcol_rh);
})

