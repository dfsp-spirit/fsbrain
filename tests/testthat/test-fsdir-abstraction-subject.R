test_that("Loading of native space whole brain morph data on subject level works", {
    subjects_dir = path.expand("~/data/tim_only")
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip on travis

    data = subject.morph.native(subjects_dir, "tim", "thickness", "lh");

    num_verts_subject1_lh = 149244;
    expect_equal(class(data), "numeric");
    expect_equal(length(data), num_verts_subject1_lh);
})


test_that("Loading of native space whole brain morph data on subject level works for both hemis", {
    subjects_dir = path.expand("~/data/tim_only")
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip on travis

    data = subject.morph.native(subjects_dir, "tim", "thickness", "both");

    num_verts_subject1_lh = 149244;
    num_verts_subject1_rh = 153333;
    expect_equal(class(data), "numeric");
    expect_equal(length(data), num_verts_subject1_lh + num_verts_subject1_rh);
})


test_that("Standard space morphometry data can be read on subject level", {
    subjects_dir = path.expand("~/data/tim_only")
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip on travis

    data = subject.morph.standard(subjects_dir, "tim", "thickness", "lh", fwhm='10', template_subject='fsaverage');

    num_verts_fsaverage = 163842
    expect_equal(class(data), "numeric")
    expect_equal(length(data), num_verts_fsaverage)
})


test_that("Standard space morphometry data can be read on subject level for both hemis", {
    subjects_dir = path.expand("~/data/tim_only")
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip on travis

    data = subject.morph.standard(subjects_dir, "tim", "thickness", "both", fwhm='10', template_subject='fsaverage');

    num_verts_fsaverage = 163842
    num_verts_fsaverage_both_hemis = num_verts_fsaverage * 2
    expect_equal(class(data), "numeric")
    expect_equal(length(data), num_verts_fsaverage_both_hemis)
})
