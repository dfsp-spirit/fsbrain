test_that("Concatination of native space measures works", {
    subjects_dir = path.expand("~/data/tim_only")
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip on travis

    num_verts_subject1_lh = 149244;
    num_verts_subject1_rh = 153333;

    subjects_list = c("tim", "timcopy");
    measures = c("area", "thickness", "volume");
    mc = concat_measures_native(subjects_dir, subjects_list, measures, "lh");
    expect_equal(nrow(mc), num_verts_subject1_lh * 3 * 2);   # 3 measures times 2 subjects
    expect_equal(ncol(mc), 3);
})


test_that("Concatination of standard space measures works", {
    subjects_dir = path.expand("~/data/tim_only")
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip on travis

    num_verts_fsaverage_per_hemi = 163842;

    subjects_list = c("tim", "timcopy");
    measures = c("area", "thickness", "volume");
    mc = concat_measures_standard(subjects_dir, subjects_list, measures, "lh", fwhm="10");
    expect_equal(nrow(mc), num_verts_fsaverage_per_hemi * 3 * 2);  # 3 measures times 2 subjects
    expect_equal(ncol(mc), 3);
})
