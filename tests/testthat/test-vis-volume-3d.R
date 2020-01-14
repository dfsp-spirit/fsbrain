

test_that("A brain volume and an overlay can be merged", {

    skip("This test has to be run manually and interactively. It requires an aseg.mgz file for the demo subject.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    aseg = subject.volume(subjects_dir, subject_id, 'aseg');    # Not shipped with the package atm.
    ventricle_aseg_codes = c(4, 14, 15, 43);    # see FreeSurferColorLUT.txt
    ventricle_mask = vol.mask.from.segmentation(aseg, ventricle_aseg_codes);

    volvis.voxels(ventricle_mask);

    # cmm = matrix(c(0, 0, 0, 0, 0, 255, 0, 255, 255, 0, 255, 0, 255, 255, 255, 255, 0, 0, 255, 255, 0, 255, 0, 255), ncol=3)
    # rgl::shade3d(rgl::cube3d(vertices=cmm))
})
