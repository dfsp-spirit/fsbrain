

test_that("A brain volume or parts of it can be rendered in voxel mode", {

    skip("This test has to be run manually and interactively. It requires an aseg.mgz file for the demo subject.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    aseg = subject.volume(subjects_dir, subject_id, 'aseg');    # Not shipped with the package atm.
    ventricle_aseg_codes = c(4, 14, 15, 43);    # see FreeSurferColorLUT.txt
    ventricle_mask = vol.mask.from.segmentation(aseg, ventricle_aseg_codes);

    volvis.voxels(ventricle_mask);

    # Some more segmentation ROIs to play with:
    wm_mask = vol.mask.from.segmentation(aseg, c(2, 41));
    cortex_mask = vol.mask.from.segmentation(aseg, c(3, 42));

    # Use voxel colors when rendering: based on a colormap.
    volvis.voxels(ventricle_mask, voxelcol = vol.overlay.colors.from.activation(ventricle_mask), render_every = 6);

    # Use voxel colors when rendering: gray-scale, computed from the intensity values of the volume itself:
    volvis.voxels(ventricle_mask, voxelcol = 'from_intensity', render_every = 6);
})


test_that("A brain volume segmentation can be rendered with correct colors from the aseg", {

    skip("This test has to be run manually and interactively. It requires an aseg.mgz file for the demo subject and the FreeSurferColorLUT.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    #aseg = subject.volume(subjects_dir, subject_id, 'aseg');    # Not shipped with the package atm.
    aseg = subject.volume(subjects_dir, subject_id, 'aparc+aseg');    # Not shipped with the package atm.

    aseg_codes = unique(as.vector(aseg));

    ct = freesurferformats::read.fs.colortable("~/software/freesurfer/FreeSurferColorLUT.txt");   # adapt to your machine

    open3d();
    for(aseg_code in aseg_codes) {
        if(aseg_code == 0) { # skip background ('unknown').
            next;
        }
        ct_entry = subset(ct, ct$struct_index == aseg_code);
        ct_color_rgb = grDevices::rgb(ct_entry$r / 255., ct_entry$g / 255., ct_entry$b / 255.);
        volvis.voxels(vol.mask.from.segmentation(aseg, aseg_code), render_every=10, color=ct_color_rgb);
    }
    # Check it out, it looks pretty cool.
})


