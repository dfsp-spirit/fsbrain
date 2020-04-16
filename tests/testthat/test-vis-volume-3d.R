

test_that("A brain volume or parts of it can be rendered in voxel mode", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11 and all test data.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    aseg = subject.volume(subjects_dir, subject_id, 'aseg');    # Not shipped with the package atm.
    ventricle_aseg_codes = c(4, 14, 15, 43);    # see FreeSurferColorLUT.txt
    ventricle_mask = vol.mask.from.segmentation(aseg, ventricle_aseg_codes);

    volvis.voxels(ventricle_mask, render_every = 10);

    # Some more segmentation ROIs to play with:
    wm_mask = vol.mask.from.segmentation(aseg, c(2, 41));
    cortex_mask = vol.mask.from.segmentation(aseg, c(3, 42));

    # Use voxel colors when rendering: gray-scale, computed from the intensity values of the volume itself:
    volvis.voxels(ventricle_mask, voxelcol = 'from_intensity', render_every = 6);

    # Use voxel colors when rendering: based on a colormap.
    coloredvoxels = volvis.voxels(ventricle_mask, voxelcol = vol.overlay.colors.from.activation(ventricle_mask), render_every = 1);

    render_animation = FALSE;
    if(render_animation) {
        rgloptions=list("windowRect"=c(80,80,1200,1200));     # the first 2 entries give the position on screen, the rest defines resolution as width, height in px
        rglactions = list("movie"="vox_ventricles_rot");
        vislayout.from.coloredmeshes(coloredvoxels, view_angles="sr", rgloptions = rgloptions, rglactions = rglactions);
    }

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.

})


test_that("A brain volume segmentation can be rendered with correct colors from the aseg", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11, an aseg.mgz file for the demo subject and the FreeSurferColorLUT.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    #aseg = subject.volume(subjects_dir, subject_id, 'aseg');    # Not shipped with the package atm.
    aseg = subject.volume(subjects_dir, subject_id, 'aparc+aseg');    # Not shipped with the package atm.

    aseg_codes = unique(as.vector(aseg));

    fs_home = find.freesurferhome()$found_at;
    ct = freesurferformats::read.fs.colortable(file.path(fs_home, 'FreeSurferColorLUT.txt'));

    open3d();
    all_regions_coloredvoxels = list();
    for(aseg_code in aseg_codes) {
        if(aseg_code == 0) { # skip background ('unknown').
            next;
        }
        ct_entry = subset(ct, ct$struct_index == aseg_code);
        ct_color_rgb = grDevices::rgb(ct_entry$r / 255., ct_entry$g / 255., ct_entry$b / 255.);
        cv = volvis.voxels(vol.mask.from.segmentation(aseg, aseg_code), render_every=1, voxelcol=ct_color_rgb);
        all_regions_coloredvoxels = c(all_regions_coloredvoxels, cv);
    }
    # Check it out, it looks pretty cool.

    render_animation = FALSE;
    if(render_animation) {
        rgloptions=list("windowRect"=c(80,80,1200,1200));     # the first 2 entries give the position on screen, the rest defines resolution as width, height in px
        rglactions = list("movie"="vox_aseg_rot");
        vislayout.from.coloredmeshes(all_regions_coloredvoxels, view_angles="sr", rgloptions = rgloptions, rglactions = rglactions);
    }

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
})


test_that("Brain structures can be rendered as contours using misc3d", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11, the misc3d package, and an aseg.mgz file for the demo subject.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    aseg = subject.volume(subjects_dir, subject_id, 'aseg');    # Not shipped with the package atm.
    ventricle_aseg_codes = c(4, 14, 15, 43);    # see FreeSurferColorLUT.txt
    ventricle_mask = vol.mask.from.segmentation(aseg, ventricle_aseg_codes);

    # Remove the NAs, contour3d does not seem to like them.
    ventricle_mask_mod = ventricle_mask;
    ventricle_mask_mod[which(is.na(ventricle_mask), arr.ind=T)] = 0;

    custom_colors = grDevices::terrain.colors(length(ventricle_aseg_codes));
    misc3d::contour3d(ventricle_mask_mod, level=ventricle_aseg_codes, color=custom_colors, alpha = seq(0.2, 0.5, length.out = length(ventricle_aseg_codes)));



    ## ---- Draw the surface of the left hemi, and the ventricle contours into the same plot ----
    vis.subject.morph.native(subjects_dir, 'subject1', 'thickness', 'lh', views = 'si', style='semitransparent');
    vent_tris = misc3d::contour3d(ventricle_mask_mod, level=3, color="red", draw=FALSE);
    # # Fix the rendering coords to surface RAS
    vent_tris = apply.transform(vent_tris, vox2ras_tkr());
    misc3d::drawScene.rgl(vent_tris, add = TRUE);

    ## Add transparent overlay of whole brain for worse performance ><
    have_mighty_computer = TRUE;
    if(have_mighty_computer) {
        brain_tris = misc3d::contour3d(aseg, level=1, color="gray", alpha=0.1, back='culled', draw = FALSE);
        brain_tris = apply.transform(brain_tris, vox2ras_tkr());
        misc3d::drawScene.rgl(brain_tris, add = TRUE);
    }

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.

})


test_that("The pial surface drawn as a transparent wrapping over the white surface", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11 and extra data.");

    fsbrain::download_optional_data();
    subjects_dir = testdatapath.subjectsdir.full.subject1();
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";

    cm_white = vis.subject.morph.native(subjects_dir, 'subject1', 'thickness', hemi = 'both', surface = 'white', views = NULL);
    cm_pial = vis.subject.morph.native(subjects_dir, 'subject1', 'thickness', hemi = 'both', surface = 'pial', views = NULL);
    cm_pial[[1]]$style = 'semitransparent';
    cm_pial[[2]]$style = 'semitransparent';
    vis.coloredmeshes(c(cm_white, cm_pial), skip_all_na = FALSE, style = 'from_mesh');

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
})


test_that("Voxels can be rotated and rendered in a brainview", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";

    vol = subject.volume(subjects_dir, subject_id, "brain");
    vol[vol < 90] = NA;
    volvox = volvis.voxels(vol);
    brainviews("t9", volvox);

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
})


test_that("A misc3d contour (Triangles3D instance) can be rotated and rendered in a brainview", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";

    vol = subject.volume(subjects_dir, subject_id, "brain");
    surface_tris = fsbrain::volvis.contour(vol);
    brainviews("t9", surface_tris);

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
})
