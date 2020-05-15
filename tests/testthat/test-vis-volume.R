test_that("A brain volume can be turned into an animation", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(run.extralong.tests(), "This test requires the full test data and X11, and takes ages.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    brain = subject.volume(subjects_dir, subject_id, 'brain');

    # compute bbox to exclude empty outer parts
    bbox = vol.boundary.box(brain);
    foreground = brain[bbox$from[1]:bbox$to[1], bbox$from[2]:bbox$to[2], bbox$from[3]:bbox$to[3]];

    imgplane = 1;
    brain_stack = vol.imagestack(foreground, imgplane);
    magick::image_write(magick::image_animate(brain_stack, fps = 20), sprintf("MRI_axis%d.gif", imgplane));

    imgplane = 2;
    brain_stack = vol.imagestack(freesurferformats::rotate3D(foreground, axis=imgplane), imgplane);
    magick::image_write(magick::image_animate(brain_stack, fps = 20), sprintf("MRI_axis%d.gif", imgplane));

    imgplane = 3;
    brain_stack = vol.imagestack(freesurferformats::rotate3D(foreground, axis=imgplane), imgplane);
    magick::image_write(magick::image_animate(brain_stack, fps = 20), sprintf("MRI_axis%d.gif", imgplane));

    expect_equal(1L, 1L);  # empty tests will be skipped
})


test_that("The axis-aligned bounding box of a 3D brain image can be computed", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    brain = subject.volume(subjects_dir, subject_id, 'brain');

    bbox = vol.boundary.box(brain);
    expect_equal(bbox$from, c(61L, 14L, 17L));
    expect_equal(bbox$to, c(195L, 166L, 194L));

    # Now select the inner foreground volume as a new image:
    foreground = brain[bbox$from[1]:bbox$to[1], bbox$from[2]:bbox$to[2], bbox$from[3]:bbox$to[3]];
    expect_equal(dim(foreground), c(135L, 153L, 178L));
})


test_that("The axis-aligned bounding box of a white (foreground-only) image has the images dimensions", {
    dimlen = 5L;
    brain = array(rep(1L, dimlen**3), rep(dimlen, 3L));

    bbox = vol.boundary.box(brain);
    expect_equal(bbox$from, c(1L, 1L, 1L));
    expect_equal(bbox$to, c(5L, 5L, 5L));
})


test_that("The axes dimensions are correct in the bounding mask result", {
    dimensions = c(5L, 8L, 3L);
    brain = array(rep(1L, 5*8*3), dimensions);

    plane = c(2,3);
    bmask = vol.boundary.mask(brain, plane);
    expect_equal(dim(bmask), dimensions[plane]);

    plane2 = c(1,3);
    bmask = vol.boundary.mask(brain, plane2);
    expect_equal(dim(bmask), dimensions[plane2]);

    plane3 = c(1,2);
    bmask = vol.boundary.mask(brain, plane3);
    expect_equal(dim(bmask), dimensions[plane3]);
})


test_that("The axes dimensions are correct in the bounding box result", {
    dimensions = c(5L, 8L, 3L);
    brain = array(rep(1L, 5*8*3), dimensions);

    bbox = vol.boundary.box(brain);
    expect_equal(bbox$from, c(1L, 1L, 1L));
    expect_equal(bbox$to, dimensions);
})


test_that("The axis-aligned bounding box of a black (background-only) image has NA values only", {
    dimlen = 5L;
    brain = array(rep(0L, dimlen**3), rep(dimlen, 3L));

    bbox = vol.boundary.box(brain);
    expect_true(all(is.na(bbox$from)));
    expect_true(all(is.na(bbox$to)));
})


test_that("Axes are derived from a plane definition as expected", {
    # Test when given as axes
    expect_equal(vol.plane.axes(c(2,3)), c(2,3));
    expect_equal(vol.plane.axes(c(1,2)), c(1,2));
    expect_equal(vol.plane.axes(c(1,3)), c(1,3));

    # Test when given as plane index
    expect_equal(vol.plane.axes(1), c(1,2));
    expect_equal(vol.plane.axes(2), c(2,3));
    expect_equal(vol.plane.axes(3), c(3,1));

    # Test with axis names
    expect_equal(vol.plane.axes("sagittal"), c(2,3));
    expect_equal(vol.plane.axes("axial"), c(3,1));
    expect_equal(vol.plane.axes("coronal"), c(1,2));
})


test_that("A brain volume and an overlay can be merged", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11 and the 'magick' package (ImageMagick for R).");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    brain = subject.volume(subjects_dir, subject_id, 'brain') / 255;

    # Generate some demo activation data
    activation = array(rep(0L, dim(brain)[1]*dim(brain)[2]*dim(brain)[3]), dim(brain));
    activation[90:110, 90:110, 90:110] = 1L;  # set some values in the center to activated
    activation[90:110, 50:90, 50:60] = -1L;

    overlay_colors = vol.overlay.colors.from.activation(activation);
    merged = vol.merge(brain, overlay_colors, bbox_threshold = NULL);   # Deactivate bbox computation, so the index does not shift. Feel free to remove the 'bbox_threshold = NULL' part, but keep in mind then that slice 95 below will not be the correct slice index anymore to look for the activation.

    # Done. Look at a single slice of the result:
    magick::image_read(vol.slice(merged, 95));

    # Now test that the merged image can be visualized as a lightbox:
    lb = volvis.lightbox(merged);       # This is large, so it is better to write it to disk and open in an external viewer.
    magick::image_write(lb, path="brain_lightbox.png");

    expect_equal(1L, 1L);   # empty tests will be skipped
})


test_that("A brain volume can be visualized as a lightbox", {

    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    brain = subject.volume(subjects_dir, subject_id, 'brain') / 255;

    # Compute and apply bbox to exclude empty outer parts
    brain = vol.boundary.box(brain, apply=TRUE);

    # Now test that the merged image can be visualized as a lightbox:
    imgplane = 1;

    volvis.lightbox(brain, axis=imgplane);

    do_write_images_to_disk = FALSE;
    if(do_write_images_to_disk) {
        magick::image_write(volvis.lightbox(brain, axis=imgplane), path=sprintf("lightbox_axis%d.png", imgplane));

        imgplane = 2;
        magick::image_write(volvis.lightbox(freesurferformats::rotate3D(brain, axis=imgplane), axis=imgplane), path=sprintf("lightbox_axis%d.png", imgplane));

        imgplane = 3;
        magick::image_write(volvis.lightbox(freesurferformats::rotate3D(brain, axis=imgplane), axis=imgplane), path=sprintf("lightbox_axis%d.png", imgplane));
    }

    expect_equal(1L, 1L);   # prevent skipping
})


test_that("Intensity integer to RGB color string conversion works in 1, 2, and 3 dimensions.", {
    # Test 1D
    out1d = vol.intensity.to.color(c(0.0, 0.5, 1.0));
    expect_true(is.vector(out1d));
    expect_equal(out1d, c("#000000", "#808080","#FFFFFF"));

    # Test 2D
    out2d = vol.intensity.to.color(matrix(c(0.0, 0.2, 0.5, 1.0), nrow=2));
    expect_true(is.matrix(out2d));
    expect_equal(nrow(out2d), 2);
    expect_equal(ncol(out2d), 2);
    expect_equal(out2d[2,2], "#FFFFFF");

    # Test 3D
    out3d = vol.intensity.to.color(array(rep(0.0, 9), c(3,3,3)));
    expect_true(is.array(out3d));
    expect_equal(out3d[2,2,2], "#000000");
    expect_equal(dim(out3d), c(3,3,3));
})


test_that("A brain volume can be visualized as a lightbox colored from the aseg", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11, the 'magick' package (ImageMagick for R), and extra data.");

    fsbrain::download_optional_data();
    subjects_dir = testdatapath.subjectsdir.full.subject1();
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    brain = subject.volume(subjects_dir, subject_id, 'brain');
    aseg = subject.volume(subjects_dir, subject_id, 'aparc+aseg');    # Not shipped with the package atm.

    colortable = freesurferformats::read.fs.colortable("~/software/freesurfer/FreeSurferColorLUT.txt");   # adapt path to your machine
    overlay_colors = vol.overlay.colors.from.colortable(aseg, colortable);
    colored_brain = vol.merge(brain/255, overlay_colors); # will also apply bounding box by default


    # Now test that the merged image can be visualized as a lightbox:
    imgplane = 1;
    volvis.lightbox(colored_brain, axis=imgplane);

    do_write_images_to_disk = FALSE;
    if(do_write_images_to_disk) {
        magick::image_write(volvis.lightbox(colored_brain, axis=imgplane), path=sprintf("lightbox_axis%d.png", imgplane));

        imgplane = 2;
        magick::image_write(volvis.lightbox(freesurferformats::rotate3D(colored_brain, axis=imgplane), axis=imgplane), path=sprintf("lightbox_axis%d.png", imgplane));

        imgplane = 3;
        magick::image_write(volvis.lightbox(freesurferformats::rotate3D(colored_brain, axis=imgplane), axis=imgplane), path=sprintf("lightbox_axis%d.png", imgplane));
    }

    expect_equal(1L, 1L);   # prevent skipping

})

