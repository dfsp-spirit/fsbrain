test_that("A brain volume can be turned into an animation", {

    skip("This test has to be run manually and interactively. It also requires the 'magick' package (ImageMagick for R).");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    brain = subject.volume(subjects_dir, subject_id, 'brain');

    brain_stack = vol.imagestack(brain);
    magick::image_write(magick::image_animate(brain_stack, fps = 20), "MRI.gif");
})


test_that("The axis-aligned bounding box of a 3D brain image can be computed", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    brain = subject.volume(subjects_dir, subject_id, 'brain');

    bbox = vol.boundary.box(brain);
    expect_equal(bbox$from, c(61L, 14L, 17L));
    expect_equal(bbox$to, c(195L, 166L, 194L));
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

