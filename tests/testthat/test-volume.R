test_that("A brain volume for a single subject can be loaded", {

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    brain = subject.volume(subjects_dir, subject_id, 'brain');

    expect_equal(dim(brain), c(256, 256, 256));

    # Extract a single slice (2D image) from the volume
    slice = vol.slice(brain, 128);
    expect_equal(dim(slice), c(256, 256));

    # Extract several slices (2D images) from the volume
    slice = vol.slice(brain, c(128, 128));
    expect_equal(dim(slice), c(2, 256, 256));

    # Load the slice into image magick (need to adjust color range)
    #img = magick::image_read(grDevices::as.raster(slice / 255));
})
