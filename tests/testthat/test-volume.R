test_that("A brain volume for a single subject can be loaded", {
    testthat::skip_on_cran(); # skip: leads to memory errors ('cannot allocate vector of size XX MB') on CRAN.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    brain = subject.volume(subjects_dir, subject_id, 'brain');
    brain2 = subject.volume(subjects_dir, subject_id, 'brain', format = 'mgz');

    expect_equal(dim(brain), c(256, 256, 256));
    expect_equal(dim(brain2), c(256, 256, 256));

    # Extract a single slice (2D image) from the volume
    slice = vol.slice(brain, 128);
    expect_equal(dim(slice), c(256, 256));

    # Extract several slices (2D images) from the volume
    slice = vol.slice(brain, c(128, 128));
    expect_equal(dim(slice), c(2, 256, 256));

    # Load the slice into image magick (need to adjust color range)
    #img = magick::image_read(grDevices::as.raster(slice / 255));

    # error handling
    testthat::expect_error(subject.volume(subjects_dir, subject_id, 'brain', format = "nosuchformat"));   # invalid format
})


test_that("Brain volume CRS voxels are rendered at the correct surface space RAS coordinates", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    # This test shows that the vol.vox.from.crs() function and the vox2ras_tkr() functions work correctly.
    # In combination, they allow to plot the voxels from a brain volume (which have no coorindates associated with them,
    # -- just indices) at coordinates in surface RAS space that lead to a proper super-position of the brain surfaces and
    # the brain volume of a subject.
    # In order for this to work, the volume has to be a FreeSurfer conformed volume.

    skip_if_not(box.can.run.all.tests(), "This test requires X11.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");
    brain = subject.volume(subjects_dir, 'subject1', 'brain', with_header = TRUE);

    # Retrieve coloredmeshes (no displaying needed) so we can render the surface and set transparent style if needed
    cm = vis.subject.morph.native(subjects_dir, 'subject1', 'thickness', views=NULL);
    vis.coloredmeshes(cm, style = 'default');   # try with style = 'semitransparent' if required, but be warned: it will have bad performance. (Later rgl.spheres calls draw into this.)

    # ----- Draw a red dot at surface RAS origin -----
    # The voxel at the origin of surface RAS coordinate system. Note that this is NOT expected to be in the
    #   center of the brain surface (because the brain surface is not centered at 0.0, 0.0, 0.0, see the min/max vertex coords along the axes).
    fs_crs = c(128, 128, 128);
    surface_ras_coords = (vox2ras_tkr() %*% vol.vox.from.crs(fs_crs, add_affine=TRUE))[1:3]; # switch to 1-based R indices with affine column, matmult, then strip affine column from result.
    rgl::rgl.spheres(surface_ras_coords, r = 5, color = "#ff0000");    # adds to the active surface plot.

     # ----- Draw a set of 8 green spheres at the outer corners of the 256x256x256 volume (in surface RAS space) -----
     fs_boundary_crs = matrix(c(0, 0, 0, 0, 0, 255, 0, 255, 255, 0, 255, 0, 255, 255, 255, 255, 0, 0, 255, 255, 0, 255, 0, 255), ncol=3, byrow=TRUE);
     boundary_crs_aff = vol.vox.from.crs(fs_boundary_crs, add_affine=TRUE); # switch to 1-based R indices, add affine column
     for(row_idx in seq_len(nrow(boundary_crs_aff))) {
         surface_ras = (vox2ras_tkr() %*% boundary_crs_aff[row_idx,])[1:3];
         rgl::rgl.spheres(surface_ras, r = 5, color = "#00ff00");
     }

     # Compute the bounding box of the brain from the volume data, plot blue spheres at border.
     bbox = vol.boundary.box(brain$data);
     bbox_R_aff = cbind(bbox$edge_coords, 1);
     for(row_idx in seq_len(nrow(bbox_R_aff))) {
         surface_ras = (vox2ras_tkr() %*% bbox_R_aff[row_idx,])[1:3];
         rgl::rgl.spheres(surface_ras, r = 5, color = "#0000ff");
     }

     # That's it, now look at the plot.
     # It shows that the brain surface lies within the volume boundaries, and the bounding box from the volume fits.

     expect_equal(1L, 1L);   # empty tests will be skipped

     # error handling
     expect_error(vol.vox.from.crs(fs_crs = "dunno")); # fs_crs must be numeric
})


test_that("The tkr vox2ras can be retrieved", {
    r2v = ras2vox_tkr();
    expect_true(is.matrix(r2v));
})


test_that("Voxel transform can be computed", {

    # with vector
    fs_crs = c(0L, 0L, 0L);
    r_ind_eucli = vol.vox.from.crs(fs_crs);
    r_ind_homog = vol.vox.from.crs(fs_crs, add_affine = TRUE);

    testthat::expect_true(is.vector(r_ind_eucli));
    testthat::expect_true(is.vector(r_ind_homog));

    # with matrix
    fs_crs_matrix = matrix(seq(6L), ncol = 3L, byrow = TRUE);
    r_ind_eucli_mat = vol.vox.from.crs(fs_crs_matrix);
    r_ind_homog_mat = vol.vox.from.crs(fs_crs_matrix, add_affine = TRUE);

    testthat::expect_true(is.matrix(r_ind_eucli_mat));
    testthat::expect_true(is.matrix(r_ind_homog_mat));
})


test_that("The loaded brain volume is in the correct orientation and the fs CRS to R CRS transformation works", {
    testthat::skip_on_cran(); # skip: leads to memory errors ('cannot allocate vector of size XX MB') on CRAN.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");
    brain = subject.volume(subjects_dir, 'subject1', 'brain', with_header = TRUE);

    # Get voxel intensity data on the OS command line, based
    #  on the FreeSUrfer (zero-based) CRS voxel indices:
    #  `mri_info --voxel 127 100 100 ~/.local/fsbrain/subjects_dir/subject1/mri/brain.mgz`  # Adapt path for your OS, you can get it by running this in R: `fsbrain::get_optional_data_filepath("subjects_dir/subject1/mri/brain.mgz")`.
    #  The result is: 106.
    fs_voxel_crs = c(127,100,100);
    expected_intensity_value = 106;

    # Check that we get the expected result:
    our_crs = vol.vox.from.crs(fs_voxel_crs, add_affine = FALSE);    # Transform to 1-based R indices.
    expect_equal(brain$data[our_crs[1], our_crs[2], our_crs[3]], expected_intensity_value);  # Check the intensity value.
})

