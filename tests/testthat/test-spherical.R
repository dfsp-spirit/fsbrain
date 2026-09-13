
# test_that("Coords of EEG are plotted roughly in expected locations", {
#     subjects_dir = find.subjectsdir.of("fsaverage")$found_at;
#     skip_if_not(find.subjectsdir.of("fsaverage")$found, message="Test data for fsaverage missing.");
#
#     vis.fs.surface(file.path(subjects_dir, 'fsaverage', 'surf', 'lh.white'));
#     coords_near_eyes = eeg_coords(c("Fp1", "Fp2"));
#     coords_near_ears = eeg_coords(c("A1", "A2"));
#     coord_nose = eeg_coords(c("Nz"));
#     coord_top_center = eeg_coords(c("Cz"));
#     coords_preauricular = eeg_coords(c("LPA", "RPA"));
#
#     sphere_radius = 3; # just for plotting
#     rgl::spheres3d(sph2fs(coords_near_eyes$theta, coords_near_eyes$phi), col="blue", radius = sphere_radius);
#     rgl::spheres3d(sph2fs(coords_near_ears$theta, coords_near_ears$phi), col="green", radius = sphere_radius);
#     rgl::spheres3d(sph2fs(coord_top_center$theta, coord_top_center$phi), col="red", radius = sphere_radius);
#     rgl::spheres3d(sph2fs(coord_nose$theta, coord_nose$phi), col="black", radius = sphere_radius);
#     rgl::spheres3d(sph2fs(coords_preauricular$theta, coords_preauricular$phi), col="yellow", radius = sphere_radius);
#
#     rgl.coord.lines();
#
#     expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
# })


test_that("Spherical coordinates can be transformed to cartesian FreeSurfer space coordinates", {
    # This test does not require downloaded data or any optional package.

    # The three axis directions.
    expect_equal(unname(sph2fs(lon = 0, lat = 0, radius = 10, center = c(0, 0, 0))), matrix(c(10, 0, 0), nrow = 1L));
    expect_equal(unname(sph2fs(lon = 90, lat = 0, radius = 10, center = c(0, 0, 0))), matrix(c(0, 10, 0), nrow = 1L));
    expect_equal(unname(sph2fs(lon = 0, lat = 90, radius = 10, center = c(0, 0, 0))), matrix(c(0, 0, 10), nrow = 1L));

    # The result is a double matrix with one row per input point and one column per axis.
    res = sph2fs(lon = c(0, 90), lat = c(0, 0), radius = 1, center = c(0, 0, 0));
    expect_equal(class(res), c("matrix", "array"));
    expect_equal(typeof(res), "double");
    expect_equal(dim(res), c(2L, 3L));
    expect_equal(colnames(res), c("x", "y", "z"));

    # The radius is applied, and the center is used to translate the result.
    expect_equal(unname(sph2fs(lon = 0, lat = 0, radius = 2, center = c(0, 0, 0))), matrix(c(2, 0, 0), nrow = 1L));
    expect_equal(unname(sph2fs(lon = 0, lat = 0, radius = 1, center = c(10, -20, 30))), matrix(c(11, -20, 30), nrow = 1L));

    # The radius can differ between the points.
    expect_equal(unname(sph2fs(lon = c(0, 90, 180), lat = c(0, 0, 0), radius = c(1, 2, 3), center = c(0, 0, 0))),
                 matrix(c(1, 0, 0, 0, 2, 0, -3, 0, 0), nrow = 3L, byrow = TRUE));

    # Negative latitudes and longitudes work.
    expect_equal(unname(sph2fs(lon = 180, lat = -45, radius = 2, center = c(0, 0, 0))),
                 matrix(c(-sqrt(2), 0, -sqrt(2)), nrow = 1L));

    # The angles can be given in radians instead of degrees.
    expect_equal(unname(sph2fs(lon = pi/2, lat = 0, radius = 1, center = c(0, 0, 0), deg = FALSE)),
                 unname(sph2fs(lon = 90, lat = 0, radius = 1, center = c(0, 0, 0), deg = TRUE)));

    # By default, the pre-computed radius and center of the fsaverage white surface are used.
    default_res = sph2fs(lon = 0, lat = 0);
    expect_equal(dim(default_res), c(1L, 3L));
    expect_equal(unname(default_res),
                 unname(sph2fs(lon = 0, lat = 0, radius = surf.radius.fsaverage(), center = surf.center.fsaverage())));
    expect_equal(unname(default_res[1, 1]), unname(surf.radius.fsaverage() + surf.center.fsaverage()[1]), tolerance = 1e-6);

    # The demo EEG electrode coordinates can be transformed. The electrode 'Cz' is at the top center.
    eeg = eeg_coords();
    expect_equal(nrow(eeg), 10L);
    eeg_cart = sph2fs(eeg$theta, eeg$phi, radius = 1, center = c(0, 0, 0));
    expect_equal(dim(eeg_cart), c(10L, 3L));
    expect_equal(as.numeric(eeg_cart[which(eeg$label == "Cz"), ]), c(1, 0, 0));
})
