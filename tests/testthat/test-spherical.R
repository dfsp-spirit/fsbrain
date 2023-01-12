
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
