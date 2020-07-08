
test_that("Coords of EEG are plotted roughly in expected locations", {
    fsavgdir = fsaverage.path();
    vis.fs.surface(file.path(fsavgdir, 'fsaverage', 'surf', 'lh.white'));
    coords_near_eyes = eeg_coords(c("Fp1", "Fp2"));
    coords_near_ears = eeg_coords(c("A1", "A2"));
    coords_near_ears = eeg_coords(c("A1", "A2"));
    coord_top_center = eeg_coords(c("Cz"));
    coords_preauricular = eeg_coords(c("LPA", "RPA"));

    sphere_radius = 3; # just for plotting
    rgl.spheres(sph2fs(coords_near_eyes$theta, coords_near_eyes$phi), col="blue", radius = sphere_radius);
    rgl.spheres(sph2fs(coords_near_ears$theta, coords_near_ears$phi), col="green", radius = sphere_radius);
    rgl.spheres(sph2fs(coord_top_center$theta, coord_top_center$phi), col="red", radius = sphere_radius);
    rgl.spheres(sph2fs(coord_nose$theta, coord_nose$phi), col="black", radius = sphere_radius);
    rgl.spheres(sph2fs(coords_preauricular$theta, coords_preauricular$phi), col="yellow", radius = sphere_radius);

    rgl.coord.lines();
})
