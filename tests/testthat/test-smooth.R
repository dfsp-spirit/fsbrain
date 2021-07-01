
test_that("We can compute vertex distances on a sphere based on a spherical surface.", {
    testthat::skip_on_cran();

    fsbrain::download_fsaverage3();
    sphere_surf_file = get_optional_data_filepath("subjects_dir/fsaverage3/surf/lh.sphere", mustWork = F);
    if(! file.exists(sphere_surf_file)) {
        testthat::skip("The sphere file is available");
    }

    spherical_surface = freesurferformats::read.fs.surface(sphere_surf_file);
    num_verts = nrow(spherical_surface$vertices);
    testthat::expect_equal(num_verts, 642L);

    sphere_dists = surf.sphere.dist(spherical_surface, maxdist = 5.0);

    testthat::expect_true(is.list(sphere_dists));
    testthat::expect_true(all(names(sphere_dists) == c("neigh", "neigh_dist_dotproduct", "neigh_dist_surface")));

    testthat::expect_true(is.list(sphere_dists$neigh));
    testthat::expect_equal(length(sphere_dists$neigh), num_verts);
    testthat::expect_true(is.list(sphere_dists$neigh_dist_dotproduct));
    testthat::expect_equal(length(sphere_dists$neigh_dist_dotproduct), num_verts);
    testthat::expect_true(is.list(sphere_dists$neigh_dist_surface));
    testthat::expect_equal(length(sphere_dists$neigh_dist_surface), num_verts);

    # So far we have only tested the data structure, but not whether the computed distances and neighbors
    # themselves make any sense.
    #
    # One should look at the histogram of neighbor distances on the sphere for any vertex to
    # quickly see whether it makes sense. E.g.,: hist(sphere_dists$neigh_dist_dotproduct[[1]]);
})


test_that("We can compute Gaussian weights based on a spherical surface.", {
    testthat::skip_on_cran();

    fsbrain::download_optional_data();
    sphere_surf_file = get_optional_data_filepath("subjects_dir/subject1/surf/lh.sphere", mustWork = F);
    if(! file.exists(sphere_surf_file)) {
        testthat::skip("The sphere file is available");
    }
    fwhm = 5.0;
    spherical_surface = freesurferformats::read.fs.surface(sphere_surf_file);

    sphere_dists = surf.sphere.dist(spherical_surface, maxdist = 5.0);
    gstd = fwhm / sqrt(log(256.0));
    gaussian_weights = fsbrain:::surf.sphere.gaussianweights(spherical_surface, sphere_dists, gstd);

    testthat::expect_equal(1L, 1L); # only prevent test skipping for now.
})


test_that("Computing fwhm from niters and vice versa is consistent.", {
    testthat::skip_on_cran();
    fsbrain::download_optional_data();
    sphere_surf_file = get_optional_data_filepath("subjects_dir/subject1/surf/lh.sphere", mustWork = F);
    if(! file.exists(sphere_surf_file)) {
        testthat::skip("The sphere file is available");
    }

    fwhm = 5.0;
    spherical_surface = freesurferformats::read.fs.surface(sphere_surf_file);

    niter = fsbrain:::pervertexdata.smoothnn.compute.numiter(spherical_surface, fwhm = fwhm);
    fwhm2 = fsbrain:::pervertexdata.smoothnn.compute.fwhm(spherical_surface, niter);
    testthat::expect_equal(fwhm, fwhm2, tolerance = 1e-2);
})


