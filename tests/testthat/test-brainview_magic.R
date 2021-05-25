
test_that("Creating pub-ready figure export from coloredmeshes works.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    testthat::skip_if_not(box.has.x11display(), "This test requires X11.");
    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    rand_data = rnorm(327684, 5, 1.5);
    cm = vis.data.on.fsaverage(morph_data_both=rand_data, rglactions=list('no_vis'=T));
    output_img = tempfile(fileext = ".png");
    vis.export.from.coloredmeshes(cm, colorbar_legend='Random data', output_img = output_img);
    vis.export.from.coloredmeshes(cm, colorbar_legend='Random data', output_img = output_img, large_legend = TRUE);

    vis.export.from.coloredmeshes(cm, colorbar_legend='Random data', output_img = output_img, quality = 2L, horizontal = FALSE);
    vis.export.from.coloredmeshes(cm, colorbar_legend='Random data', output_img = output_img, quality = 2L, large_legend = TRUE);

    expect_true(is.fs.coloredmesh(cm$lh));
    expect_true(is.fs.coloredmesh(cm$rh));
    expect_true(is.hemilist(cm));
    expect_true(file.exists(output_img));
    unlink(output_img); # cleanup

    expect_error(vis.export.from.coloredmeshes(cm, colorbar_legend='Random data', output_img = output_img, quality = 3L, large_legend = TRUE)); # invalid quality
})
