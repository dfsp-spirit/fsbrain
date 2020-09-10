
test_that("Exporting coloredmeshes to vertex-colored PLY meshes works.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    cm = vis.subject.morph.native(subjects_dir, 'subject1', 'thickness', rglactions = list('no_vis'=TRUE));
    export.coloredmesh.ply(tempfile(fileext = ".ply"), cm$lh);

    expect_error(export.coloredmesh.ply(tempfile(fileext = ".ply"), cm)); # hemilist of meshes passed, only single mesh supported.
    expect_error(export.coloredmesh.ply(tempfile(fileext = ".ply"), seq.int(5))); # passed data is not a coloredmesh

    cm_no_metadata = cm$lh;
    cm_no_metadata$metadata$fs_mesh = NULL;
    expect_error(export.coloredmesh.ply(tempfile(fileext = ".ply"), cm_no_metadata)); # missing fs_mesh metadata
})
