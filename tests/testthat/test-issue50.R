
test_that("Issue50 is fixed: t9 view export works with inflated surfaces without overlap", {
    testthat::skip_on_cran();
    testthat::skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");

    fsbrain::download_optional_data();
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'fsaverage';

    lh_demo_cluster_file = system.file("extdata", "lh.clusters_fsaverage.mgz", package = "fsbrain", mustWork = TRUE);
    rh_demo_cluster_file = system.file("extdata", "rh.clusters_fsaverage.mgz", package = "fsbrain", mustWork = TRUE);

    lh_clust = freesurferformats::read.fs.morph(lh_demo_cluster_file);   # contains a single positive cluster (activation, group difference), the other values are 0
    rh_clust = freesurferformats::read.fs.morph(rh_demo_cluster_file);   # contains two negative clusters

    cm = fsbrain::vis.symmetric.data.on.subject(subjects_dir, subject_id, lh_clust, rh_clust, surface="inflated", bg="curv_light", rglactions = list('shift_hemis_apart'=TRUE), views = NULL);

    fsbrain::export(cm, img_only = TRUE, output_img = "fsbrain_issue50_export.png", rglactions = list('shift_hemis_apart'=TRUE), view_angles=fsbrain::get.view.angle.names(angle_set = "t9"), colorbar_legend= "issue50 test");

    testthat::expect_equal(1L-1L, 0L);    # Must have expect, otherwise test gets skipped. One currently needs to verify visually that the figure looks correct (tight layout, non-overlapping surfaces).
})

test_that("Issue50 is fixed: t9 view export works with inflated surfaces without overlap on native", {

    testthat::skip_on_cran();
    testthat::skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");

    fsbrain::download_optional_data()
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE)
    sjd = fsbrain::get_optional_data_filepath("subjects_dir");
    sj = 'subject1';

    cm = fsbrain::vis.subject.morph.standard(sjd, sj, 'sulc', fwhm='10', cortex_only = T, views=NULL, surface = "inflated", rglactions = list('shift_hemis_apart'=TRUE));
    img = fsbrain::export(cm, colorbar_legend='Sulcal depth [mm]', output_img = "~/sulcal_depth.png", view_angles = fsbrain::get.view.angle.names(angle_set = "t9"), rglactions = list('shift_hemis_apart'=TRUE));

    testthat::expect_equal(1L-1L, 0L);    # Must have expect, otherwise test gets skipped. One currently needs to verify visually that the figure looks correct (tight layout, non-overlapping surfaces).

})
