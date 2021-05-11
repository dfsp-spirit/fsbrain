# These tests are to be run manually and interactively, they are therefore skipped by default.
# You can run them by copying & pasting the code into an R session. Treat it as examples.

test_that("We can visualize morphometry data from different views.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11.");

    fsbrain::download_optional_data();
    fsbrain::download_optional_paper_data();

    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';
    measure = 'thickness';
    surface = 'white';

    vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', rgloptions = rglot(), draw_colorbar = T, views = c("t9", "si", "sr"));
    vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', rgloptions = rglot(), draw_colorbar = T, views = c("t4"), draw_colorbar = "horizontal");
    vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', rgloptions = rglot(), draw_colorbar = T, views = c("t9"), draw_colorbar = "horizontal");

    vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', rgloptions = rglot(), draw_colorbar = T, views = c("sd_lateral_lh"), draw_colorbar = "horizontal");
    vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', rgloptions = rglot(), draw_colorbar = T, views = c("sd_ventral"), draw_colorbar = "vertical");
    vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', rgloptions = rglot(), draw_colorbar = T, views = c("sd_rostral"), draw_colorbar = FALSE);
    vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', rgloptions = rglot(), draw_colorbar = T, views = c("sd_caudal"), draw_colorbar = FALSE);

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.

    # error handling
    expect_error(vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', rgloptions = rglot(), draw_colorbar = T, views = "nosuchview"));  # invalid views
    expect_error(vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', rgloptions = rglot(), draw_colorbar = T, views = c("t4"), draw_colorbar = "dunno")); # invalid draw_colorbar setting in t4 view
    expect_error(vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', rgloptions = rglot(), draw_colorbar = T, views = c("t9"), draw_colorbar = "dunno")); # invalid draw_colorbar setting in t9 view
    expect_error(vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', rgloptions = rglot(), draw_colorbar = T, views = c("sd_laternal_lh"), draw_colorbar = "dunno")); # invalid draw_colorbar setting in sd_ view

    close.all.rgl.windows();
})


test_that("We can visualize annotation atlas data.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11.");

    fsbrain::download_optional_data();

    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';

    vis.subject.annot(subjects_dir, subject_id, 'aparc', 'both', rgloptions = rglot());

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
    close.all.rgl.windows();
})


test_that("We can visualize arbitrary data on a subjects surface.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';

    num_verts_subject1_lh = 149244;  # We need to know these to generate random data of suitable length.
    num_verts_subject1_rh = 153333;

    morph_data_lh = rnorm(num_verts_subject1_lh, 2.0, 1.0);
    morph_data_rh = rnorm(num_verts_subject1_rh, 2.0, 1.0);

    vis.data.on.subject(subjects_dir, subject_id, morph_data_lh, morph_data_rh, rgloptions = rglot(), draw_colorbar = T);

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
})


test_that("We can visualize arbitrary data on the fsaverage surfaces if available.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11.");

    subjects_dir_query = find.subjectsdir.of(subject_id='fsaverage', mustWork = FALSE);
    if(subjects_dir_query$found) {
        subjects_dir = subjects_dir_query$found_at;
    } else {
        skip("The environment variables FREESURFER_HOME and SUBJECTS_DIR are not set. FreeSurfer is not installed correctly, or maybe you are running this from within a GUI program (like rstudio) started from an environment that does not have them available.");
    }

    num_verts_fsaverage = 163842;

    morph_data_lh = rnorm(num_verts_fsaverage, 2.0, 1.0);
    morph_data_rh = rnorm(num_verts_fsaverage, 2.0, 1.0);

    vis.data.on.fsaverage(morph_data_lh=morph_data_lh, morph_data_rh=morph_data_rh, rgloptions = rglot(), draw_colorbar = T);

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
    close.all.rgl.windows();
})


test_that("We can visualize one value per atlas region on a subject.", {
    testthat::skip_on_cran();
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';

    atlas = "aparc";           # an atlas, e.g., 'aparc', 'aparc.a2009s', 'aparc.DKTatlas'
    atlas_region_names = get.atlas.region.names(atlas, template_subjects_dir = subjects_dir, template_subject = subject_id);

    # Get some data to display. Here we use random data as an example. You would put something like the mean of some morphometry measure, p value, effect size, or whatever.
    lh_region_value_list = as.list(rnorm(length(atlas_region_names), mean=5, sd=1.5)); # assign random normal values to regions
    names(lh_region_value_list) = atlas_region_names;    # Assign the names to the values.
    rh_region_value_list = as.list(rnorm(length(atlas_region_names), mean=5.5, sd=1.8));
    names(rh_region_value_list) = atlas_region_names;

    morph_data = spread.values.over.subject(subjects_dir, subject_id, atlas, lh_region_value_list, rh_region_value_list, value_for_unlisted_regions=NaN);
    vis.data.on.subject(subjects_dir, subject_id, morph_data$lh, morph_data$rh);

    # Test that we can pass NULL data, which should not render that hemisphere.
    morph_data2 = spread.values.over.subject(subjects_dir, subject_id, atlas, NULL, rh_region_value_list, value_for_unlisted_regions=NaN);
    vis.data.on.subject(subjects_dir, subject_id, morph_data2$lh, morph_data2$rh);

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
    close.all.rgl.windows();
})


test_that("We can visualize one value per Desikan atlas region on fsaverage.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11.");

    subjects_dir = find.subjectsdir.of("fsaverage")$found_at;
    subject_id = 'fsaverage';                                   # You could visualize on any other subject, of course.

    atlas = "aparc";    # An atlas file name, e.g., 'aparc' for Desikan-Killiany, 'aparc.a2009s' for Destrieux, 'aparc.DKTatlas' for DKT40 (see https://surfer.nmr.mgh.harvard.edu/fswiki/CorticalParcellation)

    # We show two different examples for the hemispheres here.
    # Example 1: for the left hemisphere, we load all the region names from the atlas and assign random values to all regions:
    atlas_region_names = get.atlas.region.names(atlas, template_subjects_dir = subjects_dir, template_subject = subject_id);
    lh_region_value_list = as.list(rnorm(length(atlas_region_names), mean=5, sd=1.5)); # assign random normal values to regions. You would put your p values, effect sizes or whatever here.
    names(lh_region_value_list) = atlas_region_names;    # Assign the names to the values.

    # Example 2: For the right hemisphere, we assume you have data for some regions only and want to manually assign the data values to these regions only:
    rh_region_value_list = list("precuneus"=0.37, "pericalcarine"=0.21, "temporalpole"=0.77);    # Put your data here.
    # Note: To find all valid region names of the atlas, see the variable 'atlas_region_names' from example 1 above.

    # Now compute morphometry data (assign the given value from the region_value_lists to each vertex of the region):
    morph_data = spread.values.over.subject(subjects_dir, subject_id, atlas, lh_region_value_list, rh_region_value_list, value_for_unlisted_regions=NaN);

    # We can visualize the data directly in fsbrain:
    vis.data.on.subject(subjects_dir, subject_id, morph_data$lh, morph_data$rh, makecmap_options=list('colFn'=grDevices::heat.colors));

    # Of course, you can also save a file with your data for visualization in other software, if you prefer:
    # freesurferformats::write.fs.morph("~/lh.regiondata.mgz", morph_data$lh);
    # freesurferformats::write.fs.morph("~/rh.regiondata.mgz", morph_data$rh);

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
    close.all.rgl.windows();
})


test_that("We can visualize a subset of the regions of the Desikan atlas on fsaverage.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.has.fsaverage(), "This test requires fsaverage.");

    subjects_dir = find.subjectsdir.of("fsaverage")$found_at;
    subject_id = 'fsaverage';                                   # You could visualize on any other subject, of course.

    atlas = "aparc";    # An atlas file name, e.g., 'aparc' for Desikan-Killiany, 'aparc.a2009s' for Destrieux, 'aparc.DKTatlas' for DKT40 (see https://surfer.nmr.mgh.harvard.edu/fswiki/CorticalParcellation)

    # Get all valid region names of the atlas:
    atlas_region_names = get.atlas.region.names(atlas, template_subjects_dir = subjects_dir, template_subject = subject_id);
    #print(atlas_region_names);

    # Select some regions we want to show. We assign the same value to each region, so all get the same color.
    lh_region_value_list = c("precuneus"=1, "pericalcarine"=1, "temporalpole"=1, "bankssts"=1, "superiorparietal"=1);
    rh_region_value_list = c("parahippocampal"=1, "parsopercularis"=1, "lingual"=1, "inferiortemporal"=1);

    # Assign the values to all region vertices:
    morph_data = spread.values.over.subject(subjects_dir, subject_id, atlas, lh_region_value_list, rh_region_value_list, value_for_unlisted_regions=NaN);

    # We can visualize the data directly in fsbrain:
    vis.data.on.subject(subjects_dir, subject_id, morph_data$lh, morph_data$rh, makecmap_options=list('colFn'=grDevices::grey.colors));

    # Of course, you can also save a file with your data for visualization in other software, if you prefer:
    # freesurferformats::write.fs.morph("~/lh.regiondata.mgz", morph_data$lh);
    # freesurferformats::write.fs.morph("~/rh.regiondata.mgz", morph_data$rh);

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
    close.all.rgl.windows();
})


test_that("We can visualize clusters on fsaverage with a background.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.has.freesurfer() & box.has.fsaverage(), "This test requires the full fsaverage subject with curv data.");

    subjects_dir = file.path(find.freesurferhome()$found_at, 'subjects');
    subject_id = 'fsaverage';

    lh_demo_cluster_file = system.file("extdata", "lh.clusters_fsaverage.mgz", package = "fsbrain", mustWork = TRUE);
    rh_demo_cluster_file = system.file("extdata", "rh.clusters_fsaverage.mgz", package = "fsbrain", mustWork = TRUE);
    lh_clust = freesurferformats::read.fs.morph(lh_demo_cluster_file);   # contains a single positive cluster (activation, group difference), the other values are 0
    rh_clust = freesurferformats::read.fs.morph(rh_demo_cluster_file);   # contains two negative clusters
    vis.symmetric.data.on.subject(subjects_dir, subject_id, lh_clust, rh_clust, bg="curv");

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
    close.all.rgl.windows();
})


test_that("We can visualize clusters on fsaverage with a background and use a range smaller than the original data range.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.has.freesurfer() & box.has.fsaverage(), "This test requires the full fsaverage subject with curv data.");

    subjects_dir = file.path(find.freesurferhome()$found_at, 'subjects');
    subject_id = 'fsaverage';

    lh_demo_cluster_file = system.file("extdata", "lh.clusters_fsaverage.mgz", package = "fsbrain", mustWork = TRUE);
    rh_demo_cluster_file = system.file("extdata", "rh.clusters_fsaverage.mgz", package = "fsbrain", mustWork = TRUE);
    lh_clust = freesurferformats::read.fs.morph(lh_demo_cluster_file);   # contains a single positive cluster (activation, group difference), the other values are 0
    rh_clust = freesurferformats::read.fs.morph(rh_demo_cluster_file);   # contains two negative clusters
    vis.symmetric.data.on.subject(subjects_dir, subject_id, lh_clust, rh_clust, bg="curv", makecmap_options = list('range' = c(-2, 2)));

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
    close.all.rgl.windows();
})


test_that("We can visualize clusters on fsaverage with a background and use a range larger than the original data range.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.has.freesurfer() & box.has.fsaverage(), "This test requires the full fsaverage subject with curv data.");

    subjects_dir = file.path(find.freesurferhome()$found_at, 'subjects');
    subject_id = 'fsaverage';

    lh_demo_cluster_file = system.file("extdata", "lh.clusters_fsaverage.mgz", package = "fsbrain", mustWork = TRUE);
    rh_demo_cluster_file = system.file("extdata", "rh.clusters_fsaverage.mgz", package = "fsbrain", mustWork = TRUE);
    lh_clust = freesurferformats::read.fs.morph(lh_demo_cluster_file);   # contains a single positive cluster (activation, group difference), the other values are 0
    rh_clust = freesurferformats::read.fs.morph(rh_demo_cluster_file);   # contains two negative clusters
    vis.symmetric.data.on.subject(subjects_dir, subject_id, lh_clust, rh_clust, bg="curv", makecmap_options = list('range' = c(-8, 8)));

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
    close.all.rgl.windows();
})



test_that("We can visualize arbitrary data on a subjects surface using a single data vector for both hemispheres.", {
    testthat::skip_on_cran();
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';

    num_verts_subject1_lh = 149244;  # We need to know these to generate random data of suitable length.
    num_verts_subject1_rh = 153333;

    morph_data_both = rnorm((num_verts_subject1_lh + num_verts_subject1_rh), 2.0, 1.0);

    # Check:
    vis.data.on.subject(subjects_dir, subject_id, morph_data_both=morph_data_both, rgloptions = rglot(), draw_colorbar = T);
    vis.symmetric.data.on.subject(subjects_dir, subject_id, morph_data_both=morph_data_both, rgloptions = rglot(), draw_colorbar = T);

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
    close.all.rgl.windows();
})


test_that("We can retrieve vertex counts for a subject.", {
    testthat::skip_on_cran();
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';

    num_verts_subject1_lh = 149244L;  # We need to know these to generate random data of suitable length.
    num_verts_subject1_rh = 153333L;

    nv = subject.num.verts(subjects_dir, subject_id);
    expect_equal(nv$lh, num_verts_subject1_lh);
    expect_equal(nv$rh, num_verts_subject1_rh);

    expect_equal(subject.num.verts(subjects_dir, subject_id, do_sum = TRUE), (num_verts_subject1_lh + num_verts_subject1_rh));
    expect_equal(subject.num.verts(subjects_dir, subject_id, hemi = 'lh'), num_verts_subject1_lh);
    expect_equal(subject.num.verts(subjects_dir, subject_id, hemi = 'rh'), num_verts_subject1_rh);
})


test_that("We can visualize meshes using vis.fs.surface as expected.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';

    # Load data and surfaces
    sf_lh = subject.surface(subjects_dir, subject_id, 'white', 'lh');
    sf_rh = subject.surface(subjects_dir, subject_id, 'white', 'rh');
    morph_lh = subject.morph.native(subjects_dir, subject_id, 'thickness', 'lh');
    morph_rh = subject.morph.native(subjects_dir, subject_id, 'thickness', 'rh');


    # Visualize a single lh mesh with morph data
    cm = vis.fs.surface(sf_lh, per_vertex_data = morph_lh, hemi='lh');
    expect_equal(length(cm), 1L);
    expect_false(is.null(cm$lh));

    # Visualize a single rh mesh with color data
    cm = vis.fs.surface(sf_rh, col = 'green', hemi='rh');
    expect_equal(length(cm), 1L);
    expect_false(is.null(cm$rh));

    # Visualize both hemispheres by passing hemilists
    cm = vis.fs.surface(list('lh'=sf_lh, 'rh'=sf_rh), col = 'green', hemi='both');
    expect_equal(length(cm), 2L);
    expect_false(is.null(cm$lh));
    expect_false(is.null(cm$rh));

    # Visualize both hemispheres by passing hemilists, this time use morph data instead of colors
    cm = vis.fs.surface(list('lh'=sf_lh, 'rh'=sf_rh), per_vertex_data=list('lh'=morph_lh, 'rh'=morph_rh), hemi='both');
    expect_equal(length(cm), 2L);
    expect_false(is.null(cm$lh));
    expect_false(is.null(cm$rh));

    # Errors should be thrown if parameters make no sense
    expect_error(vis.fs.surface(list('lh'=sf_lh, 'rh'=sf_rh), per_vertex_data=list('lh'=morph_lh, 'rh'=morph_rh), hemi='lh')); # hemilist passed for surface but hemi is not both
    expect_error(vis.fs.surface(sf_lh, per_vertex_data=list('lh'=morph_lh, 'rh'=morph_rh), hemi='lh')); # hemilist passed for per_vertex_data but hemi is not both

    close.all.rgl.windows();
})


test_that("We can shift hemis apart, e.g. for inflated where lh and rh intersect.", {
    testthat::skip_on_cran();
    cm = get.demo.coloredmeshes.hemilist();
    cm_shifted = shift.hemis.apart(cm);
    cm_shifted2 = shift.hemis.apart(cm, hemi_order_on_axis = 'auto');
    cm_shifted3 = shift.hemis.apart(cm, hemi_order_on_axis = 'auto_flipped');
    cm_shifted4 = shift.hemis.apart(cm, hemi_order_on_axis = 'lr');
    cm_shifted5 = shift.hemis.apart(cm, hemi_order_on_axis = 'rl');

    # error handling
    expect_error(shift.hemis.apart(cm, axis = -1L));  # invalid axis
    expect_error(shift.hemis.apart(cm, hemi_order_on_axis = "noidea"));  # invalid hemi order
})
