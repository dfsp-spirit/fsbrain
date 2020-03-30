# These tests are to be run manually and interactively, they are therefore skipped by default.
# You can run them by copying & pasting the code into an R session. Treat it as examples.

test_that("We can visualize morphometry data.", {
    skip("This test has to be run manually and interactively.");

    fsbrain::download_optional_data();

    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';
    measure = 'thickness';
    surface = 'white';

    vis.subject.morph.native(subjects_dir, subject_id, measure, 'both');
})


test_that("We can visualize annotation atlas data.", {
    skip("This test has to be run manually and interactively.");

    fsbrain::download_optional_data();

    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';

    vis.subject.annot(subjects_dir, subject_id, 'aparc', 'both');
})


test_that("We can visualize arbitrary data on a subjects surface.", {
    skip("This test has to be run manually and interactively.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';

    num_verts_subject1_lh = 149244;  # We need to know these to generate random data of suitable length.
    num_verts_subject1_rh = 153333;

    morph_data_lh = rnorm(num_verts_subject1_lh, 2.0, 1.0);
    morph_data_rh = rnorm(num_verts_subject1_rh, 2.0, 1.0);

    vis.data.on.subject(subjects_dir, subject_id, morph_data_lh, morph_data_rh);
})


test_that("We can visualize arbitrary data on the fsaverage surfaces if available.", {
    skip("This test has to be run manually and interactively.");

    subjects_dir_query = find.subjectsdir.of(subject_id='fsaverage', mustWork = FALSE);
    if(subjects_dir_query$found) {
        subjects_dir = subjects_dir_query$found_at;
    } else {
        skip("The environment variables FREESURFER_HOME and SUBJECTS_DIR are not set. FreeSurfer is not installed correctly, or maybe you are running this from within a GUI program (like rstudio) started from an environment that does not have them available.");
    }

    num_verts_fsaverage = 163842;

    morph_data_lh = rnorm(num_verts_fsaverage, 2.0, 1.0);
    morph_data_rh = rnorm(num_verts_fsaverage, 2.0, 1.0);

    vis.data.on.fsaverage(morph_data_lh, morph_data_rh);
})


test_that("We can visualize one value per atlas region on a subject.", {
    skip("This test has to be run manually and interactively.");

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
})


test_that("We can visualize one value per Desikan atlas region on fsaverage.", {
    skip("This test has to be run manually and interactively. It requires fsaverage subject from a FreeSurfer installation.");

    subjects_dir = file.path("~/software/freesurfer/subjects"); # Directory that has 'fsaverage' data, adapt this to your machine.
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
    vis.data.on.subject(subjects_dir, subject_id, morph_data$lh, morph_data$rh, colormap=grDevices::heat.colors);

    # Of course, you can also save a file with your data for visualization in other software, if you prefer:
    freesurferformats::write.fs.morph("~/lh.regiondata.mgz", morph_data$lh);
    freesurferformats::write.fs.morph("~/rh.regiondata.mgz", morph_data$rh);
})


test_that("We can visualize a subset of the regions of the Desikan atlas on fsaverage.", {
    skip("This test has to be run manually and interactively. It requires the 'fsaverage' subject from a FreeSurfer installation.");

    subjects_dir = file.path("~/software/freesurfer/subjects"); # Directory that has 'fsaverage' data, adapt this to your machine.
    subject_id = 'fsaverage';                                   # You could visualize on any other subject, of course.

    atlas = "aparc";    # An atlas file name, e.g., 'aparc' for Desikan-Killiany, 'aparc.a2009s' for Destrieux, 'aparc.DKTatlas' for DKT40 (see https://surfer.nmr.mgh.harvard.edu/fswiki/CorticalParcellation)

    # Get all valid region names of the atlas:
    atlas_region_names = get.atlas.region.names(atlas, template_subjects_dir = subjects_dir, template_subject = subject_id);
    print(atlas_region_names);

    # Select some regions we want to show. We assign the same value to each region, so all get the same color.
    lh_region_value_list = c("precuneus"=1, "pericalcarine"=1, "temporalpole"=1, "bankssts"=1, "superiorparietal"=1);
    rh_region_value_list = c("parahippocampal"=1, "parsopercularis"=1, "lingual"=1, "inferiortemporal"=1);

    # Assign the values to all region vertices:
    morph_data = spread.values.over.subject(subjects_dir, subject_id, atlas, lh_region_value_list, rh_region_value_list, value_for_unlisted_regions=NaN);

    # We can visualize the data directly in fsbrain:
    vis.data.on.subject(subjects_dir, subject_id, morph_data$lh, morph_data$rh, colormap=grDevices::grey.colors);

    # Of course, you can also save a file with your data for visualization in other software, if you prefer:
    freesurferformats::write.fs.morph("~/lh.regiondata.mgz", morph_data$lh);
    freesurferformats::write.fs.morph("~/rh.regiondata.mgz", morph_data$rh);
})



test_that("We can visualize a cluster or activation data on fsaverage.", {
    skip("This test has to be run manually and interactively. It requires the 'fsaverage' subject from a FreeSurfer installation.");

    subjects_dir = file.path("~/software/freesurfer/subjects"); # Directory that has 'fsaverage' data, adapt this to your machine.
    subject_id = 'fsaverage';

    demo_cluster_file = system.file("extdata", "clusters_fsaverage.mgz", package = "fsbrain", mustWork = TRUE);
    lh_clust = freesurferformats::read.fs.morph(demo_cluster_file);   # contains a single positive cluster (activation, group difference), the other values are 0
    rh_clust = - 2 * lh_clust;                                        # create negative data, take multiple so the data are not symmetric around zero (because that is not typically the case and needs to be handled properly by the plot function)
    vis.symmetric.data.on.subject(subjects_dir, subject_id, lh_clust, rh_clust);
})

