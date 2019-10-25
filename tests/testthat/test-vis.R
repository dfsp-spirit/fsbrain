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
    region_names_aparc = get.atlas.region.names('aparc', template_subjects_dir = subjects_dir, template_subject = subject_id);

    # Get some data to display. Here we use random data as an example. You would put something like the mean of some morphometry measure, p value, effect size, or whatever.
    lh_region_value_list = as.list(rnorm(length(region_names_aparc), mean=5, sd=1.5)); # assign random normal values to regions
    names(lh_region_value_list) = region_names_aparc;    # Assign the names to the values.
    rh_region_value_list = as.list(rnorm(length(region_names_aparc), mean=5.5, sd=1.8));
    names(rh_region_value_list) = region_names_aparc;

    morph_data = spread.values.over.subject(subjects_dir, subject_id, atlas, lh_region_value_list, rh_region_value_list, value_for_unlisted_regions=NaN);
    vis.data.on.subject(subjects_dir, subject_id, morph_data$lh, morph_data$rh);

    # Test that we can pass NULL data, which should not render that hemisphere.
    morph_data2 = spread.values.over.subject(subjects_dir, subject_id, atlas, NULL, rh_region_value_list, value_for_unlisted_regions=NaN);
    vis.data.on.subject(subjects_dir, subject_id, morph_data2$lh, morph_data2$rh);
})


