# These tests are to be run manually and interactively, they are therefore skipped by default.
# You can run them by copying & pasting the code into an R session. Treat it as examples.

test_that("We can visualize morphometry data.", {
    skip("This test has to be run manually and interactively.");

    nitools::download_optional_data();

    subjects_dir = nitools::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';
    measure = 'thickness';
    surface = 'white';

    vis.subject.morph.native(subjects_dir, subject_id, measure, 'both');
})


test_that("We can visualize annotation atlas data.", {
    skip("This test has to be run manually and interactively.");

    nitools::download_optional_data();

    subjects_dir = nitools::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';

    vis.subject.annot(subjects_dir, subject_id, 'aparc', 'both');
})


test_that("We can visualize arbitrary data on a subjects surface.", {
    skip("This test has to be run manually and interactively.");

    nitools::download_optional_data();

    subjects_dir = nitools::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';

    num_verts_subject1_lh = 149244;  # We need to know these to generate random data of suitable length.
    num_verts_subject1_rh = 153333;

    morph_data_lh = rnorm(num_verts_subject1_lh, 2.0, 1.0);
    morph_data_rh = rnorm(num_verts_subject1_rh, 2.0, 1.0);

    vis.data.on(subjects_dir, subject_id, morph_data_lh, morph_data_rh);
})




