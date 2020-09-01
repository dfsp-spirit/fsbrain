# tests for shape descriptor computation

test_that("Shape descriptors can be computed", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';

    # Load data
    k1 = subject.morph.native(subjects_dir, subject_id, 'white.max', hemi='both');
    k2 = subject.morph.native(subjects_dir, subject_id, 'white.min', hemi='both');

    # Get principal curvature info
    pc = principal.curvatures(k1, k2);

    # Visualize k1
    vis.data.on.subject(subjects_dir, subject_id, morph_data_both = pc$principal_curvature_k1, rglactions = list('trans_fun'=clip.data));

    # Compute shape descriptors
    shape = shape.descriptors(pc);
    expect_true(is.data.frame(shape));

    # error handling
    expect_error(shape.descriptors(pc, descriptors = c('noidea'))); # invalid descriptor

    vis.data.on.subject(subjects_dir, subject_id, morph_data_both = shape$shape_index, rglactions = list('trans_fun'=clip.data));
})
