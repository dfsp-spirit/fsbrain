# tests for shape descriptor computation

test_that("Shape descriptors can be computed", {
    testthat::skip_on_cran();

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';

    # Load data (see function surface.curvature for computing it in R)
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


test_that("Principal curvatures can be computed", {
    testthat::skip_on_cran();

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';

    # Load data (see function surface.curvature for computing it in R)
    lh_surf = subject.surface(subjects_dir, subject_id, surface = "white", hemi='lh');
    rh_surf = subject.surface(subjects_dir, subject_id, surface = "white", hemi='rh');
    lh_curvatures = surface.curvatures(lh_surf);
    rh_curvatures = surface.curvatures(rh_surf);

    # Get principal curvature info
    lh_pc = principal.curvatures(lh_curvatures$K1, lh_curvatures$K2);
    rh_pc = principal.curvatures(rh_curvatures$K1, rh_curvatures$K2);

    vis.data.on.subject(subjects_dir, subject_id, morph_data_lh = lh_pc$principal_curvature_k1, morph_data_rh = rh_pc$principal_curvature_k1, rglactions = list('trans_fun'=clip.data));
})

