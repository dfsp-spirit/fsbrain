test_that("Label border can be computed", {
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = 'subject1';
    surface = 'white';
    hemi = 'lh';
    atlas = 'aparc';
    region = 'bankssts';

    # Create a label
    lh_annot = subject.annot(subjects_dir, subject_id, hemi, atlas);
    lh_label = label.from.annotdata(lh_annot, region);

    # Load a surface
    lh_surf = subject.surface(subjects_dir, subject_id, surface, hemi);

    lh_label_border = label.border(lh_surf, lh_label);
    #vis.labeldata.on.subject(subjects_dir, subject_id, lh_label_border$vertices, NULL);
    expect_equal(length(lh_label_border$vertices), 188);
})


test_that("Label border can be computed, thickened and visualized", {
    skip("This test has to be run manually and interactively.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = 'subject1';
    surface = 'white';
    hemi = 'lh';

    # Load surface mesh
    mesh = subject.surface(subjects_dir, subject_id, surface, hemi);

    # Create 3 labels. We just use random points and grow a neighborhood around them.
    l1 = mesh.vertex.neighbors(mesh, c(121543), k=5);
    l2 = mesh.vertex.neighbors(mesh, c(83862), k=5);
    l3 = mesh.vertex.neighbors(mesh, c(46324), k=5);


    l2_border = label.border(mesh, l2$vertices);
    l3_border_thick = label.border(mesh, l3$vertices, expand_inwards=2L);

    vis.labeldata.on.subject(subjects_dir, subject_id, c(l1$vertices, l2_border$vertices, l3_border_thick$vertices), NULL, surface = "inflated");
})
