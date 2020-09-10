test_that("Label border can be computed", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
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
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11 and all test data.");

    fsbrain::download_optional_data();

    subjects_dir = testdatapath.subjectsdir.full.subject1();
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

    # Another way to visualize this would be by constructing a mask from several labels. Or by merging them into an annotation:
    label_vertices_by_region = list("region1"=l1$vertices, "region2"=l2_border$vertices, "region3"=l3_border_thick$vertices);
    annot = label.to.annot(label_vertices_by_region, nrow(mesh$vertices));
    vis.subject.annot(subjects_dir, subject_id, annot, hemi, surface = "inflated");

    expect_equal(1L, 1L);   # empty tests will be skipped
})


test_that("The borders of all annotation regions can be computed", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11 and takes a while.");

    fsbrain::download_optional_data();
    subjects_dir = testdatapath.subjectsdir.full.subject1();
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = 'subject1';
    surface = 'inflated';
    hemi = 'lh';

    # Load surface mesh
    annot = subject.annot(subjects_dir, subject_id, hemi, "aparc");
    mesh = subject.surface(subjects_dir, subject_id, surface, hemi);
    vertex_colors = annot.outline(annot, mesh);  # What we came for: compute outlines of all annot regions


    # We could show morphometry data (or whatever) in the white inner parts, but that is a bit overkill imo.
    # It is still demonstrated here:
    show_background_morph = TRUE;
    if(show_background_morph) {
        ct = subject.morph.native(subjects_dir, subject_id, "thickness", hemi);
        vertex_colors_thickness = adjustcolor(squash::cmap(ct, map = squash::makecmap(ct, colFn = squash::jet)), alpha.f = 0.5);
        wi = which(vertex_colors=="white");
        vertex_colors[vertex_colors=="white"] = vertex_colors_thickness[wi];
    }

    vis.color.on.subject(subjects_dir, subject_id, vertex_colors_thickness, NULL);

    expect_equal(1L, 1L);   # empty tests will be skipped
})


