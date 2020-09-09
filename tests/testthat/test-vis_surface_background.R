# Tests for surface color layers.

test_that("A mean curvature color layer can be loaded", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires the full test data and X11.");
    subjects_dir = testdatapath.subjectsdir.full.subject1();

    subject_id = "subject1";

    bgcol = collayer.bg.meancurv(subjects_dir, subject_id);

    num_verts_subject1_lh = 149244;
    num_verts_subject1_rh = 153333;
    expect_equal(length(bgcol$lh), num_verts_subject1_lh);
    expect_equal(length(bgcol$rh), num_verts_subject1_rh);

    vis.color.on.subject(subjects_dir, subject_id, bgcol$lh, bgcol$rh, surface="inflated");
    close.all.rgl.windows();
})


test_that("Color layers can be merged", {
    collayers = list();
    collayers$background = rep('#000000', 100L);
    collayers$foreground = rep('#ff0000', 100L);
    collayers$foreground[30:50] = NA;           # set NA as color, this will be treated as fully transparent
    collayers$foreground[70:90] = '#ffffff00';  # set fully transparent color
    collayers$foreground[10:20] = '#ffffff77';  # set partly transparent color, requires alpha blending

    merged_layer = collayers.merge(collayers);

    expect_equal(length(merged_layer), 100L);
    expect_equal(merged_layer[30:50], rep('#000000FF', 21L));
    expect_equal(merged_layer[70:90], rep('#000000FF', 21L));
})


test_that("Alphablending works", {

    front_color = c('#ff000044', '#00ff0088', NA, '#555555');
    back_color = c('#000000', '#000000', '#0000ff', NA);

    blended = alphablend(front_color, back_color);

    expect_equal(blended[1], '#440000FF');  # partially transparent red on opaque white background becomes reddish
    expect_equal(blended[2], '#008800FF');  # partially transparent green on opaque white background becomes greenish
    expect_equal(blended[3], '#0000FFFF');  # no color/full transparency on blue background becomes blue
    expect_equal(blended[4], '#555555FF');  # gray on fully transparent background becomes gray
})


test_that("An annotation-based or atlas color layer can be created", {

    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");

    num_verts_subject1_lh = 149244L;
    num_verts_subject1_rh = 153333L;

    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    annot_layer = collayer.from.annot(subjects_dir, 'subject1', 'both', 'aparc');

    expect_true(is.list(annot_layer));
    expect_equal(names(annot_layer), c("lh", "rh"));
    expect_equal(length(annot_layer$lh), num_verts_subject1_lh);
    expect_equal(length(annot_layer$rh), num_verts_subject1_rh);
    expect_equal(length(unique(annot_layer$lh)), 35L); # number of aparc atlas regions for subject
    expect_equal(length(unique(annot_layer$rh)), 35L);

    #vis.color.on.subject(subjects_dir, 'subject1', annot_layer$lh, annot_layer$rh);
    #vis.color.on.subject(subjects_dir, 'subject1', desaturate(annot_layer$lh), desaturate(annot_layer$rh));
})



test_that("An outline layer based on an annotation can be created", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires the full test data and X11.");
    subjects_dir = testdatapath.subjectsdir.full.subject1();

    fsbrain::download_optional_data();

    num_verts_subject1_lh = 149244L;
    num_verts_subject1_rh = 153333L;

    outline_layer = collayer.bg.atlas(subjects_dir, 'subject1', outline = TRUE, grayscale = FALSE);

    expect_true(is.list(outline_layer));
    expect_equal(names(outline_layer), c("lh", "rh"));
    expect_equal(length(outline_layer$lh), num_verts_subject1_lh);
    expect_equal(length(outline_layer$rh), num_verts_subject1_rh);
    expect_equal(length(unique(outline_layer$lh)), 35L); # number of aparc atlas regions for subject
    expect_equal(length(unique(outline_layer$rh)), 35L);

    vis.color.on.subject(subjects_dir, 'subject1', outline_layer$lh, outline_layer$rh, surface = "inflated");
    close.all.rgl.windows();
})


test_that("We can visualize meshes using vis.fs.surface as expected.", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';

    col_aparc = collayer.bg(subjects_dir, subject_id, "aparc");

    skip_if_not(box.has.full.subject1(), message = "Full recon-all output for subject1 required");
    subjects_dir = testdatapath.subjectsdir.full.subject1();
    col_curv = collayer.bg(subjects_dir, subject_id, "curv");
    col_curv_light = collayer.bg(subjects_dir, subject_id, "curv_light");
    col_sulc = collayer.bg(subjects_dir, subject_id, "sulc");
    col_sulc_light = collayer.bg(subjects_dir, subject_id, "sulc_light");
})
