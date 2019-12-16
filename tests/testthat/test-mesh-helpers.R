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
    vis.labeldata.on.subject(subjects_dir, subject_id, lh_label_border$vertices, NULL);
})
