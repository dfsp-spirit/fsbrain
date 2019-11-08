# These tests are to be run manually and interactively, they are therefore skipped by default.
# You can run them by copying & pasting the code into an R session. Treat them as examples.

test_that("We can visualize morphometry data in multiview.", {
    skip("This test has to be run manually and interactively.");

    fsbrain::download_optional_data();

    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';
    measure = 'thickness';
    surface = 'white';

    rgloptions=list("windowRect"=c(20,20,1900,1200));     # the first 2 entries give the position on screen, the rest defines resolution as width, height in px
    rglactions = list("snapshot_png"="~/fsbrain.png", "clip_data"=c(0.05, 0.95));
    rglactionsmovie = list("snapshot_png"="~/fsbrain.png", "movie"="brain_rot");

    coloredmeshes = vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', views=c('si', 't4', 't9'), rgloptions=rgloptions, rglactions=rglactions);
    coloredmeshes = vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', views=c('sr'));

    vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', views=c('t4'), rgloptions = rgloptions, rglactions = list("snapshot_png"="~/brain_t4.png"));
    vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', views=c('t9'), rgloptions = rgloptions, rglactions = list("snapshot_png"="~/brain_t9.png"));

    vis.subject.annot(subjects_dir, subject_id, 'aparc', 'both', views=c('t4'), rgloptions = rgloptions, rglactions = list("snapshot_png"="~/annot_t4.png"));
    vis.subject.annot(subjects_dir, subject_id, 'aparc', 'both', views=c('t9'), rgloptions = rgloptions, rglactions = list("snapshot_png"="~/annot_t9.png"));

    #vis.subject.annot(subjects_dir, subject_id, 'aparc', 'both', views=c('sr'), rgloptions = rgloptions, rglactions = rglactionsmovie);

    do_rotations = FALSE;
    if(do_rotations) {
        cmesh = coloredmeshes[[1]]; # 1 = lh , 2 = rh

        # open3d(); shade3d(cmesh$mesh, col=cmesh$col);
        # rgl.viewpoint(0, 0, fov=0, interactive=FALSE, zoom=.9);

        #r3dDefaults$windowRect <- c(0,50, 1200, 1200)

        ax = 1;
        ay = 0;
        az = 0;
        for(theta in c(0, 90, -90)) {
            for(phi in c(0, 90, -90)) {
                for(rotation_angle in c(0, pi, pi/2)) {
                    label_text = sprintf("[%d %d %d] theta=%d, phi=%d, a=%f", ax, ay, az, theta, phi, rotation_angle);
                    open3d(); shade3d(rotate3d(cmesh$mesh, rotation_angle, ax, ay, az), col=cmesh$col); rgl.viewpoint(theta, phi, fov=0, interactive=FALSE, type="modelviewpoint"); text3d(0, -150, 100, label_text);

                }
            }
        }
    }

})


test_that("We can visualize p values or other arbitrary data, one value per atlas region.", {
    skip("This test has to be run manually and interactively.");
    fsbrain::download_optional_data();

    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    atlas = 'aparc';

    subject = 'fsaverage';

    # lh: Manually set data values for some regions (the rest will be assigned the default value for NaNs, which can be set as a parameter of the spread.values.over.subject function.):
    lh_region_value_list = list("bankssts"=0.9, "precuneus"=0.7, "postcentral"=0.8, "lingual"=0.6);

    # rh: retrieve the full list of regions for the atlas, and assign random values for this example:
    atlas_region_names = get.atlas.region.names(atlas, template_subjects_dir = subjects_dir, template_subject=subject);
    rh_region_value_list = rnorm(length(atlas_region_names), 3.0, 1.0);
    names(rh_region_value_list) = atlas_region_names;


    if(dir.exists(file.path(subjects_dir, subject))) {
        rgloptions=list("windowRect"=c(0,0,1200,1200), mar=c(0,0,0,0));
        rglactions = list("snapshot_png"="~/fsbrain_pvalues_fsavg.png");
        vis.region.values.on.subject(subjects_dir, subject, atlas, lh_region_value_list, rh_region_value_list, rgloptions=rgloptions, rglactions=rglactions);
    } else {
        message("Subject not found.");
    }
})


test_that("We can visualize data on fsaverage if available", {
    skip("This test has to be run manually and interactively.");
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");

    #fsaverage_dir = file.path(Sys.getenv('FREESURFER_HOME'), 'subjects');
    fsaverage_dir = subjects_dir;

    rgloptions=list("windowRect"=c(50,50,1200,1200));     # the first 2 entries give the position on screen, the rest defines resolution as width, height in px
    rglactions = list("snapshot_png"="~/fsbrain_t4_fsavg.png");

    if(dir.exists(fsaverage_dir)) {
         vis.subject.morph.standard(subjects_dir, 'subject1', 'thickness', 'both', '10', template_subjects_dir=fsaverage_dir, rgloptions=rgloptions, rglactions=rglactions);
    } else {
        message("No fsaverage found.");
    }

})


test_that("We can record a gif movie of a rotating brain.", {
    skip("This test has to be run manually and interactively.");

    fsbrain::download_optional_data();

    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';
    rgloptions=list("windowRect"=c(50,50,600,600));     # the first 2 entries give the position on screen, the rest defines resolution as width, height in px
    surface = 'white';

    measures_native = c('volume', 'thickness', 'curv', 'pial_lgi', 'truncation', 'sulc', 'area');
    for (measure in measures_native) {
        movie_base_filename = sprintf("brain_rot_%s_%s_%s", subject_id, measure, surface);
        rglactions = list("movie"=movie_base_filename, "clip_data"=c(0.05, 0.95)); # The "movie" action will write the movie to a file named "brain_rot.gif" in your HOME.
        # Creating a movie requires the rotating view ('sr' for 'single rotating'). The action will be silently ignored in all other views.
        vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', views=c('sr'), rgloptions=rgloptions, rglactions=rglactions);
    }

    ### Now make an annotation movie
    atlases = c('aparc', 'aparc.a2009s');
    for (atlas in atlases) {
        rglactions = list("movie"=sprintf("brain_rot_%s_%s_%s", subject_id, atlas, surface));
        vis.subject.annot(subjects_dir, subject_id, atlas, 'both', views=c('sr'), rgloptions=rgloptions, rglactions=rglactions);
    }



    # Creating a movie from standard space data (mapped to fsaverage, and displayed on fsaverage):
    measures_std = c('volume', 'thickness', 'curv', 'pial_lgi', 'truncation', 'sulc', 'area');
    #subjects_dir = path.expand("~/data/tim_only")
    fwhm = '10';
    for (measure in measures_std) {
        rglactions = list("movie"=sprintf("fsbrain_rot_%s_std_fwhm%s", measure, fwhm));
        vis.subject.morph.standard(subjects_dir, subject_id, measure, 'both', fwhm, views=c('sr'), rgloptions=rgloptions, rglactions=rglactions);
    }
})


test_that("A label can be visualized.", {
    skip("This test has to be run manually and interactively.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';
    surface = 'white';
    hemi = 'both';
    label = 'cortex.label';

    vis.subject.label(subjects_dir, subject_id, label, hemi);
})

test_that("A region from an atlas can be converted to a label and visualized.", {
    skip("This test has to be run manually and interactively.");

    fsbrain::download_optional_data();

    # Define the data to use:
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';
    surface = 'white';
    hemi = 'both';
    atlas = 'aparc';
    region = 'bankssts';

    # Create a mask from a region of an annotation:
    lh_annot = subject.annot(subjects_dir, subject_id, 'lh', atlas);
    rh_annot = subject.annot(subjects_dir, subject_id, 'rh', atlas);
    lh_label = label.from.annotdata(lh_annot, region);
    rh_label = label.from.annotdata(rh_annot, region);
    lh_mask = mask.from.labeldata.for.hemi(lh_label, length(lh_annot$vertices));
    rh_mask = mask.from.labeldata.for.hemi(rh_label, length(rh_annot$vertices));
    vis.mask.on.subject(subjects_dir, subject_id, lh_mask, rh_mask);

    # Edit the mask: add the vertices from another region to it:
    region2 = 'medialorbitofrontal';
    lh_label2 = label.from.annotdata(lh_annot, region2);
    rh_label2 = label.from.annotdata(rh_annot, region2);
    lh_mask2 = mask.from.labeldata.for.hemi(lh_label2, length(lh_annot$vertices), existing_mask = lh_mask);
    rh_mask2 = mask.from.labeldata.for.hemi(rh_label2, length(rh_annot$vertices), existing_mask = rh_mask);
    # Visualize the mask:
    vis.mask.on.subject(subjects_dir, subject_id, lh_mask2, rh_mask2);
})


