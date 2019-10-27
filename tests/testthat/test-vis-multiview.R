# These tests are to be run manually and interactively, they are therefore skipped by default.
# You can run them by copying & pasting the code into an R session. Treat them as examples.

test_that("We can visualize morphometry data in multiview.", {
    skip("This test has to be run manually and interactively.");

    fsbrain::download_optional_data();

    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';
    measure = 'thickness';
    surface = 'white';

    rgloptions=list("windowRect"=c(50,50,1200,1200));     # the first 2 entries give the position on screen, the rest defines resolution as width, height in px
    rglactions = list("snapshot_png"="~/fsbrain.png");
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

    # lh: Manually set data values for some regions (the rest will be assigned the default value, see below):
    lh_region_value_list = list("bankssts"=0.9, "precuneus"=0.7, "postcentral"=0.8, "lingual"=0.6);

    # rh: retrieve the full list of regions for the atlas, and assign random values for this example:
    atlas_region_names = get.atlas.region.names('aparc', template_subjects_dir = fsaverage_dir);
    rh_region_value_list = rnorm(length(atlas_region_names), 3.0, 1.0);
    names(rh_region_value_list) = atlas_region_names;

    morph_like_data = spread.values.over.subject(subjects_dir, 'subject1', 'aparc', lh_region_value_list, rh_region_value_list);

    vis.data.on.subject(subjects_dir, 'subject1', morph_like_data$lh, morph_like_data$rh, colormap=squash::heat);
})

