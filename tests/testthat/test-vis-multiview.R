# These tests are to be run manually and interactively, they are therefore skipped by default.
# You can run them by copying & pasting the code into an R session. Treat them as examples.

test_that("We can visualize morphometry data in multiview.", {
    skip("This test has to be run manually and interactively.");

    fsbrain::download_optional_data();

    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';
    measure = 'thickness';
    surface = 'white';

    rgloptions=list("windowRect"=c(50,50,900,900));     # window at position (50,50) on screen, width and height 900

    coloredmeshes = vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', views=c('si', 't4', 't9'), rgloptions=rgloptions);
    coloredmeshes = vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', views=c('sr'));
    #vis.mult.coloredmeshes(coloredmeshes, background="white", skip_all_na=TRUE);
    brainview.t4(coloredmeshes);
    brainview.t9(coloredmeshes);

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

