# These tests are to be run manually and interactively, they are therefore skipped by default.
# You can run them by copying & pasting the code into an R session. Treat them as examples.

# A note on the plot size for all of the vis functions:
# You may want to set a globale default for windowrect, e.g.: `library('rgl'); r3dDefaults$windowRect <- c(50,50, 800, 800);`

test_that("We can visualize morphometry data in multiview.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires the full test data and X11.");

    fsbrain::download_optional_data();

    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';
    measure = 'thickness';
    surface = 'white';

    rgloptions=list("windowRect"=c(80,80,800,800));     # the first 2 entries give the position on screen, the rest defines resolution as width, height in px
    rglactions = list("snapshot_png"="~/fsbrain.png", "clip_data"=c(0.05, 0.95));
    rglactionsmovie = list("snapshot_png"="~/fsbrain.png", "movie"="brain_rot");

    coloredmeshes = vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', views=c('si', 't4', 't9'), rgloptions=rgloptions, rglactions=rglactions);
    #coloredmeshes = vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', views=c('si'));

    vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', views=c('t4'), draw_colorbar =TRUE, rgloptions = rgloptions, rglactions = list("snapshot_png"="~/brain_t4.png"));
    vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', views=c('t9'), rgloptions = rgloptions, rglactions = list("snapshot_png"="~/brain_t9.png"));

    vis.subject.annot(subjects_dir, subject_id, 'aparc', 'both', views=c('t4'), rgloptions = rgloptions, rglactions = list("snapshot_png"="~/annot_t4.png"));
    vis.subject.annot(subjects_dir, subject_id, 'aparc', 'both', views=c('t9'), rgloptions = rgloptions, rglactions = list("snapshot_png"="~/annot_t9.png"));

    #vis.subject.annot(subjects_dir, subject_id, 'aparc', 'both', views=c('sr'), rgloptions = rgloptions, rglactions = rglactionsmovie);

    do_rotations = FALSE;
    if(do_rotations) {
        cmesh = coloredmeshes[[1]]; # 1 = lh , 2 = rh

        # open3d(); shade3d(cmesh$mesh, col=cmesh$col);
        # rgl.viewpoint(0, 0, fov=0, interactive=FALSE, zoom=.9);

        ax = 1;
        ay = 0;
        az = 0;
        for(theta in c(0, 90, -90)) {
            for(phi in c(0, 90, -90)) {
                for(rotation_angle in c(0, pi, pi/2)) {
                    label_text = sprintf("[%d %d %d] theta=%d, phi=%d, a=%f", ax, ay, az, theta, phi, rotation_angle);
                    rgl::open3d(); rgl::shade3d(rgl::rotate3d(cmesh$mesh, rotation_angle, ax, ay, az), col=cmesh$col); rgl::rgl.viewpoint(theta, phi, fov=0, interactive=FALSE, type="modelviewpoint"); rgl::text3d(0, -150, 100, label_text);

                }
            }
        }
    }

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
    close.all.rgl.windows();

})


test_that("We can visualize p values or other arbitrary data, one value per atlas region.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires the full test data and X11.");
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
        rgloptions=list("windowRect"=c(80,80,800,800));
        rglactions = list("snapshot_png"="~/fsbrain_pvalues_fsavg.png");
        vis.region.values.on.subject(subjects_dir, subject, atlas, lh_region_value_list, rh_region_value_list, rgloptions=rgloptions, rglactions=rglactions);
    } else {
        message("Subject not found.");
    }
    testthat::expect_equal(1L, 1L);
    close.all.rgl.windows();
})


test_that("We can visualize data on fsaverage if available", {
    testthat::skip_on_cran(); # skip: leads to memory errors ('cannot allocate vector of size XX MB') on CRAN.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");

    fsaverage_dir = subjects_dir;

    rgloptions=list("windowRect"=c(50,50,800,800));
    rglactions = list();
    makecmap_options = list('colFn'=grDevices::terrain.colors);

    if(dir.exists(fsaverage_dir)) {
         vis.subject.morph.standard(subjects_dir, 'subject1', 'thickness', 'both', '10', template_subjects_dir=fsaverage_dir, rgloptions=rgloptions, rglactions=rglactions, draw_colorbar=T);
    } else {
        message("No fsaverage found.");
    }
    testthat::expect_equal(1L, 1L);
    close.all.rgl.windows();
})


test_that("We can visualize data on fsaverage3 if available", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_fsaverage3(accept_freesurfer_license = TRUE);
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");

    makecmap_options = list('colFn'=grDevices::terrain.colors, 'n'=100);

    vis.subject.morph.standard(subjects_dir, 'subject1', 'thickness', 'both', fwhm='0', template_subject='fsaverage3');
    testthat::expect_equal(1L, 1L);
    close.all.rgl.windows();
})


test_that("We can record a gif movie of a rotating brain.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(run.extralong.tests(), "This test takes ages.");

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
    testthat::expect_equal(1L, 1L);
    close.all.rgl.windows();
})


test_that("A label can be visualized.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.has.x11display(), "This test requires X11.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    subject_id = 'subject1';
    surface = 'white';
    hemi = 'both';
    label = 'cortex.label';

    vis.subject.label(subjects_dir, subject_id, label, hemi);

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
    close.all.rgl.windows();
})

test_that("A region from an atlas can be converted to a label and visualized.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires the full test data and X11.");
    subjects_dir = testdatapath.subjectsdir.full.subject1();

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

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
    close.all.rgl.windows();
})


test_that("We can visualize label data or arbitrary sets of vertices.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires the full test data and X11.");
    subjects_dir = testdatapath.subjectsdir.full.subject1();

    fsbrain::download_optional_data();
    subject_id = 'subject1';
    surface = 'inflated';  # If possible, use the 'inflated' surface instead: it is much easier to find the vertices on it. We do not
    #  use it here because the inflated surface is not shipped with the example data for this package to reduce download size.


    # For the left hemi, we just specify 3 vertices. They are very small in the high-resolution mesh and may be hard to spot.
    lh_labeldata = c(1000, 1001, 1002);

    # For the right hemi, we extend the neighborhood around our vertices of interest for the visualization. This makes the a lot easier to spot.
    rh_labeldata = c(5000)
    rh_surface = subject.surface(subjects_dir, subject_id, surface, 'rh');
    rh_labeldata_neighborhood = mesh.vertex.neighbors(rh_surface, rh_labeldata);   # extend neighborhood
    rh_labeldata_neighborhood = mesh.vertex.neighbors(rh_surface, rh_labeldata_neighborhood$vertices);   # extend neighborhood again

    # Hint: Check the area around the visual cortex when searching for the vertices in interactive mode.
    vis.labeldata.on.subject(subjects_dir, subject_id, lh_labeldata, rh_labeldata_neighborhood$vertices, views=c('si'), surface=surface);

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
    close.all.rgl.windows();
})


test_that("We can combine an output view with a separate colormap.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(run.extralong.tests(), "This test requires the full test data and X11, and takes ages.");
    subjects_dir = testdatapath.subjectsdir.full.subject1();

    fsbrain::download_optional_data();

    subject_id = 'subject1';
    measure = 'jacobian_white';
    measure_legend_text = "Jacobian white";
    surface = 'white';

    output_width = 1200; # in px
    output_height = output_width;
    cbar_height = output_height;  # We cannot set this much smaller without getting errors, we will instead crop the resulting image in imagemagick below.
    output_main_image = path.expand(sprintf("~/fsbrain_img_main_%s.png", measure));
    output_cbar_image = path.expand(sprintf("~/fsbrain_img_cbar_%s.png", measure));
    output_main_movie_file_noext = sprintf("fsbrain_mov_main_%s", measure);
    output_main_movie = sprintf("~/%s.gif", output_main_movie_file_noext);

    rgloptions=list("windowRect"=c(80, 80, output_width, output_height));
    rglactions = list("snapshot_png"=output_main_image, "movie"=output_main_movie_file_noext);

    # Some measures need a bit of cleanup:
    if(measure %in% c("curv", "thickness", "jacobian_white")) {
        rglactions$clip_data = c(0.05, 0.95);
    }

    coloredmeshes = vis.subject.morph.native(subjects_dir, subject_id, measure, 'both', views=c('sr'), rgloptions=rgloptions, rglactions=rglactions, cortex_only=TRUE);

    #coloredmesh.plot.colorbar.separate(coloredmeshes, png_options = list("filename"=output_cbar_image, "width"=output_width, "height"=cbar_height), image.plot_extra_options = list("legend.lab"=measure_legend_text, horizontal=TRUE, legend.cex=1.5, legend.line=-3));
    # You will have to manually export the cbar with the settings above. Programmatically saving it seems to result in missing colors in the bar
    # for some reason. UPDATE: This seems to happen only for some OpenGL implementations (i.e., it is hardware/system dependent). Try it on your machine.
    coloredmesh.plot.colorbar.separate(coloredmeshes, image.plot_extra_options = list("legend.lab"=measure_legend_text, horizontal=TRUE, legend.cex=1.5, legend.line=-3));

    combine.colorbar.with.brainview.animation(output_main_movie, output_cbar_image, "~/anim_with_cbar.gif");

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.


    ## The following are some ideas on how to combine the colorbar and another image using imagemagick.
    ## The colorbar shouldis displayed below the full image here.
    ## These are to be run on the command line of the OS, but we could turn them into R code with the 'magick' package later I guess.
    #
    # cd ~

    ## Remove the whitespace around the colorbar:
    # convert fsbrain_img_cbar.png -trim +repage fsbrain_img_cbar_min.png

    ## Vertically append the colorbar below the main image
    # convert fsbrain_img_main.png fsbrain_img_cbar_min.png -gravity center -background white -append fsbrain_img_combined.png


    ## Append the colorbar below the gif movie: this is a bit more tricky. We have to split the gif into its
    ## frames, append to them, then recombine.


    ## One may want to crop something from the top of the colorbar image before combining the images, to reduce white space.
    ## In this example, we remove the 20 px at the top.
    # convert fsbrain_img_cbar.png -gravity North -chop 1x80 fsbrain_img_cbar_cropped.gif
    # convert fsbrain_img_cbar_min.png fsbrain_img_cbar_min.gif

    ## Split the animated gif into frames:
    # convert fsbrain_mov_main.gif -coalesce frames-%03d.png
    ## We may want to crop a bit from the frames as well.
    ## In this example, we remove the 40 px at the bottom.
    # for FRAME in frames*; do convert $FRAME -gravity South -chop 1x40 $FRAME; done

    ## Now combine colorbar and image for each frame:
    # for FRAME in frames*; do montage $FRAME fsbrain_img_cbar_cropped.gif -tile 1x2 -geometry +0+0 -background white $FRAME; done

    ## And finally create a new animated gif from all the frames:
    # convert -delay 5 -loop 0 -layers optimize frames* fsbrain_mov_combined.gif

    ## This will give you the image and the rotating brain gif animation, both with a suitable colorbar at the bottom of the image/animation.
})


### Some ideas for creating an MP4 movie from the frames of the GIF animation (on the OS command line): ###
#
# 1) split the GIF into frames: convert anim_with_cbar.gif -coalesce frames-%03d.png
# 2) encode to MP4 using ffmpeg with libx264 codec: ffmpeg -framerate 20 -i frames-%03d.png -c:v libx264 -crf 18 -pix_fmt yuv420p brain_once.mp4 -y
# It may be better to make the video loop 3 times, so the user has more time to view it:
# 3a) for i in {1..3}; do printf "file '%s'\n" brain_once.mp4 >> vidlist.txt; done
# 3b) ffmpeg -f concat -i vidlist.txt -c copy brain_looped.mp4 -y
# If you intend to upload to youtube or other video streaming platforms, you may want to optimize the file for them to get good quality:
    # 4) ffmpeg -i brain_looped.mp4 -vf yadif,format=yuv420p -c:v libx264 -crf 18 -bf 2 -c:a aac -q:a 1 -ac 2 -ar 48000 -use_editlist 0 -movflags +faststart brain_looped_opt_streaming.mp4 -y


test_that("We can construct a tight layout image by merging several sd views.", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11 and imagemagick.");

    fsbrain::download_optional_data();

    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");

    view_angles = get.view.angle.names(angle_set = "t9");
    rgloptions=list("windowRect"=c(80,80,800,800));

    coloredmeshes = vis.subject.morph.native(subjects_dir, "subject1", "thickness", cortex_only=TRUE, rglactions=list("clip_data"=c(0.05, 0.95)), views=NULL);
    vislayout.from.coloredmeshes(coloredmeshes, view_angles = view_angles);

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
    close.all.rgl.windows();
})


test_that("View angle names can be retrieved", {
    expect_equal(get.view.angle.names("medial"), c("sd_medial_lh", "sd_medial_rh"));
    expect_equal(get.view.angle.names("lateral"), c("sd_lateral_lh", "sd_lateral_rh"));
    expect_equal(get.view.angle.names("lh"), c("sd_lateral_lh", "sd_medial_lh"));
    expect_equal(get.view.angle.names("rh"), c("sd_lateral_rh", "sd_medial_rh"));
    expect_equal(length(get.view.angle.names("all")), 8L);

    # check for expected errors
    expect_error(get.view.angle.names("no_such_set"));
})


test_that("We can shift hemis apart", {
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");

    cm_lh = coloredmesh.from.morph.native(subjects_dir, 'subject1', 'thickness', hemi='lh');
    cm_rh = coloredmesh.from.morph.native(subjects_dir, 'subject1', 'thickness', hemi='rh');
    cm_hemilist = list('lh'=cm_lh, 'rh'=cm_rh);

    cm_hl_shifted = shift.hemis.apart(cm_hemilist);
    expect_true(is.hemilist(cm_hl_shifted));
})
