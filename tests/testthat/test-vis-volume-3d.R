

test_that("A brain volume and an overlay can be merged", {

    skip("This test has to be run manually and interactively. It requires an aseg.mgz file for the demo subject.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    aseg = subject.volume(subjects_dir, subject_id, 'aseg');    # Not shipped with the package atm.
    ventricle_aseg_codes = c(4, 14, 15, 43);    # see FreeSurferColorLUT.txt
    ventricle_mask = vol.mask.from.segmentation(aseg, ventricle_aseg_codes);

    volvis.voxels(ventricle_mask);

    # ---Draw a cube---
    #cube_vertices = matrix(c(0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1), ncol=3);
    #cube = rgl::cube3d(vertices=cube_vertices);
    #rgl::shade3d(rgl::translate3d(cube, 5, 5, 5));

    # ---Use triangles to quickly draw many cubes---
    tris_cube = matrix(c(-0.5, -0.5, 0, # the 2 front tris start
                         -0.5, -0.5, 1,
                         0.5, -0.5, 0,
                         # tris 2
                         0.5,-0.5,0,
                         -0.5, -0.5,1,
                         0.5,-0.5,1,
                         # tris 3: the back starts (shifted by +1.0 on y axis compared to front)
                         -0.5, 0.5, 0,
                         -0.5, 0.5, 1,
                         0.5, 0.5, 0,
                         # tris 4
                         0.5,0.5,0,
                         -0.5, 0.5,1,
                         0.5,0.5,1,
                         # tris 5: the left side starts
                         -0.5,0.5,0,
                         -0.5,0.5,1,
                         -0.5,-0.5,0,
                         # tris 6
                         -0.5,-0.5,0,
                         -0.5,0.5,1,
                         -0.5,-0.5,1,
                         # tris 7: the right side starts (shifted by +1.0 on x axis compared to left side)
                         0.5,0.5,0,
                         0.5,0.5,1,
                         0.5,-0.5,0,
                         # tris 8
                         0.5,-0.5,0,
                         0.5,0.5,1,
                         0.5,-0.5,1,
                         # tris 9: the bottom starts
                         -0.5,-0.5,0,
                         -0.5,0.5,0,
                         0.5,-0.5,0,
                         # tris 10
                         0.5,-0.5,0,
                         -0.5,0.5,0,
                         0.5,0.5,0,
                         # tris 11: the top starts (shifted by +1.0 on z axis compared to the bottom)
                         -0.5,-0.5,1,
                         -0.5,0.5,1,
                         0.5,-0.5,1,
                         # tris 10
                         0.5,-0.5,1,
                         -0.5,0.5,1,
                         0.5,0.5,1
                         ), ncol=3, byrow = TRUE);          # create a cube with coords in range (0,0,0) to (1,1,1).


    #rgl::triangles3d(tris_coords);
})
