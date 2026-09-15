

test_that("A brain volume or parts of it can be rendered in voxel mode", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if_rgl_required();
    skip_if_rgl_window_required();
    testthat::skip_on_travis(); # Reduce test time on travis to prevent the build from being killed.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11 and all test data.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    aseg = subject.volume(subjects_dir, subject_id, 'aseg');    # Not shipped with the package atm.
    ventricle_aseg_codes = c(4, 14, 15, 43);    # see FreeSurferColorLUT.txt
    ventricle_mask = vol.mask.from.segmentation(aseg, ventricle_aseg_codes);

    volvis.voxels(ventricle_mask, render_every = 10);

    # Some more segmentation ROIs to play with:
    wm_mask = vol.mask.from.segmentation(aseg, c(2, 41));
    cortex_mask = vol.mask.from.segmentation(aseg, c(3, 42));

    # Use voxel colors when rendering: gray-scale, computed from the intensity values of the volume itself:
    volvis.voxels(ventricle_mask, voxelcol = 'from_intensity', render_every = 6);

    # Use voxel colors when rendering: based on a colormap.
    coloredvoxels = volvis.voxels(ventricle_mask, voxelcol = vol.overlay.colors.from.activation(ventricle_mask), render_every = 1);

    render_animation = FALSE;
    if(render_animation) {
        rgloptions=list("windowRect"=c(80,80,800,800));
        rglactions = list("movie"="vox_ventricles_rot");
        vislayout.from.coloredmeshes(coloredvoxels, view_angles="sr", rgloptions = rgloptions, rglactions = rglactions);
    }

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.

})


test_that("A test volume or parts of it can be rendered in voxel mode", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if_rgl_required();
    skip_if_rgl_window_required();
    myvol = gen.test.volume(c(40, 40, 40), bg = NA);
    volvis.voxels(myvol, render_every = 10);
    volvis.voxels(myvol, render_every = 10, voxelcol = "blue");

    myvol2 = gen.test.volume(c(40, 40, 40), bg = 0L);
    volvis.voxels(myvol2, render_every = 10, voxelcol = "from_intensity");

    testthat::expect_equal(dim(myvol), c(40, 40, 40)); # add a check to prevent skip
})


test_that("The voxel hull can be computed from a volume", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    myvol = gen.test.volume(c(25, 25, 25), bg = NA);
    vh = vol.hull(myvol);
    testthat::expect_equal(dim(myvol), dim(vh));
})


test_that("The voxel contour can be visualized for a volume", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if_rgl_required();
    skip_if_rgl_window_required();
    testthat::skip_if_not(box.has.x11display(), "This test requires an X11 display.");
    myvol = gen.test.volume(c(25, 25, 25), bg = 1L);
    volvis.contour(myvol);
    testthat::expect_equal(dim(myvol), c(25,25,25)); # add a check to prevent skip

    #testthat::expect_error(volvis.contour(myvol, color = c("white", "green"))); # color must be a scalar
})


test_that("The voxel contour can be visualized for a volume", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if_rgl_required();
    skip_if_rgl_window_required();
    centers = matrix(rnorm(500*3)*100, ncol=3);
    rglvoxels(centers, voxelcol="red");
    rglvoxels(centers);
    testthat::expect_equal(1L, 1L); # add a check to prevent skip
})

test_that("A brain volume segmentation can be rendered with correct colors from the aseg", {
    testthat::skip_on_travis(); # Reduce test time on travis to prevent the build from being killed.
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if_rgl_required();
    skip_if_rgl_window_required();
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11, an aseg.mgz file for the demo subject and the FreeSurferColorLUT.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    #aseg = subject.volume(subjects_dir, subject_id, 'aseg');    # Not shipped with the package atm.
    aseg = subject.volume(subjects_dir, subject_id, 'aparc+aseg');    # Not shipped with the package atm.

    aseg_codes = unique(as.vector(aseg));

    fs_home = find.freesurferhome()$found_at;
    ct = freesurferformats::read.fs.colortable(file.path(fs_home, 'FreeSurferColorLUT.txt'));

    rgl::open3d();
    all_regions_coloredvoxels = list();
    for(aseg_code in aseg_codes) {
        if(aseg_code == 0) { # skip background ('unknown').
            next;
        }
        ct_entry = subset(ct, ct$struct_index == aseg_code);
        ct_color_rgb = grDevices::rgb(ct_entry$r / 255., ct_entry$g / 255., ct_entry$b / 255.);
        cv = volvis.voxels(vol.mask.from.segmentation(aseg, aseg_code), render_every=1, voxelcol=ct_color_rgb);
        all_regions_coloredvoxels = c(all_regions_coloredvoxels, cv);
    }
    # Check it out, it looks pretty cool.

    render_animation = FALSE;
    if(render_animation) {
        rgloptions=list("windowRect"=c(80,80,800,800));
        rglactions = list("movie"="vox_aseg_rot");
        vislayout.from.coloredmeshes(all_regions_coloredvoxels, view_angles="sr", rgloptions = rgloptions, rglactions = rglactions);
    }

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
})


test_that("Brain structures can be rendered as contours using misc3d", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if_rgl_required();
    skip_if_rgl_window_required();
    testthat::skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    testthat::skip_if_not(box.can.run.all.tests(), "This test requires X11, the misc3d package, and an aseg.mgz file for the demo subject.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    testthat::skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";
    aseg = subject.volume(subjects_dir, subject_id, 'aseg');    # Not shipped with the package atm.
    ventricle_aseg_codes = c(4, 14, 15, 43);    # see FreeSurferColorLUT.txt
    ventricle_mask = vol.mask.from.segmentation(aseg, ventricle_aseg_codes);

    # Remove the NAs, contour3d does not seem to like them.
    ventricle_mask_mod = ventricle_mask;
    ventricle_mask_mod[which(is.na(ventricle_mask), arr.ind=T)] = 0;

    custom_colors = grDevices::terrain.colors(length(ventricle_aseg_codes));
    misc3d::contour3d(ventricle_mask_mod, level=ventricle_aseg_codes, color=custom_colors, alpha = seq(0.2, 0.5, length.out = length(ventricle_aseg_codes)));



    ## ---- Draw the surface of the left hemi, and the ventricle contours into the same plot ----
    vis.subject.morph.native(subjects_dir, 'subject1', 'thickness', 'lh', views = 'si', style='semitransparent');
    vent_tris = misc3d::contour3d(ventricle_mask_mod, level=3, color="red", draw=FALSE);
    # # Fix the rendering coords to surface RAS (the contour is in 1-based R array index space).
    vent_tris = apply.transform(vent_tris, index2ras_tkr());
    misc3d::drawScene.rgl(vent_tris, add = TRUE);

    ## Add transparent overlay of whole brain for worse performance ><
    have_mighty_computer = TRUE;
    if(have_mighty_computer) {
        brain_tris = misc3d::contour3d(aseg, level=1, color="gray", alpha=0.1, back='culled', draw = FALSE);
        brain_tris = apply.transform(brain_tris, index2ras_tkr());
        misc3d::drawScene.rgl(brain_tris, add = TRUE);
    }

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.

})


test_that("The pial surface drawn as a transparent wrapping over the white surface", {
    testthat::skip_on_travis(); # Reduce test time on travis to prevent the build from being killed.
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if_rgl_required();
    skip_if_rgl_window_required();
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.can.run.all.tests(), "This test requires X11 and extra data.");

    fsbrain::download_optional_data();
    subjects_dir = testdatapath.subjectsdir.full.subject1();
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";

    cm_white = vis.subject.morph.native(subjects_dir, 'subject1', 'thickness', hemi = 'both', surface = 'white', views = NULL);
    cm_pial = vis.subject.morph.native(subjects_dir, 'subject1', 'thickness', hemi = 'both', surface = 'pial', views = NULL);
    cm_pial[[1]]$style = 'semitransparent';
    cm_pial[[2]]$style = 'semitransparent';
    vis.coloredmeshes(c(cm_white, cm_pial), skip_all_na = FALSE, style = 'from_mesh');

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
})


test_that("Voxels can be rotated and rendered in a brainview", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_rgl_window_required();
    skip_if_not(box.can.run.all.tests(), "This test requires X11.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";

    vol = subject.volume(subjects_dir, subject_id, "brain");
    vol[vol < 90] = NA;
    volvox = volvis.voxels(vol);
    brainviews("t9", volvox);

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
})


test_that("A misc3d contour (Triangles3D instance) can be rotated and rendered in a brainview", {
    testthat::skip_on_cran(); # CRAN maintainers asked me to reduce test time on CRAN by disabling unit tests.
    skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_rgl_window_required();
    skip_if_not(box.can.run.all.tests(), "This test requires X11.");
    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

    subject_id = "subject1";

    vol = subject.volume(subjects_dir, subject_id, "brain");
    surface_tris = fsbrain::volvis.contour(vol);
    brainviews("t9", surface_tris);

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
})


test_that("A misc3d Triangles3D iso-surface is converted to a coloredmesh without changing its geometry", {
    skip_if_not_installed("misc3d");

    # A sphere-like volume. Its iso-surface area is known: the level 5 isosurface of '15 - distance
    # from the center' is a sphere of radius 10 centered in the volume.
    sdim = 30L;
    grid = expand.grid(i = seq_len(sdim), j = seq_len(sdim), k = seq_len(sdim));
    radius = sqrt((grid$i - 15)^2 + (grid$j - 15)^2 + (grid$k - 15)^2);
    vol = array(15 - radius, dim = c(sdim, sdim, sdim));
    tris = misc3d::contour3d(vol, level = 5, draw = FALSE);

    tri.area = function(v1, v2, v3) {
        face_normals = cbind(
            (v2[, 2] - v1[, 2]) * (v3[, 3] - v1[, 3]) - (v2[, 3] - v1[, 3]) * (v3[, 2] - v1[, 2]),
            (v2[, 3] - v1[, 3]) * (v3[, 1] - v1[, 1]) - (v2[, 1] - v1[, 1]) * (v3[, 3] - v1[, 3]),
            (v2[, 1] - v1[, 1]) * (v3[, 2] - v1[, 2]) - (v2[, 2] - v1[, 2]) * (v3[, 1] - v1[, 1]));
        return(sqrt(rowSums(face_normals^2)) / 2.0);
    };

    source_area = sum(tri.area(tris$v1, tris$v2, tris$v3));
    expect_equal(source_area, 4.0 * pi * 10^2, tolerance = 0.02);   # sanity check of the test data

    # The fields 'v1', 'v2' and 'v3' of the Triangles3D are Nx3 matrices, one row per triangle.
    num_tris = length(tris$v1) / 3L;
    expect_equal(dim(tris$v1), c(num_tris, 3L));

    cm = Triangles3D.to.coloredmesh(tris);
    expect_true(is.fs.coloredmesh(cm));
    expect_null(cm$hemi);   # iso-surfaces are not hemisphere-specific, see the function docs.

    mesh = cm$mesh;
    faces = mesh$it;
    verts = t(mesh$vb[1:3, ]);
    expect_equal(ncol(faces), num_tris);   # one mesh face per source triangle

    # Face k of the mesh consists of the three vertices of triangle k, in the same order. If the
    # vertex blocks were combined wrongly, most faces would be degenerate and the mesh would not
    # represent the iso-surface at all.
    expect_equal(verts[faces[1, ], ], tris$v1);
    expect_equal(verts[faces[2, ], ], tris$v2);
    expect_equal(verts[faces[3, ], ], tris$v3);
    expect_equal(sum(tri.area(verts[faces[1, ], ], verts[faces[2, ], ], verts[faces[3, ], ])), source_area);

    # Extra arguments: a hemisphere can be set, and a list of Triangles3D instances is converted
    # element-wise.
    expect_equal(Triangles3D.to.coloredmesh(tris, hemi = "lh")$hemi, "lh");
    cm_list = Triangles3D.to.coloredmesh(list(tris, tris), hemi = "rh");
    expect_length(cm_list, 2L);
    expect_true(is.fs.coloredmesh(cm_list[[1]]));
    expect_equal(cm_list[[2]]$hemi, "rh");

    # error handling
    expect_error(Triangles3D.to.coloredmesh("notatriangles3d"));   # not a Triangles3D
    expect_error(Triangles3D.to.coloredmesh(list("notatriangles3d")));   # list of something else
    brokentris = tris;   # a Triangles3D whose vertex fields have the transposed layout
    class(brokentris) = "Triangles3D";
    brokentris$v1 = t(tris$v1);
    brokentris$v2 = t(tris$v2);
    brokentris$v3 = t(tris$v3);
    expect_error(Triangles3D.to.coloredmesh(brokentris), "3 vertex coordinates");
})


test_that("apply.transform restores the orientation of meshes for orientation-flipping matrices", {
    # A sphere-like volume, from which we extract an iso-surface. Contouring in voxel space and then
    # transforming to surface RAS with the FreeSurfer 'vox2ras_tkr' matrix is what the volume
    # overlay functions do.
    sdim = 30L;
    grid = expand.grid(i = seq_len(sdim), j = seq_len(sdim), k = seq_len(sdim));
    radius = sqrt((grid$i - 15)^2 + (grid$j - 15)^2 + (grid$k - 15)^2);
    vol = array(15 - radius, dim = c(sdim, sdim, sdim));
    tris = misc3d::contour3d(vol, level = 5, draw = FALSE);

    # Mean sign of the geometric normal (computed from the winding, i.e., from the vertex order of
    # the faces) relative to the direction away from the mesh center: positive means outward-facing.
    mean.orientation = function(v1, v2, v3) {
        face_normals = cbind(
            (v2[, 2] - v1[, 2]) * (v3[, 3] - v1[, 3]) - (v2[, 3] - v1[, 3]) * (v3[, 2] - v1[, 2]),
            (v2[, 3] - v1[, 3]) * (v3[, 1] - v1[, 1]) - (v2[, 1] - v1[, 1]) * (v3[, 3] - v1[, 3]),
            (v2[, 1] - v1[, 1]) * (v3[, 2] - v1[, 2]) - (v2[, 2] - v1[, 2]) * (v3[, 1] - v1[, 1]));
        centers = (v1 + v2 + v3) / 3.0;
        mesh_center = matrix(colMeans(rbind(v1, v2, v3)), nrow(centers), 3L, byrow = TRUE);
        return(mean(rowSums(face_normals * (centers - mesh_center))));
    };

    # The same for a mesh3d/tmesh3d instance, using its triangles ('it') or quads ('ib').
    mean.mesh.orientation = function(mesh) {
        verts = t(mesh$vb[1:3, ]);
        faces = if(! is.null(mesh$it)) mesh$it else mesh$ib;
        return(mean.orientation(verts[faces[1, ], , drop = FALSE], verts[faces[2, ], , drop = FALSE], verts[faces[3, ], , drop = FALSE]));
    };

    transform.coords = function(coords, mat) { return((cbind(coords, 1) %*% t(mat))[, 1:3]); };

    vox2ras = vox2ras_tkr();
    expect_true(det(vox2ras[1:3, 1:3]) < 0);   # the transform does flip the orientation

    # The raw iso-surface (in voxel space) is wound so that its normals point outwards, and it has to
    # stay that way after the transformation to surface RAS. Without the orientation restoration in
    # apply.transform, the mirroring matrix would flip all normals, so the iso-surface would be
    # rendered inside-out.
    expect_gt(mean.orientation(tris$v1, tris$v2, tris$v3), 0);
    tris_ras = apply.transform(tris, vox2ras);
    expect_gt(mean.orientation(tris_ras$v1, tris_ras$v2, tris_ras$v3), 0);
    # This is achieved by swapping the 2nd and 3rd vertex of every triangle.
    expect_equal(tris_ras$v2, transform.coords(tris$v3, vox2ras));
    expect_equal(tris_ras$v3, transform.coords(tris$v2, vox2ras));

    # A mesh3d: the winding of its faces is restored as well. The stored normals are transformed with
    # the linear part of the matrix, they must stay consistent with the winding (negating them would
    # make the surface render black with lighting enabled).
    tmesh = rgl::mesh3d(cbind(t(tris$v1[1, , drop = FALSE]), 1));
    tmesh$it = matrix(c(1L, 2L, 3L), nrow = 3L);
    tmesh$normals = matrix(c(0, 0, 1, 0), ncol = 1L);
    tmesh_ras = apply.transform(tmesh, vox2ras);
    expect_equal(tmesh_ras$it[2, ], tmesh$it[3, ]);
    expect_equal(tmesh_ras$it[3, ], tmesh$it[2, ]);
    expect_equal(tmesh_ras$normals[1:3, 1], as.vector(vox2ras[1:3, 1:3] %*% c(0, 0, 1)));

    # A quad mesh (rgl::cube3d) is outward-wound, and stays that way after the mirroring transform.
    cube3d_mesh = rgl::cube3d();
    expect_gt(mean.mesh.orientation(cube3d_mesh), 0);
    cube3d_ras = apply.transform(cube3d_mesh, vox2ras);
    expect_gt(mean.mesh.orientation(cube3d_ras), 0);
    expect_equal(cube3d_ras$ib, cube3d_mesh$ib[c(1L, 3L, 2L, 4L), ]);

    # An orientation-preserving transform (rotation + translation, e.g. a registration matrix) must
    # not touch the face order at all.
    rotation_translation = matrix(c(0, -1, 0, 10, 1, 0, 0, 20, 0, 0, 1, 30, 0, 0, 0, 1), nrow = 4L, byrow = TRUE);
    expect_true(det(rotation_translation[1:3, 1:3]) > 0);
    tris_rot = apply.transform(tris, rotation_translation);
    expect_equal(tris_rot$v2, transform.coords(tris$v2, rotation_translation));
    expect_equal(tris_rot$v3, transform.coords(tris$v3, rotation_translation));
    expect_equal(apply.transform(cube3d_mesh, rotation_translation)$ib, cube3d_mesh$ib);

    # fs.surface instances: the vertex order within the faces is reversed for flipping matrices.
    cube = freesurferformats::read.fs.surface(system.file("extdata", "cube.ply", package = "fsbrain", mustWork = TRUE));
    cube_flipped = apply.transform(cube, vox2ras);
    expect_equal(cube_flipped$faces, cube$faces[, c(1L, 3L, 2L)]);
    expect_equal(cube_flipped$vertices, apply.transform(cube$vertices, vox2ras));
    expect_equal(apply.transform(cube, rotation_translation)$faces, cube$faces);
})


test_that("apply.transform supports surface meshes, renderables and lists of them", {
    cube = freesurferformats::read.fs.surface(system.file("extdata", "cube.ply", package = "fsbrain", mustWork = TRUE));
    num_verts = nrow(cube$vertices);

    # A rotation around z (90 degrees) combined with a translation.
    transform = matrix(c(0, -1, 0, 10,
                         1,  0, 0, 20,
                         0,  0, 1, 30,
                         0,  0, 0, 1), nrow = 4L, byrow = TRUE);
    expected_vertices = cbind(-cube$vertices[, 2] + 10, cube$vertices[, 1] + 20, cube$vertices[, 3] + 30);

    # NULL matrix: the input is returned as-is.
    expect_identical(apply.transform(cube, NULL), cube);

    # fs.surface: vertices are transformed, faces are not touched, class is kept.
    cube_moved = apply.transform(cube, transform);
    expect_true(freesurferformats::is.fs.surface(cube_moved));
    expect_equal(cube_moved$vertices, expected_vertices);
    expect_equal(cube_moved$faces, cube$faces);

    # Coordinate matrix (both representations).
    expect_equal(apply.transform(cube$vertices, transform), expected_vertices);
    expect_equal(apply.transform(cbind(cube$vertices, 1), transform), expected_vertices);
    expect_equal(apply.transform(cube$vertices[1, ], transform), expected_vertices[1, ]);
    expect_error(apply.transform(matrix(rep(1, 10), ncol = 5), transform), "must have 3");

    # rgl tmesh3d.
    tmesh_moved = apply.transform(fs.surface.to.tmesh3d(cube), transform);
    expect_equal(t(tmesh_moved$vb[1:3, ]), expected_vertices);

    # fs.coloredmesh: the mesh and the source mesh in the metadata are transformed, colors stay.
    cm = coloredmesh.from.preloaded.data(cube, morph_data = seq.int(num_verts), hemi = "lh");
    cm_moved = apply.transform(cm, transform);
    expect_true(is.fs.coloredmesh(cm_moved));
    expect_equal(cm_moved$col, cm$col);
    expect_equal(cm_moved$metadata$fs_mesh$vertices, expected_vertices);

    # A hemilist of coloredmeshes is transformed element-wise, names are kept.
    cms_moved = apply.transform(list("lh" = cm, "rh" = cm), transform);
    expect_equal(names(cms_moved), c("lh", "rh"));
    expect_equal(cms_moved$rh$metadata$fs_mesh$vertices, expected_vertices);

    # misc3d Triangles3D (as produced by volvis.contour or misc3d::contour3d).
    tris = list("v1" = cube$vertices[cube$faces[, 1], ], "v2" = cube$vertices[cube$faces[, 2], ], "v3" = cube$vertices[cube$faces[, 3], ]);
    class(tris) = c(class(tris), "Triangles3D");
    tris_moved = apply.transform(tris, transform);
    expect_equal(tris_moved$v1, expected_vertices[cube$faces[, 1], ]);

    # Unsupported input is reported.
    expect_error(apply.transform("not a mesh", transform), "not supported");
})
