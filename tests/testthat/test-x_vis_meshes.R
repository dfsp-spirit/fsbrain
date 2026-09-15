

test_that("A coloredmesh can be rendered using vis.coloredmeshes", {
    skip_if_rgl_window_required();

    cm_hemilist = get.demo.coloredmeshes.hemilist();

    vis.coloredmeshes(cm_hemilist, draw_colorbar = TRUE);
    vis.coloredmeshes(cm_hemilist, draw_colorbar = "horizontal");
    vis.coloredmeshes(cm_hemilist, draw_colorbar = "vertical");

    # error handling
    expect_error(vis.coloredmeshes("notameshlist")); # first parameter must be list
    expect_warning(vis.coloredmeshes(list())); # first parameter must not be empty list
    expect_error(vis.coloredmeshes(list("notamesh"))); # list in first parameter must contain renderables
    expect_error(vis.coloredmeshes(cm, draw_colorbar = "dunno")); # invalid colorbar setting

    close.all.rgl.windows();
})


test_that("A coloredmesh without a hemisphere is assigned to both hemisphere views", {
    skip_if_rgl_required();

    # Meshes which are not hemisphere-specific, e.g., volume iso-surfaces, have hemi=NULL.
    cm_nohemi = fs.coloredmesh(rgl::cube3d(), "#FF0000", hemi = NULL);
    expect_true("hemi" %in% names(cm_nohemi));
    expect_null(cm_nohemi$hemi);

    sorted = fsbrain:::sortcoloredmeshes.by.hemi(list(cm_nohemi));
    expect_length(sorted$lh, 1L);
    expect_length(sorted$rh, 1L);
    expect_silent(fsbrain:::sortcoloredmeshes.by.hemi(list(cm_nohemi)));

    # A mesh for one hemisphere is assigned to that hemisphere only.
    sorted_lh = fsbrain:::sortcoloredmeshes.by.hemi(list(fs.coloredmesh(rgl::cube3d(), "#00FF00", hemi = "lh")));
    expect_length(sorted_lh$lh, 1L);
    expect_length(sorted_lh$rh, 0L);

    # A coloredmesh without a hemi field at all is assigned to both, with a warning.
    cm_nofield = cm_nohemi;
    cm_nofield$hemi = NULL; # removes the field, the class is kept
    expect_false("hemi" %in% names(cm_nofield));
    expect_warning(sorted_nofield <- fsbrain:::sortcoloredmeshes.by.hemi(list(cm_nofield)), "no hemi value");
    expect_length(sorted_nofield$lh, 1L);
    expect_length(sorted_nofield$rh, 1L);
})


test_that("A coloredmesh can be rendered using vis.coloredmeshes.rotating", {
    skip_if_rgl_window_required();
    cm_hemilist = get.demo.coloredmeshes.hemilist();

    vis.coloredmeshes.rotating(cm_hemilist, duration = 2L);

    # error handling
    testthat::expect_error(vis.coloredmeshes.rotating("notameshlist")); # first parameter must be list

    close.all.rgl.windows();
})
