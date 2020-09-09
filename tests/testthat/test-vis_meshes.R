

test_that("A coloredmesh can be rendered using vis.coloredmeshes", {

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


test_that("A coloredmesh can be rendered using vis.coloredmeshes.rotating", {
    cm_hemilist = get.demo.coloredmeshes.hemilist();

    vis.coloredmeshes.rotating(cm_hemilist, duration = 2L);

    # error handling
    testthat::expect_error(vis.coloredmeshes.rotating("notameshlist")); # first parameter must be list

    close.all.rgl.windows();
})
