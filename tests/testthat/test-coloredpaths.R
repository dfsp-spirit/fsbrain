# Tests for the 'fs.coloredpaths' renderable class (R/coloredpaths.R), i.e. the
# line segments used for connectome edges, and for its integration into the rgl
# and scimesh renderer backends.

# Helper: get a small fs.coloredpaths instance for testing.
get.demo.coloredpaths <- function(num_segments = 3L, col = "#FF0000", width = 1.0) {
    from = matrix(c(0, 0, 0, 10, 0, 0, 0, 10, 0), ncol = 3L, byrow = TRUE)[seq_len(num_segments), , drop = FALSE];
    to = matrix(c(10, 0, 0, 0, 10, 0, 0, 0, 10), ncol = 3L, byrow = TRUE)[seq_len(num_segments), , drop = FALSE];
    return(fs.coloredpaths(from, to, col = col, width = width));
}


test_that("fs.coloredpaths creates an instance with the expected fields", {
    p = get.demo.coloredpaths(2L, col = c("#FF0000", "#00FF00"), width = c(1.0, 3.0));

    expect_true(is.fs.coloredpaths(p));
    expect_true(is.list(p));
    expect_equal(nrow(p$from), 2L);
    expect_equal(nrow(p$to), 2L);
    expect_equal(p$col, c("#FF0000", "#00FF00"));
    expect_equal(p$width, c(1.0, 3.0));
    expect_true(p$depth_test);
    expect_false(p$lit);
    expect_null(p$hemi);
    expect_true(p$render);
    expect_equal(coloredpaths.length(p), 2L);
});


test_that("fs.coloredpaths recycles a single color and width over all segments", {
    p = get.demo.coloredpaths(3L, col = "#FFFFFF", width = 2.0);
    expect_equal(length(p$col), 3L);
    expect_equal(p$col, rep("#FFFFFF", 3L));
    expect_equal(p$width, rep(2.0, 3L));

    # More colors than segments: the extra ones are dropped (like for other fsbrain functions).
    p2 = get.demo.coloredpaths(1L, col = c("#FF0000", "#00FF00"));
    expect_equal(p2$col, "#FF0000");
});


test_that("fs.coloredpaths accepts a single point as a length-3 vector", {
    p = fs.coloredpaths(c(0, 0, 0), c(1, 1, 1));
    expect_equal(nrow(p$from), 1L);
    expect_equal(as.numeric(p$from), c(0, 0, 0));
    expect_equal(as.numeric(p$to), c(1, 1, 1));
});


test_that("fs.coloredpaths validates its input", {
    from = matrix(c(0, 0, 0), ncol = 3L);
    to = matrix(c(1, 1, 1), ncol = 3L);

    expect_error(fs.coloredpaths(from, to, col = 1L));                       # not a color
    expect_error(fs.coloredpaths(from, to, width = -1.0));
    expect_error(fs.coloredpaths(from, to, width = 0.0));
    expect_error(fs.coloredpaths(from, to, width = c(1.0, 2.0)));            # wrong length
    expect_error(fs.coloredpaths(from, to, width = NA_real_));
    expect_error(fs.coloredpaths(from, to, depth_test = "yes"));
    expect_error(fs.coloredpaths(from, to, lit = "yes"));
    expect_error(fs.coloredpaths(from, to, hemi = "up"));
    expect_error(fs.coloredpaths(from, to, hemi = c("lh", "rh")));

    # Mismatching number of segments.
    expect_error(fs.coloredpaths(rbind(from, from), to));

    # Invalid coordinate input.
    expect_error(fs.coloredpaths(NULL, to));
    expect_error(fs.coloredpaths(cbind(1, 2), to));
    expect_error(fs.coloredpaths(data.frame(x = 1, y = 2, z = 3), to));
    expect_error(fs.coloredpaths("nope", to));
});


test_that("the print method of fs.coloredpaths reports the segment count", {
    p = get.demo.coloredpaths(2L, col = c("#FF0000", "#00FF00"), width = 2.0);
    out = utils::capture.output(print(p));
    expect_true(any(grepl("2 segment", out)));
    expect_true(any(grepl("width", out)));
    expect_true(any(grepl("depth test", out)));

    # A coloredpaths instance with metadata also mentions the metadata keys.
    p2 = get.demo.coloredpaths(1L);
    p2$metadata = list("src_data" = 1.0);
    out2 = utils::capture.output(print(p2));
    expect_true(any(grepl("metadata keys: src_data", out2)));
});


test_that("fsbrain.renderable and is.fs.coloredpaths identify the class", {
    p = get.demo.coloredpaths(1L);
    expect_true(fsbrain.renderable(p));
    expect_true(is.fs.coloredpaths(p));
    expect_false(is.fs.coloredpaths(list()));
    expect_false(fsbrain.renderable("nope"));
});


test_that("transform_renderable transforms the segment endpoints of fs.coloredpaths", {
    p = get.demo.coloredpaths(1L);
    m = diag(4);
    m[1:3, 4] = c(1, 2, 3);
    tp = transform_renderable(p, m);
    expect_true(is.fs.coloredpaths(tp));
    expect_equal(as.numeric(tp$from), as.numeric(p$from) + c(1, 2, 3));
    expect_equal(as.numeric(tp$to), as.numeric(p$to) + c(1, 2, 3));

    # apply.transform() dispatches to the same code.
    tp2 = apply.transform(p, m);
    expect_equal(as.numeric(tp2$to), as.numeric(tp$to));
});


test_that("apply.transform works on a list which contains an fs.coloredpaths instance", {
    p = get.demo.coloredpaths(1L);
    cm = get.demo.coloredmesh();
    m = diag(4);
    m[1, 4] = 5;
    res = apply.transform(list("paths" = p, "mesh" = cm), m);
    expect_true(is.fs.coloredpaths(res$paths));
    expect_equal(res$paths$from[1, 1], p$from[1, 1] + 5);
});


test_that("vis.renderable draws an fs.coloredpaths instance with rgl", {
    skip_if_rgl_required();
    p = get.demo.coloredpaths(2L, col = c("#FF0000", "#00FF00"), width = c(1.0, 3.0));
    rgl::open3d();
    expect_silent(vis.renderable(p));
    # The two segments have different widths, so they are drawn in two calls.
    expect_equal(length(rgl::ids3d()$id), 2L);
    close.all.rgl.windows();
});


test_that("vis.coloredpaths validates its input and skips empty renderables", {
    expect_error(vis.coloredpaths(list()));
    skip_if_rgl_required();
    p = get.demo.coloredpaths(1L);
    p$render = FALSE;
    rgl::open3d();
    expect_silent(vis.renderable(p));       # skipped, but no error
    expect_equal(length(rgl::ids3d()$id), 0L);
    close.all.rgl.windows();
});


test_that("vis.coloredpaths applies the line width as a material property", {
    skip_if_rgl_required();
    p = get.demo.coloredpaths(1L, width = 4.0);
    rgl::open3d();
    vis.coloredpaths(p);
    ids = rgl::ids3d();
    expect_equal(length(ids$id), 1L);
    expect_equal(as.character(ids$type[1L]), "lines");
    close.all.rgl.windows();
});


test_that("coloredpaths_to_scimesh converts segments to scimesh line layers", {
    skip_if_not_installed("scimesh");
    p = get.demo.coloredpaths(3L, col = c("#FF0000", "#00FF00", "#0000FF"), width = 1.0);
    layers = coloredpaths_to_scimesh(p);
    expect_equal(length(layers), 1L);
    expect_s3_class(layers[[1L]], "scimesh_lines");
    expect_equal(nrow(layers[[1L]]$from), 3L);
    expect_equal(layers[[1L]]$width, 1.0);
    expect_true(layers[[1L]]$depth_test);
    expect_false(layers[[1L]]$lit);

    # One layer per distinct width, and the layer colors follow the segment colors.
    p2 = get.demo.coloredpaths(3L, col = c("#FF0000", "#00FF00", "#0000FF"), width = c(1.0, 2.0, 1.0));
    layers2 = coloredpaths_to_scimesh(p2);
    expect_equal(length(layers2), 2L);
    expect_equal(sort(unique(vapply(layers2, function(l) { l$width; }, numeric(1L)))), c(1.0, 2.0));
    widths = vapply(layers2, function(l) { l$width; }, numeric(1L));
    red_layer = layers2[[which(widths == 1.0)]];
    expect_equal(nrow(red_layer$colors), 2L);
    expect_equal(red_layer$colors[1L, 1L], 1.0);   # red
    expect_equal(red_layer$colors[1L, 2L], 0.0);
});


test_that("coloredpaths_to_scimesh respects the render flag and empty input", {
    skip_if_not_installed("scimesh");
    p = get.demo.coloredpaths(1L);
    p$render = FALSE;
    expect_equal(length(coloredpaths_to_scimesh(p)), 0L);
    expect_error(coloredpaths_to_scimesh(list()));
});


test_that("coloredpaths_to_scimesh honors the alpha value of the rendering style", {
    skip_if_not_installed("scimesh");
    p = get.demo.coloredpaths(1L, col = "#FF0000");
    layers = coloredpaths_to_scimesh(p, style = list("alpha" = 0.25));
    expect_equal(layers[[1L]]$colors[1L, 4L], 0.25);
});


test_that("renderables_to_line_layers walks flat lists, hemilists and single renderables", {
    skip_if_not_installed("scimesh");
    p = get.demo.coloredpaths(2L);
    cm = get.demo.coloredmesh();

    expect_equal(length(renderables_to_line_layers(p)), 1L);
    expect_equal(length(renderables_to_line_layers(list(p))), 1L);
    expect_equal(length(renderables_to_line_layers(list(cm, p, cm))), 1L);
    expect_equal(length(renderables_to_line_layers(list("lh" = cm, "rh" = p))), 1L);
    expect_equal(length(renderables_to_line_layers(list(cm, cm))), 0L);
    expect_equal(length(renderables_to_line_layers(list())), 0L);
});


test_that("the scimesh backend renders line renderables and skips them in the mesh scene", {
    skip_if_not_installed("scimesh");
    p = get.demo.coloredpaths(2L, col = c("#FF0000", "#0000FF"), width = 2.0);
    cm = get.demo.coloredmesh();

    # Lines are not meshes: the mesh conversion ignores them, the line conversion picks them up.
    expect_equal(length(coloredmeshes_to_scimesh(list(p))), 0L);
    expect_equal(length(renderables_to_line_layers(list(p))), 1L);
    expect_equal(length(coloredmeshes_to_scimesh(list(cm))), 1L);
    expect_equal(length(renderables_to_line_layers(list(cm))), 0L);
});


test_that("export renders a scene which contains an fs.coloredpaths renderable", {
    skip_if_not_installed("scimesh");
    p = get.demo.coloredpaths(2L, col = c("#FF0000", "#00FF00"), width = 1.0);
    nodes = coloredmesh.from.spheres(rbind(c(0, 0, 0), c(10, 0, 0)), c(1, 1), c("#FFFF00", "#00FFFF"));
    out_img = tempfile(fileext = ".png");

    withr::local_options(list(fsbrain.renderer_backend = "scimesh"));
    withr::local_dir(tempdir());
    expect_silent(export(list(p, nodes), draw_colorbar = FALSE, output_img = out_img, silent = TRUE));
    expect_true(file.exists(out_img));
    expect_true(file.size(out_img) > 1000L);
});


test_that("export draws a colorbar for the edges of a connectome-like scene", {
    skip_if_not_installed("scimesh");
    p = get.demo.coloredpaths(3L, col = c("#FF0000", "#00FF00", "#0000FF"));
    p$metadata = list("src_data" = c(0.1, 0.5, 0.9),
        "makecmap_options" = list("colFn" = cm.seq(), "n" = 100L, "range" = c(0.0, 1.0)),
        "data_range" = c(0.1, 0.9));
    nodes = coloredmesh.from.spheres(rbind(c(0, 0, 0), c(10, 0, 0)), c(1, 1), c("#FFFF00", "#00FFFF"));

    expect_true(can.plot.colorbar.from.coloredmeshes(list(p, nodes)));
    # The colorbar is drawn for the first renderable which carries the metadata, which is the edges.
    expect_equal(coloredmeshes.get.md(list(p, nodes), 'makecmap_options')$range, c(0.0, 1.0));

    out_img = tempfile(fileext = ".png");
    withr::local_options(list(fsbrain.renderer_backend = "scimesh"));
    withr::local_dir(tempdir());
    expect_silent(export(list(p, nodes), draw_colorbar = "horizontal", output_img = out_img, silent = TRUE));
    expect_true(file.exists(out_img));
});


test_that("the rgl backend renders a scene which contains an fs.coloredpaths renderable", {
    skip_if_rgl_required();
    p = get.demo.coloredpaths(2L, col = c("#FF0000", "#00FF00"), width = 1.0);
    nodes = coloredmesh.from.spheres(rbind(c(0, 0, 0), c(10, 0, 0)), c(1, 1), c("#FFFF00", "#00FFFF"));

    rgl::open3d();
    expect_silent(vis.coloredmeshes(list(p, nodes), rgloptions = list("windowRect" = c(0, 0, 200, 200))));
    expect_equal(length(rgl::ids3d()$id), 2L);
    close.all.rgl.windows();
});


test_that("rglactions highlight_points still work in a scene with line renderables", {
    skip_if_rgl_required();
    p = get.demo.coloredpaths(1L);
    rgl::open3d();
    expect_silent(vis.coloredmeshes(list(p), rglactions = list("highlight_points" = list("coords" = matrix(c(1, 1, 1), ncol = 3L), "color" = "#FF00FF")),
        rgloptions = list("windowRect" = c(0, 0, 200, 200))));
    close.all.rgl.windows();
});
