# Tests for the white matter tract visualization functions (R/tracts.R and
# R/tracts_data.R): read.tract.bundles() and vis.tracts().
#
# Most tests are offline and do not require any downloaded data: the tract files
# are written into a temporary directory with the writers of the 'freesurferformats'
# package, and the context surface comes from a tiny synthetic subject (a cube
# surface, exactly like in the connectome tests).


# Helper: a few synthetic streamlines, as a list of (n, 3) matrices.
make.demo.streamlines <- function(num_tracts = 3L, num_points = 5L, seed = 42L, offset = c(0.0, 0.0, 0.0)) {
    set.seed(seed);
    lapply(seq_len(num_tracts), function(tract_idx) {
        base = cbind(seq(0.0, 20.0, length.out = num_points),
                     seq(0.0, 10.0, length.out = num_points),
                     seq(0.0, 5.0, length.out = num_points));
        base = sweep(base, 2L, offset, "+");
        return(base + matrix(stats::rnorm(num_points * 3L, sd = 0.01), ncol = 3L));
    });
}


# Helper: the TRK header used for the demo bundle 'bundleB'. It is a realistic
# one: the identity matrix for 'bundleA' (in which case no coordinate correction
# is needed at all) and the header of the XTRACT atlas files (an LPS voxel grid
# with a non-identity voxel-to-RAS matrix) for 'bundleB'.
make.demo.trk.header <- function(identity_affine = TRUE) {
    if(identity_affine) {
        return(list("dim" = c(100L, 100L, 100L), "voxel_size" = c(1.0, 1.0, 1.0),
                    "origin" = c(0.0, 0.0, 0.0), "vox2ras" = diag(4L), "voxel_order" = "RAS"));
    }
    xtract_affine = matrix(c(-1.0, 0.0, 0.0, 78.0,
                              0.0, -1.0, 0.0, 76.0,
                              0.0, 0.0, 1.0, -50.0,
                              0.0, 0.0, 0.0, 1.0), nrow = 4L, byrow = TRUE);
    return(list("dim" = c(157L, 189L, 136L), "voxel_size" = c(1.0, 1.0, 1.0),
                "origin" = c(0.0, 0.0, 0.0), "vox2ras" = xtract_affine, "voxel_order" = "LPS"));
}


# Helper: write a temporary directory with two TRK bundles and one TCK bundle.
# Returns the directory path, plus the streamlines that were written per bundle.
# All coordinates are given in RAS+ mm space, the TRK files are written with the
# corresponding header entry, so that reading them with coords = 'ras' returns
# the coordinates that were written.
make.demo.tract.dir <- function() {
    dir = tempfile("fsbrain_tracts_");
    dir.create(dir, recursive = TRUE);

    streamlines_A = make.demo.streamlines(3L, 5L, seed = 1L);
    streamlines_B = make.demo.streamlines(2L, 7L, seed = 2L, offset = c(10.0, 0.0, 0.0));
    streamlines_C = make.demo.streamlines(4L, 6L, seed = 3L, offset = c(0.0, 10.0, 0.0));

    freesurferformats::write.dti.trk(streamlines_A, file.path(dir, "bundleA.trk"),
        header = make.demo.trk.header(identity_affine = TRUE), coords_space = "ras");
    freesurferformats::write.dti.trk(streamlines_B, file.path(dir, "bundleB.trk"),
        header = make.demo.trk.header(identity_affine = FALSE), coords_space = "ras");
    freesurferformats::write.dti.tck(streamlines_C, file.path(dir, "bundleC.tck"));

    return(list("dir" = dir,
                "streamlines" = list("bundleA" = streamlines_A, "bundleB" = streamlines_B, "bundleC" = streamlines_C)));
}


# Helper: create a minimal subjects dir with a cube 'midthickness' surface, which
# is used as the context surface in the tests (see the connectome tests).
make.synthetic.context.subject <- function() {
    subjects_dir = tempfile("fsbrain_tracts_subjects_dir_");
    subject_id = "testsubject";
    subject_dir = file.path(subjects_dir, subject_id);
    dir.create(file.path(subject_dir, "surf"), recursive = TRUE);

    cube = freesurferformats::read.fs.surface(system.file("extdata", "cube.ply", package = "fsbrain", mustWork = TRUE));
    cube$vertices = cube$vertices * 50.0;
    for(hemi in c("lh", "rh")) {
        freesurferformats::write.fs.surface(file.path(subject_dir, "surf", sprintf("%s.midthickness", hemi)), cube$vertices, cube$faces);
    }
    return(list("subjects_dir" = subjects_dir, "subject_id" = subject_id));
}


test_that("streamlines.to.segments computes the segments of all streamlines at once", {
    streamlines = make.demo.streamlines(3L, num_points = 4L, seed = 11L);
    segments = streamlines.to.segments(streamlines);

    expect_equal(nrow(segments$from), 3L * 3L);   # 3 streamlines with 4 points each: 3 segments each
    expect_equal(nrow(segments$to), nrow(segments$from));
    expect_equal(segments$lengths, rep(3L, 3L));

    # The result must be identical to the straightforward per-streamline computation.
    naive_from = do.call(rbind, lapply(streamlines, function(m) { m[-nrow(m), , drop = FALSE] }));
    naive_to = do.call(rbind, lapply(streamlines, function(m) { m[-1L, , drop = FALSE] }));
    expect_equal(unname(segments$from), unname(naive_from));
    expect_equal(unname(segments$to), unname(naive_to));
});


test_that("streamlines.to.segments accepts fs.tracts, a single matrix, and handles streamlines without segments", {
    streamlines = make.demo.streamlines(2L, num_points = 3L, seed = 12L);
    tracts = freesurferformats::as.fs.tracts(streamlines);

    segments_fs = streamlines.to.segments(tracts);
    segments_list = streamlines.to.segments(streamlines);
    expect_equal(segments_fs, segments_list);

    # A single matrix is a single streamline.
    expect_equal(nrow(streamlines.to.segments(streamlines[[1L]])$from), 2L);

    # Single-point streamlines have no segment at all, and must not break the others.
    mixed = list(streamlines[[1L]], matrix(c(1, 2, 3), ncol = 3L));
    segments_mixed = streamlines.to.segments(mixed);
    expect_equal(segments_mixed$lengths, c(2L, 0L));
    expect_equal(nrow(segments_mixed$from), 2L);
});


test_that("streamlines.to.segments validates its input", {
    expect_error(streamlines.to.segments("not tracts"), "must be an fs.tracts instance");
    expect_error(streamlines.to.segments(list()), "at least one streamline");
    expect_error(streamlines.to.segments(list(matrix(1:4, ncol = 2L))), "must be a list of \\(n, 3\\) numeric matrices");
    expect_error(streamlines.to.segments(list("nope")), "must be a list of \\(n, 3\\) numeric matrices");
});


test_that("segment.orientation.colors assigns one saturated color per direction axis", {
    from = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0), ncol = 3L, byrow = TRUE);
    to = matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), ncol = 3L, byrow = TRUE);

    axis_colors = segment.orientation.colors(from, to, mode = "axis");
    expect_equal(axis_colors, c("#FF0000", "#00FF00", "#0000FF"));

    rgb_colors = segment.orientation.colors(from, to, mode = "rgb");
    expect_equal(length(rgb_colors), 3L);
    expect_equal(rgb_colors[1L], "#FF0000");

    # A segment without length still gets a color, and does not produce NA.
    degenerate = segment.orientation.colors(matrix(1, nrow = 1L, ncol = 3L), matrix(1, nrow = 1L, ncol = 3L));
    expect_false(anyNA(degenerate));
});


test_that("read.tract.bundles reads a directory of TRK and TCK files", {
    demo = make.demo.tract.dir();
    bundles = read.tract.bundles(demo$dir, silent = TRUE);

    expect_equal(length(bundles), 3L);
    expect_equal(names(bundles), c("bundleA", "bundleB", "bundleC"));
    expect_true(all(vapply(bundles, freesurferformats::is.fs.tracts, logical(1L))));

    expect_equal(length(bundles$bundleA), 3L);
    expect_equal(length(bundles$bundleB), 2L);
    expect_equal(length(bundles$bundleC), 4L);
    expect_equal(freesurferformats::fs.tracts.lengths(bundles$bundleA), rep(5L, 3L));

    expect_equal(attr(bundles, "coords_space"), "ras");
    expect_equal(length(attr(bundles, "files")), 3L);
    expect_equal(names(attr(bundles, "files")), names(bundles));

    # The coordinates must survive the write-read round trip (RAS coordinates
    # written to a TRK file and read back as RAS coordinates), for the file with
    # the identity matrix and for the one with the XTRACT-style LPS grid.
    coords_A = freesurferformats::fs.tracts.coords(bundles$bundleA);
    expect_equal(unname(coords_A), unname(do.call(rbind, demo$streamlines$bundleA)), tolerance = 1e-4);
    coords_B = freesurferformats::fs.tracts.coords(bundles$bundleB);
    expect_equal(unname(coords_B), unname(do.call(rbind, demo$streamlines$bundleB)), tolerance = 1e-4);

    # Reading the file with the non-identity matrix in native coordinates gives
    # different coordinates (they are in the voxel grid of the file, not in RAS).
    coords_native = freesurferformats::fs.tracts.coords(read.tract.bundles(file.path(demo$dir, "bundleB.trk"), coords = "native", silent = TRUE)$bundleB);
    expect_false(isTRUE(all.equal(coords_native, coords_B, tolerance = 1e-3)));
    expect_equal(dim(coords_native), dim(coords_B));

    # The bundle from the TRK file which stores a non-identity matrix carries the
    # per-point data of the reader, i.e., its per-tract accessor returns a list.
    expect_true(is.list(bundles$bundleB[[1L]]));
    expect_true(is.matrix(bundles$bundleC[[1L]]));
});


test_that("read.tract.bundles accepts a vector of file paths and a single file", {
    demo = make.demo.tract.dir();
    files = file.path(demo$dir, c("bundleA.trk", "bundleC.tck"));

    bundles = read.tract.bundles(files, silent = TRUE);
    expect_equal(names(bundles), c("bundleA", "bundleC"));

    single = read.tract.bundles(file.path(demo$dir, "bundleB.trk"), silent = TRUE);
    expect_equal(names(single), "bundleB");
    expect_equal(length(single$bundleB), 2L);
});


test_that("read.tract.bundles applies a transformation matrix", {
    demo = make.demo.tract.dir();
    file_A = file.path(demo$dir, "bundleA.trk");

    coords_plain = freesurferformats::fs.tracts.coords(read.tract.bundles(file_A, silent = TRUE)$bundleA);

    translation = diag(4L);
    translation[1:3, 4L] = c(100.0, -50.0, 25.0);
    coords_moved = freesurferformats::fs.tracts.coords(read.tract.bundles(file_A, transform_matrix = translation, silent = TRUE)$bundleA);

    expect_equal(coords_moved, sweep(coords_plain, 2L, c(100.0, -50.0, 25.0), "+"), tolerance = 1e-9);
    expect_error(read.tract.bundles(file_A, transform_matrix = diag(3L)), "must be a 4x4 numeric matrix");
});


test_that("read.tract.bundles can read only a subset of the streamlines", {
    demo = make.demo.tract.dir();
    file_A = file.path(demo$dir, "bundleA.trk");

    expect_equal(length(read.tract.bundles(file_A, max_tracks = 2L, silent = TRUE)$bundleA), 2L);
    expect_equal(length(read.tract.bundles(file_A, skip_tracks = 2L, silent = TRUE)$bundleA), 1L);

    # A bounding box that contains nothing must not return any streamline.
    far_away = read.tract.bundles(file_A, bbox = c(1000, 2000, 1000, 2000, 1000, 2000), silent = TRUE);
    expect_equal(length(far_away$bundleA), 0L);
});


test_that("read.tract.bundles validates its input", {
    demo = make.demo.tract.dir();
    expect_error(read.tract.bundles(file.path(demo$dir, "nope.trk")), "do not exist");
    expect_error(read.tract.bundles(tempfile("fsbrain_empty_dir_")), "do not exist");
    expect_error(read.tract.bundles(42L), "must be a character string");
    expect_error(read.tract.bundles(file.path(demo$dir, "bundleA.trk"), coords = "nope"), "'arg' should be one of");
    expect_error(read.tract.bundles(file.path(demo$dir, "bundleA.trk"), max_tracks = -1L), "must be a single positive number");

    # An empty directory is reported, not silently ignored (the directory exists in this case).
    empty_dir = tempfile("fsbrain_empty_tract_dir_");
    dir.create(empty_dir);
    expect_error(read.tract.bundles(empty_dir), "does not contain any tract files");

    # Gzip-compressed TRK files are not supported by the freesurferformats reader.
    gz_trk = file.path(demo$dir, "bundleA.trk.gz");
    writeLines("not really a tract file", gz_trk);
    expect_error(read.tract.bundles(gz_trk, silent = TRUE), "not supported");

    # Files with an unsupported extension are rejected.
    other = file.path(demo$dir, "bundleA.txt");
    writeLines("not really a tract file", other);
    expect_error(read.tract.bundles(other, silent = TRUE), "not in TRK or TCK format");
});


test_that("match.bundle.values matches values by name and by position", {
    bundles = make.demo.tract.dir();
    bundle_list = read.tract.bundles(bundles$dir, silent = TRUE);

    by_position = match.bundle.values(c(1.0, 2.0, 3.0), bundle_list);
    expect_equal(by_position, c(1.0, 2.0, 3.0));

    # Named values are matched to the bundles, no matter in which order they are given.
    by_name = match.bundle.values(c(bundleC = 3.0, bundleA = 1.0, bundleB = 2.0), bundle_list);
    expect_equal(by_name, c(1.0, 2.0, 3.0));

    expect_null(match.bundle.values(NULL, bundle_list));
    expect_error(match.bundle.values(c(1.0, 2.0), bundle_list), "has 2 entries, but there are 3 bundles");
    expect_error(match.bundle.values(c(bundleA = 1.0), bundle_list), "without a value in 'bundle_values'");
    expect_error(match.bundle.values("a", bundle_list), "must be a numeric vector");
});


test_that("vis.tracts builds a tract renderable from a directory of tract files", {
    demo = make.demo.tract.dir();
    r = vis.tracts(demo$dir, context = NULL, views = NULL, silent = TRUE);

    expect_true("tracts" %in% names(r));
    expect_true(is.fs.coloredpaths(r$tracts));
    expect_equal(nrow(r$tracts$from), 3L * 4L + 2L * 6L + 4L * 5L);   # segments of all streamlines
    expect_equal(nrow(r$tracts$to), nrow(r$tracts$from));
    expect_true(is.null(r$context_lh));
    expect_equal(length(unique(r$tracts$col)), 1L);   # a single default color
    expect_equal(unique(r$tracts$width), 1.0);
});


test_that("vis.tracts maps bundle values to colors and provides colorbar metadata", {
    demo = make.demo.tract.dir();
    bundles = read.tract.bundles(demo$dir, silent = TRUE);
    values = c(bundleA = 0.1, bundleB = 0.5, bundleC = 0.9);

    r = vis.tracts(bundles, bundle_values = values, context = NULL, views = NULL, silent = TRUE);
    expect_equal(length(unique(r$tracts$col)), 3L);
    expect_equal(r$tracts$metadata$src_data, c(0.1, 0.5, 0.9));
    expect_equal(r$tracts$metadata$data_range, c(0.1, 0.9));
    expect_false(is.null(r$tracts$metadata$makecmap_options));

    # Each bundle is drawn in its own color, and the colors are the ones of the colorlayer.
    segment_counts = c(3L * 4L, 2L * 6L, 4L * 5L);
    expect_equal(r$tracts$col, rep(r$tracts$col[c(1L, segment_counts[1L] + 1L, segment_counts[1L] + segment_counts[2L] + 1L)],
                                   segment_counts));

    r2 = vis.tracts(bundles, bundle_values = values, tract_color = "#00FF00", context = NULL, views = NULL, silent = TRUE);
    expect_equal(r2$tracts$col, r$tracts$col);   # 'tract_color' is ignored when values are given
});


test_that("vis.tracts supports a single color and per-segment orientation colors", {
    demo = make.demo.tract.dir();

    r = vis.tracts(demo$dir, tract_color = "#00FF00", context = NULL, views = NULL, silent = TRUE);
    expect_equal(unique(r$tracts$col), "#00FF00");

    expect_error(vis.tracts(demo$dir, tract_color = c("#00FF00", "#FF0000"), context = NULL, views = NULL, silent = TRUE),
                 "must be a single hex color string");

    # Orientation colors need segments in different directions: one bundle along
    # each axis gives exactly the three axis colors.
    axis_streamlines = list(cbind(seq(0.0, 20.0, length.out = 5L), 0.0, 0.0),
                            cbind(0.0, seq(0.0, 20.0, length.out = 5L), 0.0),
                            cbind(0.0, 0.0, seq(0.0, 20.0, length.out = 5L)));
    r_orient = vis.tracts(axis_streamlines, color_by_orientation = TRUE, context = NULL, views = NULL, silent = TRUE);
    expect_equal(sort(unique(r_orient$tracts$col)), c("#0000FF", "#00FF00", "#FF0000"));
    expect_equal(length(r_orient$tracts$metadata), 0L);   # no colorbar for orientation colors
});


test_that("vis.tracts scales the line width by the bundle values", {
    demo = make.demo.tract.dir();
    values = c(bundleA = 0.0, bundleB = 5.0, bundleC = 10.0);

    r = vis.tracts(demo$dir, bundle_values = values, tract_width_scale = "value",
                   tract_width = 2.0, context = NULL, views = NULL, silent = TRUE);
    expect_equal(sort(unique(r$tracts$width)), c(1.0, 2.0, 3.0));

    r_range = vis.tracts(demo$dir, bundle_values = values, tract_width_scale = "value",
                         tract_width_range = c(1.0, 4.0), context = NULL, views = NULL, silent = TRUE);
    expect_equal(sort(unique(r_range$tracts$width)), c(1.0, 2.5, 4.0));

    # Without value scaling, all segments of all bundles have the same width.
    r_plain = vis.tracts(demo$dir, tract_width = 1.5, context = NULL, views = NULL, silent = TRUE);
    expect_equal(unique(r_plain$tracts$width), 1.5);
});


test_that("vis.tracts accepts in-memory tracts", {
    streamlines = make.demo.streamlines(2L, num_points = 4L, seed = 21L);

    r_list = vis.tracts(streamlines, context = NULL, views = NULL, silent = TRUE);
    expect_equal(nrow(r_list$tracts$from), 2L * 3L);

    r_fs = vis.tracts(freesurferformats::as.fs.tracts(streamlines), context = NULL, views = NULL, silent = TRUE);
    expect_equal(r_list$tracts$from, r_fs$tracts$from);

    r_single = vis.tracts(streamlines[[1L]], context = NULL, views = NULL, silent = TRUE);
    expect_equal(nrow(r_single$tracts$from), 3L);

    # A list of bundles, i.e., one entry per bundle, with per-bundle values.
    bundles = list("left" = streamlines, "right" = make.demo.streamlines(3L, num_points = 5L, seed = 22L));
    r_bundles = vis.tracts(bundles, bundle_values = c(left = 1.0, right = 2.0), context = NULL, views = NULL, silent = TRUE);
    expect_equal(length(unique(r_bundles$tracts$col)), 2L);
    expect_equal(nrow(r_bundles$tracts$from), 2L * 3L + 3L * 4L);
});


test_that("vis.tracts validates its input", {
    demo = make.demo.tract.dir();
    expect_error(vis.tracts(demo$dir, tract_width = -1.0, context = NULL, views = NULL, silent = TRUE), "must be a single positive number");
    expect_error(vis.tracts(demo$dir, tract_width_range = c(2.0, 1.0), context = NULL, views = NULL, silent = TRUE), "increasing positive values");
    expect_error(vis.tracts(demo$dir, tract_width_scale = "value", context = NULL, views = NULL, silent = TRUE), "requires 'bundle_values'");
    expect_error(vis.tracts(demo$dir, context = list("nope" = 1), views = NULL, silent = TRUE), "Unknown entry/entries in parameter 'context'");
    expect_error(vis.tracts(demo$dir, context = list("alpha" = 2.0), views = NULL, silent = TRUE), "must be a single number in the range 0 to 1");
    expect_error(vis.tracts(demo$dir, color_by_orientation = "yes", context = NULL, views = NULL, silent = TRUE), "must be a single logical value");
    expect_error(vis.tracts(list(), context = NULL, views = NULL, silent = TRUE), "must be a character string");

    # Streamlines with a single point have no segment, so there is nothing to draw.
    expect_error(vis.tracts(list(matrix(c(1, 2, 3), ncol = 3L)), context = NULL, views = NULL, silent = TRUE),
                 "do not contain any line segment");
});


test_that("vis.tracts draws the context surface from a template", {
    demo = make.demo.tract.dir();
    subject = make.synthetic.context.subject();

    r = vis.tracts(demo$dir, subjects_dir = subject$subjects_dir, template_id = subject$subject_id,
                   context = list("surface" = "midthickness", "alpha" = 0.2), views = NULL, silent = TRUE);

    expect_equal(names(r), c("tracts", "context_lh", "context_rh"));
    expect_true(fsbrain.renderable(r$context_lh));
    expect_null(r$context_lh$metadata$makecmap_options);   # not a colorbar renderable

    # Without context, no surface is loaded (this also means that no template is needed).
    r_no_context = vis.tracts(demo$dir, context = NULL, views = NULL, silent = TRUE);
    expect_equal(names(r_no_context), "tracts");
});


test_that("vis.tracts renders with the rgl backend", {
    skip_if_rgl_required();
    demo = make.demo.tract.dir();
    streamlines = make.demo.streamlines(2L, num_points = 4L, seed = 31L);

    expect_silent(vis.tracts(streamlines, context = NULL, views = c("sd_lateral_lh"),
        rgloptions = list("windowRect" = c(0, 0, 200, 200)), silent = TRUE));
    close.all.rgl.windows();
});


test_that("vis.tracts renders with the scimesh backend and exports with a colorbar", {
    skip_if_not_installed("scimesh");
    demo = make.demo.tract.dir();
    out_img = tempfile(fileext = ".png");

    withr::local_options(list(fsbrain.renderer_backend = "scimesh"));
    withr::local_dir(tempdir());

    r = vis.tracts(demo$dir, bundle_values = c(bundleA = 0.2, bundleB = 0.5, bundleC = 0.9),
                   context = NULL, views = c("sd_lateral_lh"), silent = TRUE);

    expect_silent(export(r, draw_colorbar = "horizontal", output_img = out_img, silent = TRUE));
    expect_true(file.exists(out_img));
    expect_true(file.size(out_img) > 1000L);
});


test_that("vis.tracts works with a downloaded XTRACT atlas if it is available", {
    skip_if_rgl_required();
    atlas_dirs = tryCatch(list.tract.bundle.files(file.path(get_optional_data_filepath("tracts", mustWork = FALSE), "xtract_tiny")), error = function(e) character(0L));
    skip_if(length(atlas_dirs) < 1L, "The XTRACT tract atlas is not available, run download_xtract_tracts('xtract_tiny') to get it.");

    bundles = read.tract.bundles(dirname(atlas_dirs[1L]), silent = TRUE);
    expect_true(length(bundles) > 30L);

    values = seq(0.0, 1.0, length.out = length(bundles));
    names(values) = names(bundles);
    r = vis.tracts(bundles, bundle_values = values, context = NULL, views = c("sd_lateral_lh"), silent = TRUE);
    expect_true(is.fs.coloredpaths(r$tracts));
    expect_true(nrow(r$tracts$from) > 10000L);
    close.all.rgl.windows();
});
