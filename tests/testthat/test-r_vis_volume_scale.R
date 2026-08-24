# Tests for the `scale` parameter (integer nearest-neighbor upscaling of slice
# images, with supersampled smooth contour lines) added to the volume slice
# visualization functions in R/vis_volume.R.


#' @title Get pixel indices of red pixels in a magick image.
#'
#' @description Helper for the unit tests: returns the indices of all pixels
#' whose color is clearly red (high R, low G and B), as a matrix with two
#' columns (the two spatial dimensions, orientation-agnostic).
#'
#' @param img a magick image instance.
#'
#' @return integer matrix with 2 columns, one row per red pixel.
#'
#' @keywords internal
get.red.pixel.indices <- function(img) {
    arr <- as.integer(img[[1]]);
    idx <- which(arr[,,1] > 200 & arr[,,2] < 60 & arr[,,3] < 60, arr.ind = TRUE);
    return(idx);
}


test_that("draw.segments.on.image maps coordinates correctly with coord_scale", {
    # A horizontal segment in CRS coordinates: row (axis 2) = 20, cols (axis 3) = 5..15.
    # slice_axis=1 -> other_axes = c(2,3); row_axis=2 -> col 1; col_axis=3 -> col 2.
    seg <- rbind(c(20, 5), c(20, 15));

    # At native resolution (coord_scale=1), the segment must land near 1-based
    # pixel row 20*1+1 = 21 (the magick graphics device draws with a small
    # constant offset of ~1-2 px, which is identical for all scales).
    img1 <- magick::image_blank(30, 30, "black");
    img1 <- fsbrain:::draw.segments.on.image(img1, list(seg),
        slice_axis = 1L, row_axis = 2L, col_axis = 3L,
        color = "red", lwd = 1, coord_scale = 1L);
    red_pixels1 <- get.red.pixel.indices(img1);
    expect_true(nrow(red_pixels1) >= 10);   # horizontal line ~11 px wide
    uniq1 <- apply(red_pixels1, 2, function(col) length(unique(col)));
    expect_true(min(uniq1) == 1L);          # exactly one dim constant -> horizontal line
    const_val1 <- unique(as.vector(red_pixels1[, which(uniq1 == 1L)]));
    expect_true(const_val1 >= 20 && const_val1 <= 24);

    # On a 2x scaled image (coord_scale=2), the same CRS coordinates must map to
    # roughly double the pixel row (20*2+1 = 41, plus the same small offset).
    img2 <- magick::image_blank(60, 60, "black");
    img2 <- fsbrain:::draw.segments.on.image(img2, list(seg),
        slice_axis = 1L, row_axis = 2L, col_axis = 3L,
        color = "red", lwd = 1, coord_scale = 2L);
    expect_equal(magick::image_info(img2)$width, 60L);
    red_pixels2 <- get.red.pixel.indices(img2);
    expect_true(nrow(red_pixels2) >= 20);   # ~21 px wide
    uniq2 <- apply(red_pixels2, 2, function(col) length(unique(col)));
    expect_true(min(uniq2) == 1L);
    const_val2 <- unique(as.vector(red_pixels2[, which(uniq2 == 1L)]));
    expect_true(const_val2 >= 40 && const_val2 <= 44);
    # The scaled position must be ~2x the native position (linear coordinate mapping).
    expect_equal(const_val2, 2 * const_val1, tolerance = 3);
});


test_that("volvis.lightbox NN-upscales slices with scale (blocky)", {
    volume <- array(0.0, dim = c(8, 8, 8));
    volume[4, 4, 4] <- 1.0;   # single bright voxel

    lb <- volvis.lightbox(volume, slices = 4, axis = 3L, scale = 2L,
        per_row = 1L, border_geometry = NULL);
    info <- magick::image_info(lb);
    expect_equal(info$width, 16L);    # 8 * 2
    expect_equal(info$height, 16L);

    # The single bright voxel must become a 2x2 block of white pixels
    # (nearest-neighbor upscaling, no smoothing).
    arr <- as.integer(lb[[1]]);
    bright <- sum(arr[,,1] > 200 & arr[,,2] > 200 & arr[,,3] > 200);
    expect_equal(bright, 4L);
});


test_that("scale parameter is validated", {
    testthat::skip_on_cran();

    fsbrain::download_optional_data();
    subjects_dir <- fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message = "Test data missing.");

    expect_error(volvis.slices.with.surface(subjects_dir, "subject1",
        volume = "brain", surface = "white", axis = 1L, slices = 120,
        scale = 0, output_dir = tempfile()));
    expect_error(volvis.slices.with.surface(subjects_dir, "subject1",
        volume = "brain", surface = "white", axis = 1L, slices = 120,
        scale = 1.5, output_dir = tempfile()));
    expect_error(volvis.lightbox(array(0, dim = c(4, 4, 4)), scale = 0));
});


test_that("volvis.slices.with.surface scales exported slice images", {
    testthat::skip_on_cran();
    testthat::skip_on_travis();

    fsbrain::download_optional_data();
    subjects_dir <- fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message = "Test data missing.");

    outdir <- tempfile();
    slices <- 120;
    f1 <- volvis.slices.with.surface(subjects_dir, "subject1",
        volume = "brain", surface = "white", axis = 1L,
        slices = slices, scale = 1L, output_dir = outdir);
    f2 <- volvis.slices.with.surface(subjects_dir, "subject1",
        volume = "brain", surface = "white", axis = 1L,
        slices = slices, scale = 2L, output_dir = outdir);

    expect_equal(length(f1), length(f2));
    # The scale suffix must differ, so the two scales must not overwrite each other.
    expect_true(basename(f2[1]) != basename(f1[1]));
    expect_match(basename(f1[1]), "_scl1\\.png$");
    expect_match(basename(f2[1]), "_scl2\\.png$");

    info1 <- magick::image_info(magick::image_read(f1[1]));
    info2 <- magick::image_info(magick::image_read(f2[1]));
    expect_equal(info2$width, 2L * info1$width);
    expect_equal(info2$height, 2L * info1$height);
    expect_gt(info2$width, info1$width);

    unlink(outdir, recursive = TRUE);
});


test_that("get.slice.indices accepts explicit slice index vectors", {
    # Regression test: a length > 1 slice index vector previously crashed
    # with "condition has length > 1" because of a scalar/vector `&` mixup.
    idx <- fsbrain:::get.slice.indices(c(10L, 10L, 10L), 1L, c(2L, 5L, 7L));
    expect_equal(idx, c(2L, 5L, 7L));
    # A single negative value still means "every nth slice".
    idx2 <- fsbrain:::get.slice.indices(c(10L, 10L, 10L), 1L, -3L);
    expect_equal(idx2, c(1L, 4L, 7L, 10L));
    # Out-of-bounds and negative-in-vector still error.
    expect_error(fsbrain:::get.slice.indices(c(10L, 10L, 10L), 1L, c(2L, 12L)));
    expect_error(fsbrain:::get.slice.indices(c(10L, 10L, 10L), 1L, c(2L, -1L)));
});


test_that("volvis.lb.with.surface scales the lightbox image", {
    testthat::skip_on_cran();
    testthat::skip_on_travis();

    fsbrain::download_optional_data();
    subjects_dir <- fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message = "Test data missing.");

    # No border and a single slice so the tile dimensions scale exactly 2x.
    lb1 <- volvis.lb.with.surface(subjects_dir, "subject1",
        volume = "brain", surface = "white", axis = 1L,
        slices = 120, scale = 1L, per_row = 1L, border_geometry = NULL);
    lb2 <- volvis.lb.with.surface(subjects_dir, "subject1",
        volume = "brain", surface = "white", axis = 1L,
        slices = 120, scale = 2L, per_row = 1L, border_geometry = NULL);

    i1 <- magick::image_info(lb1);
    i2 <- magick::image_info(lb2);
    expect_equal(i2$width, 2L * i1$width);
    expect_equal(i2$height, 2L * i1$height);
});
