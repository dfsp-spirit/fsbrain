

test_that("We can append images with different width/height using our custom wrapper function.", {

    im1 = magick::image_blank(50, 55, "red");
    im2 = magick::image_blank(60, 50, "blue");
    images = c(im1, im2);

    images_v = fsbrain:::wrapped.image.append(images, stack = T, background_color = "green");
    images_h = fsbrain:::wrapped.image.append(images, stack = F, background_color = "green");

    # This currently is not the greatest test: it only checks whether the functions return
    # without error. One has to look at the returned images to see whether it worked out.

    expect_equal(1L, 1L); # Empty tests will be skipped by testthat.
})


