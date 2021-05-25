

test_that("The transform matrix for rotations can be computed", {

    point = c(2, 2, 2);
    res = matrix(freesurferformats::doapply.transform.mtx(point, rotation.matrix.for.axis.rot(pi/2, 1, 0, 0)), ncol = 3, byrow = TRUE);
    expected = rgl::rotate3d(point, pi/2, 1, 0, 0);

    testthat::expect_equal(res, expected);
})


