

test_that("We can retrieve recommended cmap functions.", {

    # Check that we get a function:
    expect_true(is.function(cm.seq()));
    expect_true(is.function(cm.heat()));
    expect_true(is.function(cm.div()));
    expect_true(is.function(cm.qual()));

    # Test that the correct number of colors is returned.
    expect_equal(length(cm.seq()(5)), 5L);
    expect_equal(length(cm.heat()(5)), 5L);
    expect_equal(length(cm.div()(5)), 5L);
    expect_equal(length(cm.qual()(5)), 5L);
})


test_that("We can retrieve recommended mkcmap_options.", {
    # Check that we get lists:
    expect_true(is.list(mkco.seq()));
    expect_true(is.list(mkco.heat()));
    expect_true(is.list(mkco.div()));
})


test_that("A colorbar can only be plotted if the metadata contains a usable data range.", {
    mkco = mkco.seq();
    expect_true(can.plot.colorbar(c(1, 2), mkco));
    # A degenerate data range (all values identical) cannot be plotted, the plotting code
    # would fail with 'increasing x and y values expected'.
    expect_false(can.plot.colorbar(c(1, 1), mkco));
    expect_false(can.plot.colorbar(c(0, 0), mkco));
    expect_false(can.plot.colorbar(c(NA, 1), mkco));
    expect_false(can.plot.colorbar(c(1, 1, 2), mkco));
    # Missing metadata does not allow plotting a colorbar either.
    expect_false(can.plot.colorbar(NULL, mkco));
    expect_false(can.plot.colorbar(c(1, 2), NULL));
    expect_false(can.plot.colorbar(c(1, 2), list('colFn' = 'not a function')));
})
