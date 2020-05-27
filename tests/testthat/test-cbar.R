

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
