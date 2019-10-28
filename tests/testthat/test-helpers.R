test_that("The uppercase function works", {
  expect_equal(fup("pearson"), "Pearson");
  expect_equal(fup("kendall correlation"), "Kendall correlation");
  expect_equal(fup(""), "");
  expect_equal(fup(c("kendall", "pearson")), c("Kendall", "Pearson"));
})

test_that("Data is clipped correctly", {
    full_data = rnorm(50, 10, 1);
    clipped = clip.data(full_data);

    expect_true(min(full_data) <= min(clipped));
    expect_true(max(full_data) >= max(clipped));
    expect_equal(length(full_data), length(clipped));
})

