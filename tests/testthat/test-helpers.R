test_that("The uppercase function works", {
  expect_equal(fup("pearson"), "Pearson");
  expect_equal(fup("kendall correlation"), "Kendall correlation");
  expect_equal(fup(""), "");
  expect_equal(fup(c("kendall", "pearson")), c("Kendall", "Pearson"));
})
