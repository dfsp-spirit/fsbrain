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


test_that("The neigborhood of a vertex is computed correctly", {
  fsbrain::download_optional_data();
  subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
  skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

  surface = subject.surface(subjects_dir, "subject1", "white", "lh");

  source_vertices = c(1);
  n = mesh.vertex.neighbors(surface, source_vertices);
  expect_equal(n$faces, c(1,3,5))
  expect_equal(n$vertices, matrix(c(1,2,6,1,97,2,1,6,97), nrow=3, byrow=TRUE));
})
