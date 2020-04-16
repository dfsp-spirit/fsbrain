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
  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
  fsbrain::download_optional_data();
  subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
  skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

  surface = subject.surface(subjects_dir, "subject1", "white", "lh");

  source_vertices = c(1);
  n = mesh.vertex.neighbors(surface, source_vertices);
  expect_equal(n$faces, c(1,3,5))
  expect_equal(n$vertices, c(1,2,97,6));
})


test_that("A hemi list can be unwrapped", {
  data = rep(42L, 10L);
  hemi_list = list("lh"=data);

  unwrapped_no_hemi_arg = hemilist.unwrap(hemi_list);
  unwrapped_with_hemi_arg = hemilist.unwrap(hemi_list, 'lh');

  expect_equal(unwrapped_no_hemi_arg, data);
  expect_equal(unwrapped_with_hemi_arg, data);

  # test with incorrect hemi args
  expect_true(is.null(hemilist.unwrap(hemi_list, 'rh')));    # not in this list
  expect_error(hemilist.unwrap(hemi_list, 'invalid_hemi'));  # never valid
})


test_that("Data can be wrapped into a hemi list", {
  data = rep(42L, 10L);

  hemi_list_lh = hemilist.wrap(data, 'lh');
  expect_true(is.list(hemi_list_lh));
  expect_equal(length(hemi_list_lh), 1L);
  expect_equal(hemi_list_lh$lh, data);

  hemi_list_rh = hemilist.wrap(data, 'rh');
  expect_true(is.list(hemi_list_rh));
  expect_equal(length(hemi_list_rh), 1L);
  expect_equal(hemi_list_rh$rh, data);

  # test with incorrect hemi args
  expect_error(hemilist.wrap(data, 'invalid_hemi'));  # never valid
})

