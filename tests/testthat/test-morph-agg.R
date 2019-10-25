

test_that("Aggregation of native space whole brain morph data on group level works", {
  fsbrain::download_optional_data();
  subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
  skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

  subjects_list = c("subject1", "subject2")
  data = group.morph.agg.native(subjects_dir, subjects_list, "thickness", "lh")

  expect_equal(class(data), "data.frame")
  expect_equal(nrow(data), 2)  # the 2 subjects
  expect_equal(ncol(data), 4)  # 4 rows, see below
  expect_true("subject_id" %in% colnames(data))
  expect_true("hemi" %in% colnames(data))
  expect_true("measure_name" %in% colnames(data))
  expect_true("measure_value" %in% colnames(data))
  expect_equal(data$subject_id[0], subjects_list[0])
  expect_equal(data$subject_id[1], subjects_list[1])
})


test_that("Aggregation of native space whole brain morph data on group level works with cast set to FALSE", {
  fsbrain::download_optional_data();
  subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
  skip_if_not(dir.exists(subjects_dir), message="Test data missing.");

  subjects_list = c("subject1", "subject2")
  data = group.morph.agg.native(subjects_dir, subjects_list, "thickness", "lh", cast=FALSE)

  expect_equal(class(data), "data.frame")
  expect_equal(nrow(data), 2)  # the 2 subjects
  expect_equal(ncol(data), 2)  # 2 rows: "subject_id", "lh.thickness"
  expect_true("subject_id" %in% colnames(data))
  expect_true("lh.thickness" %in% colnames(data))
  expect_equal(data$subject_id[0], subjects_list[0])
  expect_equal(data$subject_id[1], subjects_list[1])
})




test_that("Aggregation of standard space whole brain morph data on group level works with cast=TRUE", {
  fsbrain::download_optional_data();
  subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
  skip_if_not(dir.exists(subjects_dir), message="Test data missing."); # skip on travis

  subjects_list = c("subject1", "subject2")
  data = group.morph.agg.standard(subjects_dir, subjects_list, "thickness", "lh", fwhm="10", cast=TRUE)

  expect_equal(class(data), "data.frame")
  expect_equal(nrow(data), 2)
  expect_equal(ncol(data), 4)
  expect_equal(data$subject_id[0], subjects_list[0])
  expect_equal(data$subject_id[1], subjects_list[1])
  cols = colnames(data)
  expect_true("subject_id" %in% cols)
  expect_true("hemi" %in% cols)
  expect_true("measure_name" %in% cols)
  expect_true("measure_value" %in% cols)
})


test_that("Aggregation of standard space whole brain morph data on group level works for several measures and hemis with cast=FALSE", {
  fsbrain::download_optional_data();
  subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
  skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip on travis

  subjects_list = c("subject1", "subject2")
  data = group.multimorph.agg.standard(subjects_dir, subjects_list, c("thickness", "area"), c("lh", "rh"), fwhm="10", cast=FALSE)

  expect_equal(class(data), "data.frame")
  expect_equal(nrow(data), 2)
  expect_equal(ncol(data), 5)
  expect_equal(data$subject_id[0], subjects_list[0])
  expect_equal(data$subject_id[1], subjects_list[1])
  cols = colnames(data)
  expect_true("subject_id" %in% cols)
  expect_true("lh.area" %in% cols)
  expect_true("rh.area" %in% cols)
  expect_true("lh.thickness" %in% cols)
  expect_true("rh.thickness" %in% cols)
})


test_that("Aggregation of native space whole brain morph data on group level works for several measures and hemis", {
  fsbrain::download_optional_data();
  subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
  skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip on travis

  subjects_list = c("subject1", "subject2")
  data = group.multimorph.agg.native(subjects_dir, subjects_list, c("thickness", "area"), c("lh", "rh"), cast=FALSE)

  expect_equal(class(data), "data.frame")
  expect_equal(nrow(data), 2)
  expect_equal(ncol(data), 5)
  expect_equal(data$subject_id[0], subjects_list[0])
  expect_equal(data$subject_id[1], subjects_list[1])
  cols = colnames(data)
  expect_true("subject_id" %in% cols)
  expect_true("lh.area" %in% cols)
  expect_true("rh.area" %in% cols)
  expect_true("lh.thickness" %in% cols)
  expect_true("rh.thickness" %in% cols)
})


test_that("Aggregation of native space whole brain morph data on group level works for several measures and hemis with cast=TRUE", {
  fsbrain::download_optional_data();
  subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
  skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip on travis

  subjects_list = c("subject1", "subject2")
  data = group.multimorph.agg.native(subjects_dir, subjects_list, c("thickness", "area"), c("lh", "rh"), cast=TRUE)

  expect_equal(class(data), "data.frame")
  expect_equal(nrow(data), 8) # 2 subjects x 2 measures x 2 hemis = 8
  expect_equal(ncol(data), 4)
  expect_equal(data$subject_id[0], subjects_list[0])
  expect_equal(data$subject_id[1], subjects_list[1])
  cols = colnames(data)
  expect_true("subject_id" %in% cols)
  expect_true("hemi" %in% cols)
  expect_true("measure_name" %in% cols)
  expect_true("measure_value" %in% cols)
})


test_that("Aggregation of standard space whole brain morph data on group level works for several measures and hemis with cast=TRUE", {
  fsbrain::download_optional_data();
  subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
  skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip on travis

  subjects_list = c("subject1", "subject2")
  data = group.multimorph.agg.standard(subjects_dir, subjects_list, c("thickness", "area"), c("lh", "rh"), fwhm="10", cast=TRUE)

  expect_equal(class(data), "data.frame")
  expect_equal(nrow(data), 8)
  expect_equal(ncol(data), 4)
  expect_equal(data$subject_id[0], subjects_list[0])
  expect_equal(data$subject_id[1], subjects_list[1])
  cols = colnames(data)
  expect_true("subject_id" %in% cols)
  expect_true("hemi" %in% cols)
  expect_true("measure_name" %in% cols)
  expect_true("measure_value" %in% cols)
})
