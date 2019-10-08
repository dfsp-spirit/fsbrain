test_that("Subjects file can  be read", {
    subjects_file = system.file("extdata", "subjects.txt", package = "nitools", mustWork = TRUE);
    subjects_list = read.subjects(subjects_file);
    expect_equal(length(subjects_list), 3);
})


test_that("Demographics file can  be read", {
  demogr_file = system.file("extdata", "demographics.tsv", package = "nitools", mustWork = TRUE);
  column_names = c("subject_id", "group", "age");
  demographics = read.demographics(demogr_file, column_names = column_names, report = FALSE);
  expect_equal(nrow(demographics), 4);
})


test_that("Demographics file reading fails with incorrect number of header names", {
  demogr_file = system.file("extdata", "demographics.tsv", package = "nitools", mustWork = TRUE);
  column_names = c("subject_id", "group", "age", "nosuchfield");
  expect_error(demographics = read.demographics(demogr_file, column_names = column_names, report = FALSE));
})
