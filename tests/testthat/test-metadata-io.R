test_that("Subjects file can  be read", {
    subjects_file = system.file("extdata", "subjects.txt", package = "fsbrain", mustWork = TRUE);
    subjects_list = read.md.subjects(subjects_file);
    expect_equal(length(subjects_list), 3);
    expect_equal(subjects_list[1], "subject1");
    expect_equal(subjects_list[2], "subject2");
    expect_equal(subjects_list[3], "subject3");
})


test_that("Demographics file with header can be read with stringsAsFactors = TRUE", {
  demogr_file = system.file("extdata", "demographics.tsv", package = "fsbrain", mustWork = TRUE);
  column_names = c("subject_id", "group", "age");
  demographics = read.md.demographics(demogr_file, column_names = column_names);
  expect_equal(nrow(demographics), 6);
  expect_equal(ncol(demographics), 3);
  expect_equal(class(demographics$subject_id), "factor");
  expect_equal(class(demographics$group), "factor");
  expect_equal(class(demographics$age), "integer");
})


test_that("Demographics reports can be generated.", {
  demogr_file = system.file("extdata", "demographics.tsv", package = "fsbrain", mustWork = TRUE);
  column_names = c("subject_id", "group", "age");
  demographics = read.md.demographics(demogr_file, column_names = column_names);

  # The expect_warning wrapper in the next line ignores the warning from the t.test function that it cannot compute exact p-values with ties.
  expect_warning(report_unpaired <- report.on.demographics(demographics, group_column_name="group"));
  expect_equal(length(report_unpaired), 11);  # check number of lines in report

  report_paired <- report.on.demographics(demographics, group_column_name="group", paired=TRUE);
  expect_equal(length(report_paired), 10);   # check number of lines in report
})


test_that("Demographics file with header can be read with stringsAsFactors = FALSE", {
  demogr_file = system.file("extdata", "demographics.tsv", package = "fsbrain", mustWork = TRUE);
  column_names = c("subject_id", "group", "age");
  demographics = read.md.demographics(demogr_file, column_names = column_names, report = FALSE, stringsAsFactors = FALSE);
  expect_equal(nrow(demographics), 6);
  expect_equal(ncol(demographics), 3);
  expect_equal(class(demographics$subject_id), "character");
  expect_equal(class(demographics$group), "character");
  expect_equal(class(demographics$age), "integer");
})


test_that("Demographics file reading fails with incorrect number of column names", {
  demogr_file = system.file("extdata", "demographics.tsv", package = "fsbrain", mustWork = TRUE);
  column_names = c("subject_id", "group", "age", "nosuchfield");
  expect_error(demographics = read.md.demographics(demogr_file, column_names = column_names, report = FALSE));
})
