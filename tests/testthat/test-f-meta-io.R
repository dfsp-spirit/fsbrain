test_that("Subjects file can  be read", {
    subjects_file = system.file("extdata", "subjects.txt", package = "nitools", mustWork = TRUE);
    subjects_list = read.subjects(subjects_file);
    expect_equal(length(subjects_list), 3);
})

test_that("Demographics file can  be read", {
  demogr_file = system.file("extdata", "demographics.tsv", package = "nitools", mustWork = TRUE);
  demographics = read.demographics(demogr_file);
  expect_equal(nrow(demographics), 4);
})
