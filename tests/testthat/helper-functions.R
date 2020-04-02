# Helper functions for the unit tests, these can be used in any test.

#' @title  Determines whether the full FreeSurfer output for subject1 is available.
box.has.all.testdata <- function() {
  return(box.has.full.subject1() & box.has.freesurfer());
}

box.can.run.all.tests <- function() {
  # To run all tests, we need a few things we do not explicitly check for, e.g.,
  # we need to be able to write files to the home directory and have all optional R packages.
  # Therefore, we require the evironment variable RUN_ALL_FSBRAIN_TESTS to be set
  # for this to return TRUE.
  #
  # This should only return TRUE on one of my (Tim's) computers, which have the full FreeSurfer data for subject1.
  # If I want to run all the tests, I still have to set the env var RUN_ALL_FSBRAIN_TESTS, e.g.,:
  #
  #     Sys.setenv("RUN_ALL_FSBRAIN_TESTS"="fosho");
  #
  return(box.has.all.testdata() & box.has.x11display() & nchar(Sys.getenv("RUN_ALL_FSBRAIN_TESTS")) > 0L);
}

run.extralong.tests <- function() {
  return(box.can.run.all.tests() & (Sys.getenv("RUN_ALL_FSBRAIN_TESTS") == "with_extra_long"));
}

box.has.full.subject1 <- function() {
  subjects_dir = testdatapath.subjectsdir.full.subject1();
  return(dir.exists(file.path(subjects_dir, 'subject1', 'surf')));
}


box.has.freesurfer <- function() {
  return(fsbrain::find.freesurferhome()$found);
}


box.has.fsaverage <- function() {
  return(fsbrain::find.subjectsdir.of("fsaverage")$found);
}


box.has.x11display <- function() {
  # I'm not really sure whether this is a great way to check for an X11 display
  return(nchar(Sys.getenv("DISPLAY")) > 0L);
}


#' @title Get path that holds full FreeSurfer output for subject1
testdatapath.subjectsdir.full.subject1 <- function () {
  return(file.path("~/data/subject1_only/"));
}

