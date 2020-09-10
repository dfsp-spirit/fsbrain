# Helper functions for the unit tests, these can be used in any test.

#' @title Determine whether a test is running on CRAN under macos
#'
#' @description We are currently getting failed unit tests on CRAN under macos, while the package works under MacOS on both <https://builder.r-hub.io/> and on our MacOS machines. This is because the package file cache does not work on CRAN, as the HOME is mounted read-only on the CRAN test systems. So we have to skip the tests that require optional data under MacOS on CRAN.
#'
#' @return logical, whether a test is running on CRAN under MacOS
tests_running_on_cran_under_macos <- function() {
    return(tolower(Sys.info()[["sysname"]]) == 'darwin' && !identical(Sys.getenv("NOT_CRAN"), "true"));
}


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
  #     Sys.setenv("RUN_ALL_FSBRAIN_TESTS"="sure");
  #
  #     if you have the time:
  #
  #     Sys.setenv("RUN_ALL_FSBRAIN_TESTS"="sure");
  #     # See run.extralong.tests() below.
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


#' @title Get coloredmesh for unit tests.
get.demo.coloredmesh <- function(add_cbar_metadata = TRUE) {
  cube_mesh = freesurferformats::read.fs.surface(system.file("extdata", "cube.ply", package = "fsbrain", mustWork = TRUE));
  morph_data = seq.int(nrow(cube_mesh$vertices));
  cm_lh = coloredmesh.from.preloaded.data(cube_mesh, morph_data = morph_data, hemi = 'lh');
  if(add_cbar_metadata) {
    cm_lh$metadata = list('makecmap_options' = mkco.seq(), 'src_data'=morph_data);
  }
  return(cm_lh);
}


#' @title Get hemilist of coloredmeshes for unit tests.
get.demo.coloredmeshes.hemilist <- function(add_cbar_metadata = TRUE) {
  cube_mesh = freesurferformats::read.fs.surface(system.file("extdata", "cube.ply", package = "fsbrain", mustWork = TRUE));
  morph_data = seq.int(nrow(cube_mesh$vertices));
  cm_lh = coloredmesh.from.preloaded.data(cube_mesh, morph_data = morph_data, hemi = 'lh');
  if(add_cbar_metadata) {
    cm_lh$metadata = list('makecmap_options' = mkco.seq(), 'src_data'=morph_data, 'fs_mesh'=cube_mesh);
  }

  cube_mesh_shifted = cube_mesh;
  cube_mesh_shifted$vertices = cube_mesh_shifted$vertices + 3L;
  cm_rh = coloredmesh.from.preloaded.data(cube_mesh_shifted, morph_data = morph_data, hemi = 'rh');
  if(add_cbar_metadata) {
    cm_rh$metadata = list('makecmap_options' = mkco.seq(), 'src_data'=morph_data, 'fs_mesh'=cube_mesh_shifted);
  }
  return(list('lh'=cm_lh, 'rh'=cm_rh));
}


#' @title Get coloredvoxels for unit tests.
get.demo.coloredvoxels <- function(n = 100L) {
  centers = matrix(rnorm(n*3)*100, ncol=3);
  return(rglvoxels(centers, voxelcol="red", do_show = FALSE));
}


#' @title Get 3D volume of integers in range 0-255 for unit tests. The volume has a background intensity and random cubes of other intensities.
#'
#' @param vd integer, dimension of the volume (will be used for all 3 axes).
#'
#' @param bg integer of NA, the value to use for the background
#'
#' @param num_centers integer, the number of clusters to spawn
#'
#' @return 3d array of integers, the volume
get.demo.volume <- function(vd = 30L, bg = NA, num_centers = 8L) {
    vdim = rep(vd, 3L);
    data = rep(bg, prod(vdim));
    vol = array(data, dim = vdim);
    for(i in 1:num_centers) {    # create small cubes within the volume
        csize = sample(3, size = 1);
        cvalue = sample(255, size = 1);
        center_xyz = sample((csize+1L):(vd-csize), size = 3);
        vol[(center_xyz[1]-csize):center_xyz[1], (center_xyz[1]-csize):center_xyz[1], (center_xyz[1]-csize):center_xyz[1]] = cvalue;
    }
    return(vol);
}

#' @title Close rgl windows after test.
close.all.rgl.windows <- function() {
  while (rgl::rgl.cur() > 0) {
    rgl::rgl.close();
  }
}


#' @title Check whether currently running R version is less than the given one.
rversion.less.than <- function(vmajor, vminor) {
  if(as.numeric(R.version$major) < vmajor) {
    return(TRUE);
  }
  if(as.numeric(R.version$major) == vmajor) {
    if(as.numeric(R.version$minor) < vminor) {
      return(TRUE);
    }
  }
  return(FALSE);
}

