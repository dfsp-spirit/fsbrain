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
  # In scimesh test mode no X11 display is required (the scimesh renderer is
  # headless), so the existing X11 guards pass automatically in that mode.
  if(fsbrain.tests.use.scimesh()) {
    return(TRUE);
  }
  # I'm not really sure whether this is a great way to check for an X11 display.
  # UPDATE: This seems to work under MacOS with XQuartz, but not Ubuntu Linux with their Xorg.
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


#' @title Get a coloredmesh of a cube with 6 distinct single-colored faces.
#'
#' @description Builds a cube whose 6 faces each carry a distinct, uniform
#'   colour (+z red, -z green, -x blue, +x yellow, +y cyan, -y magenta).
#'   Vertices are duplicated per face (24 vertices / 12 triangles) so each face
#'   can have its own colour. Winding is counter-clockwise seen from OUTSIDE
#'   (outward normal = cross(v1-v0, v2-v0)), so both the rgl and scimesh
#'   backends agree on back-face culling and an outside view shows exactly the
#'   faces whose normal points toward the camera. This makes a view's
#'   orientation immediately readable (which face is shown) and lets you verify
#'   backend parity programmatically.
#'
#' @return a list with entries: \code{coloredmesh} (an fs.coloredmesh),
#'   \code{face_colors} (named vector, face axis -> hex colour), and
#'   \code{face_normals} (6x3 matrix, outward unit normal per face).
get.demo.facecolored.cube <- function() {
  h = 0.5;
  faces = list(
    "+z" = rbind(c(-h, -h,  h), c( h, -h,  h), c( h,  h,  h), c(-h,  h,  h)),
    "-z" = rbind(c(-h, -h, -h), c(-h,  h, -h), c( h,  h, -h), c( h, -h, -h)),
    "-x" = rbind(c(-h, -h, -h), c(-h, -h,  h), c(-h,  h,  h), c(-h,  h, -h)),
    "+x" = rbind(c( h, -h, -h), c( h,  h, -h), c( h,  h,  h), c( h, -h,  h)),
    "+y" = rbind(c(-h,  h, -h), c(-h,  h,  h), c( h,  h,  h), c( h,  h, -h)),
    "-y" = rbind(c(-h, -h, -h), c( h, -h, -h), c( h, -h,  h), c(-h, -h,  h))
  );
  face_colors = c("+z"="#FF0000", "-z"="#00FF00", "-x"="#0000FF", "+x"="#FFFF00", "+y"="#00FFFF", "-y"="#FF00FF");
  face_normals = rbind("+z"=c(0,0,1), "-z"=c(0,0,-1), "-x"=c(-1,0,0), "+x"=c(1,0,0), "+y"=c(0,1,0), "-y"=c(0,-1,0));
  V = do.call(rbind, faces);
  it = do.call(rbind, lapply(seq_along(faces), function(f) { off = (f-1L)*4L; rbind(c(off+1L, off+2L, off+3L), c(off+1L, off+3L, off+4L)); }));
  col = rep(face_colors, each = 4L);
  tmesh = rgl::tmesh3d(t(cbind(V, 1)), it);
  cm = structure(list(mesh = tmesh, col = col, render = TRUE), class = "fs.coloredmesh");
  return(list("coloredmesh" = cm, "face_colors" = face_colors, "face_normals" = face_normals));
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
  while (rgl::cur3d() > 0) {
    rgl::close3d();
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


# --- scimesh renderer backend test mode ---------------------------------------

#' @title Check whether the test suite should use the scimesh backend.
#'
#' @description Reads the environment variable 'FSBRAIN_TESTS_USE_SCIMESH'. If
#'   set to a truthy value ('1', 'true', 'yes', 'on'), the tests switch the
#'   renderer backend to scimesh at load time, so static image export goes
#'   through the headless software renderer instead of rgl.
#'
#' @return logical, whether the scimesh backend should be used.
fsbrain.tests.use.scimesh <- function() {
  return(tolower(Sys.getenv("FSBRAIN_TESTS_USE_SCIMESH", unset = "false")) %in% c("1", "true", "yes", "on"));
}


# Apply the backend switch once, when the helpers are sourced.
if(fsbrain.tests.use.scimesh()) {
  options(fsbrain.renderer_backend = "scimesh");
}


#' @title Skip the current test if it requires the interactive rgl backend.
#'
#' @description In scimesh test mode, tests that open interactive rgl windows
#'   (brainview.si/sr/sd/t4/t9, rglwidget, rotating views, or direct rgl calls)
#'   cannot run and are skipped. Tests that only produce static images, or do
#'   not render at all, are unaffected.
#'
#' @return invisible NULL; skips the test when the scimesh backend is active.
skip_if_rgl_required <- function() {
  testthat::skip_if(fsbrain.tests.use.scimesh(), "This test requires the interactive rgl backend.");
}


#' @title Skip the current test if it requires an interactive rgl window.
#'
#' @description Some tests need a working interactive rgl window (a real
#'   X11/OpenGL display) to open a scene and/or take a screenshot of it, e.g.
#'   via \code{take.screenshot()} or the 'snapshot_png'/'movie' rglactions. On
#'   recent macOS versions (Tahoe 26.x, Sonoma 14.x) the X11/OpenGL stack
#'   (XQuartz) is broken, so no window can be opened and screenshots cannot be
#'   produced (this surfaces as spurious CI failures like 'Postscript
#'   conversion failed' / 'Failed to convert PDF to PNG'). Such tests are
#'   therefore skipped on macOS. See README_HEADLESS.md for details.
#'
#' This complements \code{\link{skip_if_rgl_required}}, which handles the
#' scimesh backend; a test that needs a window should call both.
#'
#' @return invisible NULL; skips the test on macOS.
skip_if_rgl_window_required <- function() {
  if(tolower(Sys.info()[["sysname"]]) == 'darwin') {
    testthat::skip("This test requires an rgl window, which is unavailable on macOS (broken X11/OpenGL stack, see README_HEADLESS.md).");
  }
  invisible(NULL);
}


#' @title Render demo coloredmeshes to a PNG (backend-aware) for smoke testing.
#'
#' @description Renders the given coloredmeshes from the two medial views
#'   (angle set 't2') to a PNG in the R session temporary directory, using the
#'   current renderer backend (rgl or scimesh). The output file is named
#'   '<name>_<backend>.png' (e.g. 'demo_rgl.png' or 'demo_scimesh.png'), so
#'   running the suite once per backend lets you compare the images directly.
#'   The full path is printed.
#'
#' @param coloredmeshes a list of fs.coloredmesh, as returned by the vis.*
#'   functions with \code{views=NULL}.
#'
#' @param name character string, base name for the output image file.
#'
#' @param ... extra arguments passed to
#'   \code{\link[fsbrain]{vislayout.from.coloredmeshes}}.
#'
#' @return invisible character string, the path to the output image.
render.demo <- function(coloredmeshes, name = "demo", ...) {
  output_img <- file.path(tempdir(), sprintf("%s_%s.png", name, get.fsbrain.renderer.backend()));
  vislayout.from.coloredmeshes(coloredmeshes, view_angles = get.view.angle.names(angle_set = "t2"), output_img = output_img, silent = TRUE, ...);
  testthat::expect_true(file.exists(output_img), info = sprintf("Expected rendered image '%s' to exist.", output_img));
  message(sprintf("Wrote demo image to '%s'.", output_img));
  return(invisible(output_img));
}

