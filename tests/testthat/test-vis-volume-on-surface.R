# Unit tests for vis.volume.on.surface — combined surface + volume 3D rendering.
#
# These tests verify that a brain volume can be rendered as a contour or voxels
# together with a cortical surface in the same rgl scene.

context("vis.volume.on.surface — combined surface + volume 3D rendering")

# ═══════════════════════════════════════════════════════════════════════════════
# 1. Contour mode: brain isosurface over thickness-colored surface
# ═══════════════════════════════════════════════════════════════════════════════
test_that("Brain volume contour can be overlaid on a thickness-colored surface", {
    testthat::skip_on_cran();
    skip_if(tests_running_on_cran_under_macos(),
        message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.has.x11display(),
        message = "This test requires an X11 display.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message = "Test data missing.");

    subject_id = "subject1";

    result = vis.volume.on.surface(
        subjects_dir = subjects_dir,
        subject_id = subject_id,
        volume = "brain",
        volume_mode = "contour",
        volume_level = 60,
        volume_color = "#888888",
        volume_alpha = 0.25,
        measure = "thickness",
        hemi = "lh",
        views = c("si"),
        surface_style = "semitransparent"
    );

    # Check return structure
    expect_true(is.list(result));
    expect_equal(names(result), c("surface", "volume"));
    expect_true(is.hemilist(result$surface));

    # Take a snapshot for visual inspection
    snapshot_path = file.path(tempdir(), "fsbrain_volume_on_surface_contour.png");
    rgl::rgl.snapshot(snapshot_path);
    expect_true(file.exists(snapshot_path));
    message(sprintf("Contour overlay snapshot written to: %s", snapshot_path));

    rgl::close3d();
})


# ═══════════════════════════════════════════════════════════════════════════════
# 2. Contour mode: both hemispheres
# ═══════════════════════════════════════════════════════════════════════════════
test_that("Brain volume contour can be overlaid on both hemispheres", {
    testthat::skip_on_cran();
    skip_if(tests_running_on_cran_under_macos(),
        message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.has.x11display(),
        message = "This test requires an X11 display.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message = "Test data missing.");

    subject_id = "subject1";

    result = vis.volume.on.surface(
        subjects_dir = subjects_dir,
        subject_id = subject_id,
        volume = "brain",
        volume_mode = "contour",
        volume_level = 60,
        volume_color = "#888888",
        volume_alpha = 0.25,
        measure = "thickness",
        hemi = "both",
        views = c("si"),
        surface_style = "semitransparent"
    );

    expect_true(is.list(result));
    expect_equal(names(result), c("surface", "volume"));
    expect_true(is.hemilist(result$surface));
    expect_true(!is.null(result$surface$lh));
    expect_true(!is.null(result$surface$rh));

    rgl::close3d();
})


# ═══════════════════════════════════════════════════════════════════════════════
# 3. Contour mode: sulc morphometry on pial surface
# ═══════════════════════════════════════════════════════════════════════════════
test_that("Brain volume contour over sulc-colored pial surface", {
    testthat::skip_on_cran();
    skip_if(tests_running_on_cran_under_macos(),
        message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.has.x11display(),
        message = "This test requires an X11 display.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message = "Test data missing.");

    subject_id = "subject1";

    result = vis.volume.on.surface(
        subjects_dir = subjects_dir,
        subject_id = subject_id,
        volume = "brain",
        volume_mode = "contour",
        volume_level = 60,
        volume_color = "#AAAAAA",
        volume_alpha = 0.2,
        measure = "sulc",
        surface = "pial",
        hemi = "lh",
        views = c("si"),
        surface_style = "semitransparent",
        makecmap_options = mkco.seq()
    );

    expect_true(is.list(result));
    rgl::close3d();
})


# ═══════════════════════════════════════════════════════════════════════════════
# 4. Voxel mode: brain voxels over surface
# ═══════════════════════════════════════════════════════════════════════════════
test_that("Brain volume voxels can be overlaid on a surface", {
    testthat::skip_on_cran();
    skip_if(tests_running_on_cran_under_macos(),
        message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.has.x11display(),
        message = "This test requires an X11 display.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message = "Test data missing.");

    subject_id = "subject1";

    vol = subject.volume(subjects_dir, subject_id, "brain");
    vol[vol < 90] = NA;         # mark low-intensity as background
    vol = vol.hull(vol, thickness = 2L, axes = c(1L, 2L, 3L));

    result = vis.volume.on.surface(
        subjects_dir = subjects_dir,
        subject_id = subject_id,
        volume = vol,
        volume_mode = "voxels",
        volume_color = "red",
        render_every = 30,
        measure = NULL,
        hemi = "lh",
        views = c("si"),
        surface_style = "default"
    );

    expect_true(is.list(result));

    # Take a snapshot for visual inspection
    snapshot_path = file.path(tempdir(), "fsbrain_volume_on_surface_voxels.png");
    rgl::rgl.snapshot(snapshot_path);
    expect_true(file.exists(snapshot_path));
    message(sprintf("Voxel overlay snapshot written to: %s", snapshot_path));

    rgl::close3d();
})


# ═══════════════════════════════════════════════════════════════════════════════
# 5. Error handling: invalid volume_mode
# ═══════════════════════════════════════════════════════════════════════════════
test_that("Invalid volume_mode is rejected", {
    expect_error(
        vis.volume.on.surface("/tmp", "dummy", volume_mode = "invalid"),
        "Parameter 'volume_mode' must be one of 'contour' or 'voxels'."
    );
})


# ═══════════════════════════════════════════════════════════════════════════════
# 6. Error handling: non-3D volume
# ═══════════════════════════════════════════════════════════════════════════════
test_that("Non-3D volume input is rejected", {
    expect_error(
        vis.volume.on.surface("/tmp", "dummy", volume = matrix(1:4, 2, 2)),
        "Parameter 'volume' must be a 3D numeric array or the name of a volume file"
    );
})


# ═══════════════════════════════════════════════════════════════════════════════
# 7. Tiled view: t4 layout with volume contour and both hemispheres
# ═══════════════════════════════════════════════════════════════════════════════
test_that("Volume contour overlaid on both hemispheres in t4 layout", {
    testthat::skip_on_cran();
    skip_if(tests_running_on_cran_under_macos(),
        message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.has.x11display(),
        message = "This test requires an X11 display.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message = "Test data missing.");

    subject_id = "subject1";

    result = vis.volume.on.surface(
        subjects_dir = subjects_dir,
        subject_id = subject_id,
        volume = "brain",
        volume_mode = "contour",
        volume_level = 60,
        volume_color = "#888888",
        volume_alpha = 0.25,
        measure = "thickness",
        hemi = "both",
        views = c("t4"),
        surface_style = "semitransparent"
    );

    expect_true(is.list(result));
    rgl::close3d();
})


# ═══════════════════════════════════════════════════════════════════════════════
# 8. Generate a publication-quality QA image
# ═══════════════════════════════════════════════════════════════════════════════
test_that("A high-quality QA image can be produced: surface + volume contour", {
    testthat::skip_on_cran();
    skip_if(tests_running_on_cran_under_macos(),
        message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
    skip_if_not(box.has.x11display(),
        message = "This test requires an X11 display.");

    fsbrain::download_optional_data();
    subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
    skip_if_not(dir.exists(subjects_dir), message = "Test data missing.");

    subject_id = "subject1";

    # Use a larger window for a higher-resolution result
    rgloptions = list("windowRect" = c(50, 50, 1200, 1200));

    vis.volume.on.surface(
        subjects_dir = subjects_dir,
        subject_id = subject_id,
        volume = "brain",
        volume_mode = "contour",
        volume_level = 60,
        volume_color = "#888888",
        volume_alpha = 0.25,
        measure = "thickness",
        hemi = "both",
        views = c("si"),
        surface_style = "semitransparent",
        rgloptions = rgloptions
    );

    # High-quality snapshot
    snapshot_path = file.path(tempdir(), "fsbrain_qa_volume_surface.png");
    rgl::rgl.snapshot(snapshot_path);
    expect_true(file.exists(snapshot_path));

    file_size = file.info(snapshot_path)$size;
    message(sprintf("QA snapshot written to: %s (%d bytes)", snapshot_path, file_size));
    expect_true(file_size > 10000);  # should be a real image, not empty

    rgl::close3d();
})
