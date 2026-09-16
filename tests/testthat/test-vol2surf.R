# Unit tests for the volume to surface projection (R/vol2surf.R).
#
# All tests here are offline: they build a tiny synthetic subject with a small
# MGZ volume and two surfaces, so no data download is required.
#
# The reference values below are the output of the reference implementation
# (yabplot's project_vol2surf: nibabel + scipy.ndimage.map_coordinates, order=1
# for trilinear and order=0 for nearest, mode='nearest'), evaluated on exactly
# the same volume array and vertex coordinates. The volume contains only integer
# values, which are exactly representable in float32 (MGZ storage) as well as in
# float64, so the data itself survives the file round trip losslessly.
#
# Note on precision: all fixture values (voxel data, affine matrix entries and
# vertex coordinates) are chosen to be exactly representable in float32, which
# is the storage type of MGZ volume headers and of surface vertex coordinates.
# The file round trips are therefore lossless, and all tests can compare
# strictly against the reference implementation.

context("volume to surface projection (vol2surf)")


# ══════════════════════════════════════════════════════════════════════════════
# Test fixture: a synthetic volume and subject
# ══════════════════════════════════════════════════════════════════════════════

#' Build the synthetic test volume (data array and affine matrix in memory).
#'
#' The volume is 8x8x8 with voxel values
#' `vol[i,j,k] = (i-1)^2 + 3*(k-1)^2 - 2*(j-1) + (i-1)*(j-1)`, using the 1-based
#' R indices i, j, k. All values are integers and thus survive the float32 MGZ
#' round trip exactly. The affine matrix is `diag(2, 2, 2)` with translation
#' (-8, -8, -8), so the volume covers world coordinates -8..6 on all axes.
#'
#' @return named list with entries 'data' (the volume) and 'affine' (the 4x4 matrix).
synthetic.vol2surf.volume <- function() {
    dims = c(8L, 8L, 8L);
    vol = array(0L, dim = dims);
    for(vi in seq_len(dims[1])) { for(vj in seq_len(dims[2])) { for(vk in seq_len(dims[3])) {
        vol[vi, vj, vk] = (vi - 1L)^2 + 3L * (vk - 1L)^2 - 2L * (vj - 1L) + (vi - 1L) * (vj - 1L);
    }}}
    affine = matrix(c(2.0, 0.0, 0.0, 0.0,
                      0.0, 2.0, 0.0, 0.0,
                      0.0, 0.0, 2.0, 0.0,
                      -8.0, -8.0, -8.0, 1.0), nrow = 4L);
    return(list("data" = vol, "affine" = affine));
}

#' Create a synthetic subject for vol2surf tests.
#'
#' Writes the volume returned by `synthetic.vol2surf.volume` to `mri/brain.mgz`
#' and creates 5-vertex `white` and `pial` surfaces for both hemispheres. The
#' vertices are (world coordinates, in mm):
#'
#'  1. (-3.0, -1.0, 0.5)                          inside
#'  2. ( 1.5,  2.5, -2.5)                         inside
#'  3. (-7.998046875, 5.998046875, -0.001953125)  inside, but only 1/1024 voxel
#'                                                away from the volume border
#'  4. ( 7.0,  7.0,  7.0)                         *outside* the volume (which
#'                                                ends at 6.0)
#'  5. ( 0.0,  0.0,  0.0)                         inside
#'
#' All coordinates are multiples of 1/1024, so they survive the float32 storage
#' of surface files exactly. The `rh` surfaces are shifted by 0.5 mm and the
#' `pial` surfaces by 1.0 mm with respect to `white`, so interpolating `white` ->
#' `pial` at fraction 0.5 gives exactly the vertex positions of the `rh` white
#' surface. Note that the `rh` vertices 3 and 4 are outside the volume (their y
#' coordinate is 6.498 and 7.5, while the volume ends at 6.0).
#'
#' The `?h.cortex.label` files contain vertices 1, 2 and 5, so vertices 3 and 4
#' are medial wall vertices.
#'
#' @return character string, the path of the synthetic subjects dir.
create.synthetic.vol2surf.subject <- function(subject_id = "subject1") {

    subjects_dir = tempfile("fsbrain_vol2surf_subjects_dir_");
    subject_dir = file.path(subjects_dir, subject_id);
    dir.create(file.path(subject_dir, "mri"), recursive = TRUE);
    dir.create(file.path(subject_dir, "surf"), recursive = TRUE);
    dir.create(file.path(subject_dir, "label"), recursive = TRUE);

    vol = synthetic.vol2surf.volume();
    freesurferformats::write.fs.mgh(file.path(subject_dir, "mri", "brain.mgz"), vol$data, vox2ras_matrix = vol$affine);

    lh_white_vertices = rbind(c(-3.0, -1.0, 0.5), c(1.5, 2.5, -2.5), c(-7.998046875, 5.998046875, -0.001953125), c(7.0, 7.0, 7.0), c(0.0, 0.0, 0.0));
    faces = rbind(c(1L, 2L, 3L), c(3L, 4L, 5L));

    for(hemi in c("lh", "rh")) {
        white_vertices = if(hemi == "lh") lh_white_vertices else lh_white_vertices + 0.5;
        freesurferformats::write.fs.surface(file.path(subject_dir, "surf", sprintf("%s.white", hemi)), white_vertices, faces);
        freesurferformats::write.fs.surface(file.path(subject_dir, "surf", sprintf("%s.pial", hemi)), white_vertices + 1.0, faces);
        freesurferformats::write.fs.label(file.path(subject_dir, "label", sprintf("%s.cortex.label", hemi)),
            vertex_indices = c(1L, 2L, 5L), vertex_coords = white_vertices[c(1L, 2L, 5L), ]);
    }

    return(subjects_dir);
}

#' Set the given entries of a reference vector to NA, for vertices outside the volume.
with.na <- function(values, indices) {
    values[indices] = NA_real_;
    return(values);
}

# Reference values, see the file comment for their origin.
lh.white.trilinear.reference   = c(63.0, 60.4375, 33.989256858825684, 231.0, 72.0);
lh.white.nearest.reference     = c(61.0, 67.0, 34.0, 231.0, 72.0);
rh.white.trilinear.reference   = c(72.0625, 68.5, 42.7314453125, 231.0, 82.5625);
lh.frac0.5.trilinear.reference = rh.white.trilinear.reference;   # lh white + 0.5 mm == rh white

lh.outside.indices = 4L;                                         # outside the volume for the 'lh' white surface
rh.outside.indices = c(3L, 4L);                                  # outside the volume for 'rh' white and for the frac-0.5 surface

# All fixture values survive the float32 file round trips exactly, so the comparison against the
# reference implementation can be strict (the two implementations differ only in summation order).
vol2surf.tolerance = 1e-12;


# ══════════════════════════════════════════════════════════════════════════════
# Tests: subject.vol2surf
# ══════════════════════════════════════════════════════════════════════════════

test_that("subject.vol2surf samples the volume at the surface vertices (trilinear)", {
    subjects_dir = create.synthetic.vol2surf.subject();

    morph_data = expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = "brain", surface = "white", hemi = "lh"));
    expect_equal(length(morph_data), 5L);
    expect_equal(morph_data, with.na(lh.white.trilinear.reference, lh.outside.indices), tolerance = vol2surf.tolerance);

    # The 'linear' alias for 'trilinear' must give the same result.
    morph_data2 = expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = "brain", surface = "white", hemi = "lh", interpolation = "linear"));
    expect_equal(morph_data2, morph_data, tolerance = 1e-12);
})


test_that("subject.vol2surf matches the reference implementation exactly for an in-memory volume", {
    subjects_dir = create.synthetic.vol2surf.subject();
    vol = synthetic.vol2surf.volume();

    morph_data = expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = vol, surface = "white", hemi = "lh"));
    expect_equal(morph_data, with.na(lh.white.trilinear.reference, lh.outside.indices), tolerance = 1e-12);

    morph_data_rh = expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = vol, surface = "white", hemi = "rh"));
    expect_equal(morph_data_rh, with.na(rh.white.trilinear.reference, rh.outside.indices), tolerance = 1e-12);
})


test_that("subject.vol2surf supports nearest neighbor interpolation", {
    subjects_dir = create.synthetic.vol2surf.subject();

    morph_data = expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = "brain", surface = "white", hemi = "lh", interpolation = "nearest"));
    expect_equal(morph_data, with.na(lh.white.nearest.reference, lh.outside.indices), tolerance = vol2surf.tolerance);
})


test_that("subject.vol2surf warns about vertices outside the volume and sets them to NA", {
    subjects_dir = create.synthetic.vol2surf.subject();

    expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = "brain", surface = "white", hemi = "lh"),
        "1 of 5 'lh' vertices");
    expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = "brain", surface = "white", hemi = "rh"),
        "2 of 5 'rh' vertices");
})


test_that("subject.vol2surf can clamp vertices outside the volume", {
    subjects_dir = create.synthetic.vol2surf.subject();

    morph_data = expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = "brain", surface = "white", hemi = "lh", clamp = TRUE));
    expect_false(any(is.na(morph_data)));
    # With clamping, the out-of-volume vertex gets the value of the closest border voxel, which is what
    # the reference implementation reports for all vertices (scipy uses mode='nearest').
    expect_equal(morph_data, lh.white.trilinear.reference, tolerance = vol2surf.tolerance);
})


test_that("subject.vol2surf does not warn if check_fov is FALSE", {
    subjects_dir = create.synthetic.vol2surf.subject();

    morph_data = expect_silent(subject.vol2surf(subjects_dir, "subject1", volume = "brain", surface = "white", hemi = "lh", check_fov = FALSE));
    expect_true(is.na(morph_data[lh.outside.indices]));
})


test_that("subject.vol2surf returns a hemilist for both hemispheres", {
    subjects_dir = create.synthetic.vol2surf.subject();

    morph_data = expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = "brain", surface = "white", hemi = "both"));
    expect_true(is.hemilist(morph_data));
    expect_equal(names(morph_data), c("lh", "rh"));
    expect_equal(morph_data$lh, with.na(lh.white.trilinear.reference, lh.outside.indices), tolerance = vol2surf.tolerance);
    expect_equal(morph_data$rh, with.na(rh.white.trilinear.reference, rh.outside.indices), tolerance = vol2surf.tolerance);

    # A single hemisphere is returned as a plain vector, not as a hemilist.
    morph_data_lh = expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = "brain", surface = "white", hemi = "lh"));
    expect_false(is.hemilist(morph_data_lh));
})


test_that("subject.vol2surf can sample an interpolated surface (surface_frac)", {
    subjects_dir = create.synthetic.vol2surf.subject();
    vol = synthetic.vol2surf.volume();

    # lh white + 0.5 mm == rh white in the fixture, so the reference values are shared.
    morph_data = expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = vol, surface = "white",
        frac_surface = "pial", surface_frac = 0.5, hemi = "lh"));
    expect_equal(morph_data, with.na(lh.frac0.5.trilinear.reference, rh.outside.indices), tolerance = 1e-12);

    # fraction 0 must be identical to sampling the surface itself.
    morph_data_frac0 = expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = vol, surface = "white",
        frac_surface = "pial", surface_frac = 0.0, hemi = "lh"));
    morph_data_white = expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = vol, surface = "white", hemi = "lh"));
    expect_equal(morph_data_frac0, morph_data_white, tolerance = 1e-12);
})


test_that("subject.vol2surf can mask the medial wall (cortex_only)", {
    subjects_dir = create.synthetic.vol2surf.subject();

    morph_data = expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = "brain", surface = "white", hemi = "lh", cortex_only = TRUE));
    # Vertices 3 and 4 are not part of the cortex label in the fixture. Vertex 3 is inside the volume
    # and gets masked, vertex 4 is outside the volume and is NA anyway.
    expect_true(is.na(morph_data[3L]));
    expect_true(is.na(morph_data[4L]));
    expect_false(is.na(morph_data[1L]));
    expect_false(is.na(morph_data[5L]));
})


test_that("subject.vol2surf can select a frame of a 4D volume", {
    subjects_dir = create.synthetic.vol2surf.subject();

    vol = synthetic.vol2surf.volume();
    # Write a 2-frame volume. Frame 2 is frame 1 plus a constant: since trilinear
    # interpolation is linear in the data, all interpolated values increase by
    # exactly that constant.
    vol4d = array(0.0, dim = c(dim(vol$data), 2L));
    vol4d[, , , 1L] = vol$data;
    vol4d[, , , 2L] = vol$data + 1000.0;
    freesurferformats::write.fs.mgh(file.path(subjects_dir, "subject1", "mri", "twoframes.mgz"), vol4d, vox2ras_matrix = vol$affine);

    morph_frame1 = expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = "twoframes", surface = "white", hemi = "lh", frame = 1L));
    morph_frame2 = expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = "twoframes", surface = "white", hemi = "lh", frame = 2L));
    inside = setdiff(seq_len(5L), lh.outside.indices);
    expect_equal(morph_frame1[inside], lh.white.trilinear.reference[inside], tolerance = vol2surf.tolerance);
    expect_equal(morph_frame2[inside], lh.white.trilinear.reference[inside] + 1000.0, tolerance = vol2surf.tolerance);
})


test_that("subject.vol2surf reports errors for invalid parameters", {
    subjects_dir = create.synthetic.vol2surf.subject();

    expect_error(subject.vol2surf(subjects_dir, "subject1", volume = "brain", hemi = "up"), "hemi");
    expect_error(subject.vol2surf(subjects_dir, "subject1", volume = "brain", interpolation = "cubic"), "interpolation");
    expect_error(subject.vol2surf(subjects_dir, "subject1", volume = "brain", surface_frac = 1.5), "surface_frac");
    expect_error(subject.vol2surf(subjects_dir, "subject1", volume = "nosuchvolume"), "Cannot find the volume file");
    expect_error(subject.vol2surf(subjects_dir, "subject1", volume = "brain", frame = 2L), "frame");
    expect_error(subject.vol2surf(subjects_dir, "subject1", volume = "brain", surface = "nosuchsurface"), "nosuchsurface");
    expect_error(subject.vol2surf(subjects_dir, "subject1", volume = list(data = NULL, affine = NULL)), "must be a named list");
})


# ══════════════════════════════════════════════════════════════════════════════
# Tests: NIfTI volumes
# ══════════════════════════════════════════════════════════════════════════════

test_that("subject.vol2surf can read NIfTI volumes", {
    testthat::skip_if_not_installed("oro.nifti");
    subjects_dir = create.synthetic.vol2surf.subject();
    vol = synthetic.vol2surf.volume();

    # Write the volume as a NIfTI1 file. Note that the sform must be set, else the
    # coordinate space of the file is undefined and the projection is not possible.
    nim = oro.nifti::nifti(img = vol$data, dim = c(8L, 8L, 8L, 1L), datatype = 16L, bitpix = 32L,
        pixdim = c(1, 2, 2, 2, 0, 0, 0, 0), srow_x = vol$affine[1, ], srow_y = vol$affine[2, ],
        srow_z = vol$affine[3, ], sform_code = 1L, qform_code = 0L);
    nii_file = oro.nifti::writeNIfTI(nim, filename = file.path(subjects_dir, "niftivol"));

    morph_nifti = expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = nii_file, surface = "white", hemi = "lh"));
    morph_memory = expect_warning(subject.vol2surf(subjects_dir, "subject1", volume = vol, surface = "white", hemi = "lh"));
    expect_equal(morph_nifti, morph_memory, tolerance = vol2surf.tolerance);
    expect_equal(morph_nifti, with.na(lh.white.trilinear.reference, lh.outside.indices), tolerance = vol2surf.tolerance);

    # A NIfTI file without a valid sform/qform (unknown coordinate space) must be reported clearly.
    nim_nospace = oro.nifti::nifti(img = vol$data, dim = c(8L, 8L, 8L, 1L), datatype = 16L, bitpix = 32L,
        pixdim = c(1, 2, 2, 2, 0, 0, 0, 0), sform_code = 0L, qform_code = 0L);
    nii_nospace = oro.nifti::writeNIfTI(nim_nospace, filename = file.path(subjects_dir, "nospacetest"));
    expect_error(suppressWarnings(subject.vol2surf(subjects_dir, "subject1", volume = nii_nospace, surface = "white", hemi = "lh")),
        "does not contain a valid sform or qform");
})


# ══════════════════════════════════════════════════════════════════════════════
# Tests: the vox2ras conventions (tkregister vs header)
# ══════════════════════════════════════════════════════════════════════════════

test_that("vol.tkreg.affine() converts a header matrix into the tkregister convention", {
    # A 'conformed' volume whose header carries a non-zero center-of-RAS offset, as produced
    # by FreeSurfer for data imported from DICOM (verified with 'mri_info --cras').
    header_affine = vox2ras_tkr();
    header_affine[1:3, 4] = header_affine[1:3, 4] + c(-0.5, 29.3727, -48.9047);

    tkreg = fsbrain:::vol.tkreg.affine(header_affine, c(256L, 256L, 256L));
    expect_equal(tkreg, vox2ras_tkr(), tolerance = 1e-12);

    # For an arbitrary affine, only the translation may change, and the volume center must be
    # mapped to the origin of the coordinate system.
    arbitrary = header_affine;
    arbitrary[1:3, 1:3] = matrix(c(0, 1, 0, -1, 0, 0, 0, 0, 2), nrow = 3L);
    tkreg2 = fsbrain:::vol.tkreg.affine(arbitrary, c(100L, 200L, 300L));
    expect_equal(tkreg2[1:3, 1:3], arbitrary[1:3, 1:3]);
    expect_equal(as.numeric(tkreg2 %*% c(50, 100, 150, 1))[1:3], c(0, 0, 0), tolerance = 1e-12);
    expect_error(fsbrain:::vol.tkreg.affine(diag(3), c(10L, 10L, 10L)), "4x4");
})


test_that("the default vox2ras policy uses the tkregister convention for MGZ volumes", {
    subjects_dir = create.synthetic.vol2surf.subject();
    vol = synthetic.vol2surf.volume();

    # Write the volume with a header that has a non-zero center-of-RAS offset.
    shifted_affine = vol$affine;
    shifted_affine[1:3, 4] = shifted_affine[1:3, 4] + c(3.0, -2.0, 4.0);
    shifted_file = file.path(subjects_dir, "subject1", "mri", "shifted.mgz");
    freesurferformats::write.fs.mgh(shifted_file, vol$data, vox2ras_matrix = shifted_affine);

    # The offset must survive the file round trip, else this test would not test anything.
    header_affine = freesurferformats::read.fs.mgh(shifted_file, with_header = TRUE, drop_empty_dims = TRUE)$header$vox2ras_matrix;
    expect_equal(header_affine, shifted_affine, tolerance = 1e-6);
    expect_true(max(abs(header_affine - vol$affine)) > 1.0);
    tkreg_affine = fsbrain:::vol.tkreg.affine(header_affine, dim(vol$data));
    expect_true(max(abs(tkreg_affine - header_affine)) > 1.0);

    project = function(...) {
        return(suppressWarnings(subject.vol2surf(subjects_dir, "subject1", volume = "shifted",
            surface = "white", hemi = "lh", check_fov = FALSE, ...)));
    }
    morph_auto = project();
    morph_tkr = project(vox2ras = "tkr");
    morph_header = project(vox2ras = "header");

    # 'auto' must use the tkregister convention for MGZ volumes, and that convention must
    # actually be applied (the tkreg and the header matrix differ here).
    expect_equal(morph_auto, morph_tkr, tolerance = vol2surf.tolerance);
    expect_false(isTRUE(all.equal(morph_auto, morph_header, tolerance = 1e-6)));

    # The values must be exactly the ones obtained by sampling with the tkreg matrix directly.
    srf = freesurferformats::read.fs.surface(file.path(subjects_dir, "subject1", "surf", "lh.white"));
    expected = fsbrain:::vol.sample.at.coords(vol$data, tkreg_affine, srf$vertices, interpolation = "trilinear")$values;
    expect_equal(morph_auto, expected, tolerance = vol2surf.tolerance);
})


test_that("the default vox2ras policy uses the header for NIfTI volumes", {
    testthat::skip_if_not_installed("oro.nifti");
    subjects_dir = create.synthetic.vol2surf.subject();
    vol = synthetic.vol2surf.volume();

    # A NIfTI file carries its own world space, so the sform must be used as-is (there is no
    # tkregister convention for NIfTI files).
    shifted_affine = vol$affine;
    shifted_affine[1:3, 4] = shifted_affine[1:3, 4] + c(3.0, -2.0, 4.0);
    nim = oro.nifti::nifti(img = vol$data, dim = c(8L, 8L, 8L, 1L), datatype = 16L, bitpix = 32L,
        pixdim = c(1, 2, 2, 2, 0, 0, 0, 0), srow_x = shifted_affine[1, ], srow_y = shifted_affine[2, ],
        srow_z = shifted_affine[3, ], sform_code = 1L, qform_code = 0L);
    nii_file = oro.nifti::writeNIfTI(nim, filename = file.path(subjects_dir, "shiftednifti"));

    project = function(...) {
        return(suppressWarnings(subject.vol2surf(subjects_dir, "subject1", volume = nii_file,
            surface = "white", hemi = "lh", check_fov = FALSE, ...)));
    }
    morph_auto = project();
    expect_equal(morph_auto, project(vox2ras = "header"), tolerance = vol2surf.tolerance);
    expect_false(isTRUE(all.equal(morph_auto, project(vox2ras = "tkr"), tolerance = 1e-6)));

    expect_error(project(vox2ras = "invalid"), "vox2ras");
})


# ══════════════════════════════════════════════════════════════════════════════
# Tests: template.vol2surf
# ══════════════════════════════════════════════════════════════════════════════

test_that("template.vol2surf gives the same results as subject.vol2surf", {
    subjects_dir = create.synthetic.vol2surf.subject(subject_id = "mytemplate");
    vol = synthetic.vol2surf.volume();

    morph_template = expect_warning(template.vol2surf(vol, template = "mytemplate", surface = "white", hemi = "lh", subjects_dir = subjects_dir));
    morph_subject = expect_warning(subject.vol2surf(subjects_dir, "mytemplate", volume = vol, surface = "white", hemi = "lh"));
    expect_equal(morph_template, morph_subject, tolerance = 1e-12);
    expect_equal(morph_template, with.na(lh.white.trilinear.reference, lh.outside.indices), tolerance = 1e-12);
})


test_that("template.vol2surf supports cortex_only", {
    subjects_dir = create.synthetic.vol2surf.subject(subject_id = "mytemplate");
    vol = synthetic.vol2surf.volume();

    morph_data = expect_warning(template.vol2surf(vol, template = "mytemplate", surface = "white", hemi = "lh",
        subjects_dir = subjects_dir, cortex_only = TRUE));
    expect_true(is.na(morph_data[3L]));
    expect_false(is.na(morph_data[1L]));
})


test_that("template.vol2surf reports a useful error if the template is missing", {
    subjects_dir = tempfile("fsbrain_vol2surf_empty_");
    dir.create(subjects_dir, recursive = TRUE);
    expect_error(template.vol2surf(synthetic.vol2surf.volume(), template = "mytemplate", subjects_dir = subjects_dir),
        "does not contain the template subject");
})


# ══════════════════════════════════════════════════════════════════════════════
# Tests: shipped data manifests
# ══════════════════════════════════════════════════════════════════════════════

test_that("the fs_LR_32 label manifest is shipped and valid", {
    manifest_file = system.file("extdata", "pkgfilecache_manifest_fs_LR_32_labels.csv", package = "fsbrain", mustWork = TRUE);
    manifest = pkgfilecache::read_manifest(manifest_file);
    expect_equal(nrow(manifest), 2L);
    expect_true(all(grepl("cortex\\.label$", manifest$path)));
    expect_true(all(grepl("fs_LR_32/label/", manifest$path)));
    expect_true(all(nchar(manifest$url) > 0L));
    expect_true(all(nchar(manifest$md5) == 32L));
})
