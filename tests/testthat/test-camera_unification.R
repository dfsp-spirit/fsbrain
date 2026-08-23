# Regression tests for the camera/view unification work tracked in
# dev_tools/TODO_FSBRAIN_RGL_CAM.md (Step 2: migrate the rgl path from
# mesh-rotation to camera/view-transform, reusing the same plain-R view->camera
# math as the scimesh bridge).
#
# These tests are HEADLESS (rgl useNULL): they must pass on CI without X11 and
# in scimesh test mode. They pin the current framing behaviour so the Step 2
# implementation cannot silently change rendered output.


# ---------------------------------------------------------------------------
# View spec table: the per-view orientation used today by brainview.t4/t9/sd.
# Each entry is a mesh rotation (rotation.matrix) composed with a fixed camera
# view3d(theta, phi=0, fov=0). This is the exact recipe the current rgl path
# applies (see R/vis_multiview.R).
# ---------------------------------------------------------------------------
current_view_spec <- list(
    "lateral_lh" = list(axis = c(pi / 2, 1, 0, 0), theta = -90),
    "lateral_rh" = list(axis = c(pi / 2, 1, 0, 0), theta = 90),
    "medial_lh"  = list(axis = c(pi / 2, 1, 0, 0), theta = 90),
    "medial_rh"  = list(axis = c(pi / 2, 1, 0, 0), theta = -90),
    "dorsal"     = list(axis = c(0, 1, 0, 0), theta = 0),
    "ventral"    = list(axis = c(pi, 1, 0, 0), theta = 0),
    "rostral"    = list(axis = c(pi / 2, 1, 0, 0), theta = 180),  # anterior (+y), corrected 2026-08-23
    "caudal"     = list(axis = c(pi / 2, 1, 0, 0), theta = 0)     # posterior (-y), corrected 2026-08-23
)


#' @title Project 3D vertices to 2D window coords (headless rgl).
#'
#' @param vertices Nx3 numeric matrix of vertex coordinates.
#'
#' @param user_matrix 4x4 numeric matrix to use as par3d("userMatrix").
#'
#' @return Nx3 numeric matrix of window coordinates (x,y in 0..1).
#'
#' @keywords internal
project_vertices_headless <- function(vertices, user_matrix) {
    rgl::open3d(useNULL = TRUE)
    on.exit(rgl::close3d(), add = TRUE)
    rgl::points3d(vertices)
    rgl::view3d(userMatrix = user_matrix, fov = 0, interactive = FALSE)
    proj <- rgl::rgl.projection()
    return(rgl::rgl.user2window(vertices, projection = proj))
}


#' @title Get the rgl userMatrix that view3d(theta, phi) would set.
#'
#' @param theta numeric, the view3d theta angle.
#'
#' @param phi numeric, the view3d phi angle (default 0).
#'
#' @return 4x4 numeric matrix.
#'
#' @keywords internal
view3d_user_matrix <- function(theta, phi = 0) {
    rgl::open3d(useNULL = TRUE)
    on.exit(rgl::close3d(), add = TRUE)
    rgl::view3d(theta, phi, fov = 0, interactive = FALSE)
    return(rgl::par3d("userMatrix"))
}


#' @title Cross product of two 3D vectors.
#'
#' @param a, b numeric vectors of length 3.
#'
#' @return numeric vector of length 3.
#'
#' @keywords internal
cross_product <- function(a, b) {
    return(c(a[2] * b[3] - a[3] * b[2],
             a[3] * b[1] - a[1] * b[3],
             a[1] * b[2] - a[2] * b[1]))
}


# ---------------------------------------------------------------------------
# 1. bounding_sphere helper (shared framing math).
# ---------------------------------------------------------------------------

test_that("bounding_sphere computes the rgl AABB sphere (radius = half AABB diagonal).", {
    # Axis-aligned cube [-1,1]^3: radius = 0.5 * sqrt(2^2+2^2+2^2) = sqrt(3).
    cube <- as.matrix(expand.grid(c(-1, 1), c(-1, 1), c(-1, 1)))
    bs <- bounding_sphere(cube)
    expect_equal(bs$center, c(0, 0, 0))
    expect_equal(bs$radius, sqrt(3), tolerance = 1e-12)

    # Elongated box 1 x 2 x 4.
    box <- as.matrix(expand.grid(c(-0.5, 0.5), c(-1, 1), c(-2, 2)))
    bs2 <- bounding_sphere(box)
    expect_equal(bs2$center, c(0, 0, 0))
    expect_equal(bs2$radius, 0.5 * sqrt(1 + 4 + 16), tolerance = 1e-12)

    # Off-center.
    bs3 <- bounding_sphere(cube + 10)
    expect_equal(bs3$center, c(10, 10, 10))
    expect_equal(bs3$radius, sqrt(3), tolerance = 1e-12)
})


test_that("bounding_sphere accepts mesh3d, fs.surface and lists and pools vertices.", {
    mesh <- rgl::cube3d()
    bs <- bounding_sphere(mesh)
    expect_equal(bs$radius, sqrt(3), tolerance = 1e-12)

    sf <- freesurferformats::read.fs.surface(
        system.file("extdata", "cube.ply", package = "fsbrain", mustWork = TRUE))
    bs_sf <- bounding_sphere(sf)
    expect_equal(bs_sf$radius, sqrt(3), tolerance = 1e-12)

    # A list of two unit cubes far apart: pooled AABB has a bigger sphere.
    c1 <- as.matrix(expand.grid(c(0, 1), c(0, 1), c(0, 1)))
    c2 <- c1
    c2[, 1] <- c2[, 1] + 100  # shift along x only (avoids column-major recycling)
    bs_list <- bounding_sphere(list(c1, c2))
    expect_equal(bs_list$center, c(50.5, 0.5, 0.5))
    expect_equal(bs_list$radius, 0.5 * sqrt(101^2 + 1 + 1), tolerance = 1e-12)
})


test_that("bounding_sphere matches the sphere rgl auto-fits for a rendered scene.", {
    set.seed(42)
    V <- matrix(rnorm(30 * 3, sd = 3), ncol = 3)
    rgl::open3d(useNULL = TRUE)
    on.exit(rgl::close3d(), add = TRUE)
    rgl::points3d(V)
    rgl::view3d(fov = 0, interactive = FALSE)
    bbox <- rgl::par3d("bbox")  # c(xmin,xmax,ymin,ymax,zmin,zmax)
    expected_radius <- sqrt(sum((bbox[c(2, 4, 6)] - bbox[c(1, 3, 5)])^2)) / 2
    # rgl stores vertex positions as float32, so its bbox has ~1e-7 relative error.
    expect_equal(bounding_sphere(V)$radius, expected_radius, tolerance = 1e-4)
})


# ---------------------------------------------------------------------------
# 2. Rotate-camera == rotate-mesh invariance (the core premise of Step 2).
#    Verified against real rgl (headless): for every fsbrain view, rendering
#    the mesh rotated by R_mesh with the fixed camera U_view produces the same
#    projection as rendering the mesh unrotated with camera U_view %*% R_mesh.
# ---------------------------------------------------------------------------

test_that("Rotating the camera == rotating the mesh for all fsbrain views (cube).", {
    sf <- freesurferformats::read.fs.surface(
        system.file("extdata", "cube.ply", package = "fsbrain", mustWork = TRUE))
    V <- sf$vertices

    for (view in names(current_view_spec)) {
        spec <- current_view_spec[[view]]
        R_mesh <- rotation.matrix(spec$axis[1], spec$axis[2], spec$axis[3], spec$axis[4])
        U_view <- view3d_user_matrix(spec$theta, 0)

        # Case A (current behaviour): rotate vertices, fixed camera.
        V_rot <- transform_coords(V, R_mesh)
        A <- project_vertices_headless(V_rot, U_view)

        # Case B (camera-based): unrotated vertices, camera applies the mesh
        # rotation too. transform_renderable rotates meshes as t(R) %*% vb
        # (column convention), so the camera userMatrix must be U_view %*% t(R).
        B <- project_vertices_headless(V, U_view %*% t(R_mesh))

        expect_equal(max(abs(A - B)), 0, tolerance = 1e-6,
                     info = sprintf("View '%s': rotate-mesh vs rotate-camera must match.", view))
    }
})


test_that("Rotating the camera == rotating the mesh for a non-symmetric cloud.", {
    set.seed(1)
    V <- matrix(rnorm(60 * 3, sd = 4), ncol = 3)  # non-symmetric

    for (view in names(current_view_spec)) {
        spec <- current_view_spec[[view]]
        R_mesh <- rotation.matrix(spec$axis[1], spec$axis[2], spec$axis[3], spec$axis[4])
        U_view <- view3d_user_matrix(spec$theta, 0)

        V_rot <- transform_coords(V, R_mesh)
        A <- project_vertices_headless(V_rot, U_view)
        B <- project_vertices_headless(V, U_view %*% t(R_mesh))

        expect_equal(max(abs(A - B)), 0, tolerance = 1e-6,
                     info = sprintf("View '%s': rotate-mesh vs rotate-camera must match.", view))
    }
})


# ---------------------------------------------------------------------------
# 3. Baseline framing of the current rgl path on the demo cube.
#    Recorded 2026-08-23 from the current mesh-rotation implementation. The
#    camera-based rewrite (Step 2) must keep these values (it is pixel
#    identical for the 90/180 deg axis rotations fsbrain uses).
# ---------------------------------------------------------------------------

test_that("Current rgl path projects the demo cube to the baseline bbox for all views.", {
    sf <- freesurferformats::read.fs.surface(
        system.file("extdata", "cube.ply", package = "fsbrain", mustWork = TRUE))
    V <- sf$vertices

    expected_bbox <- c(xmin = 0.2113, xmax = 0.7887, ymin = 0.2113, ymax = 0.7887)

    for (view in names(current_view_spec)) {
        spec <- current_view_spec[[view]]
        R_mesh <- rotation.matrix(spec$axis[1], spec$axis[2], spec$axis[3], spec$axis[4])
        U_view <- view3d_user_matrix(spec$theta, 0)

        V_rot <- transform_coords(V, R_mesh)
        w <- project_vertices_headless(V_rot, U_view)
        bbox <- c(xmin = min(w[, 1]), xmax = max(w[, 1]), ymin = min(w[, 2]), ymax = max(w[, 2]))

        expect_equal(unname(bbox), unname(expected_bbox), tolerance = 1e-3,
                     info = sprintf("View '%s': baseline framing must be preserved.", view))
    }
})


# ---------------------------------------------------------------------------
# 4. Face-coloured cube: orientation baseline (headless).
#    The face cube (get.demo.facecolored.cube) has one distinct colour per face,
#    so the set of faces whose outward normal points toward the camera
#    identifies the orientation of a view. This pins the orientation the rgl
#    camera applies per view angle (U = U_view %*% t(R_mesh)); if someone
#    changes the per-view rotation/theta/phi, these expectations must be
#    revisited.
# ---------------------------------------------------------------------------

#' @title Compute which cube faces the rgl camera shows for a view.
#'
#' @param view character, a view angle name from \code{current_view_spec}.
#'
#' @param face_normals 6x3 matrix of outward face normals (rows named by face).
#'
#' @return character vector of face names whose normal points toward the camera.
#'
#' @keywords internal
visible_faces_for_rgl_view <- function(view, face_normals) {
    spec <- current_view_spec[[view]]
    R_mesh <- rotation.matrix(spec$axis[1], spec$axis[2], spec$axis[3], spec$axis[4])
    U_view <- view3d_user_matrix(spec$theta, 0)
    U <- U_view %*% t(R_mesh)
    visible <- c()
    for (i in seq_len(nrow(face_normals))) {
        z_cam <- (U %*% c(face_normals[i, ], 1))[3]
        if (z_cam > 1e-9) {
            visible <- c(visible, rownames(face_normals)[i])
        }
    }
    return(visible)
}


test_that("The face-coloured cube has correct outward winding on every face.", {
    fc <- get.demo.facecolored.cube()
    # Face cube geometry: 24 verts (vb is 4x24), 12 tris (it is 3x12).
    expect_equal(ncol(fc$coloredmesh$mesh$vb), 24L)
    expect_equal(nrow(fc$coloredmesh$mesh$it), 12L)

    for (i in seq_len(nrow(fc$face_normals))) {
        off <- (i - 1L) * 4L
        V <- t(fc$coloredmesh$mesh$vb[1:3, off + 1:4])
        n_calc <- cross_product(V[2, ] - V[1, ], V[3, ] - V[1, ])
        n_calc <- n_calc / sqrt(sum(n_calc^2))
        # Outward normal must agree with the face's label axis.
        expect_equal(n_calc, fc$face_normals[i, ], tolerance = 1e-9,
                     info = sprintf("Face '%s' winding is not outward.", rownames(fc$face_normals)[i]))
    }
})


test_that("Face-cube orientation baseline: rgl camera shows the expected faces per view.", {
    fc <- get.demo.facecolored.cube()

    expected <- list(
        "lateral_lh" = c("blue", "cyan"),  # face axes: -x, +y
        "lateral_rh" = c("yellow", "cyan"),
        "medial_lh"  = c("yellow", "cyan"),
        "medial_rh"  = c("blue", "cyan"),
        "dorsal"     = c("red"),
        "ventral"    = c("green"),
        "rostral"    = c("blue", "cyan"),
        "caudal"     = c("magenta")
    )
    # Map face name -> face axis label used by the cube.
    face_axis <- c("+z" = "red", "-z" = "green", "-x" = "blue", "+x" = "yellow",
                   "+y" = "cyan", "-y" = "magenta")

    for (view in names(expected)) {
        vis <- visible_faces_for_rgl_view(view, fc$face_normals)
        vis_names <- unname(face_axis[vis])
        expect_equal(sort(vis_names), sort(expected[[view]]),
                     info = sprintf("View '%s': rgl orientation changed.", view))
    }
})
