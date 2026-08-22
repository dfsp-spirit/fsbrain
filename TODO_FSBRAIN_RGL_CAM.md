# TODO: unify fsbrain view positioning (mesh-rotation vs camera)

## Problem

The rgl backend bakes each view's orientation into the mesh geometry:

- `vis.rotated.coloredmeshes()` (`R/vis_meshes.R`) applies
  `rgl::rotate3d(mesh, angle, x, y, z)` to the mesh before rendering.
- `brainview.t4()` / `brainview.t9()` (`R/vis_multiview.R`) then use a FIXED
  camera (`rgl::view3d(theta, phi, fov=0, interactive=FALSE)`).

So a "view" is produced by rotating the object, not the camera.

## Why this is a problem

The same rotation must be re-applied to every object in the scene:

- `vis.rotated.coloredmeshes()` special-cases three renderable types
  (`fs.coloredmesh`, `fs.coloredvoxels`, `Triangles3D`), each with its own
  `rotate3d` call.
- `handle.rglactions.highlight.points()` (`R/vis_multiview.R`) separately
  rotates marker coordinates so highlight spheres land in the correct view.

Adding any new renderable means remembering to rotate it the same way —
duplicated, error-prone, and easy to get subtly wrong.

## Difference vs the scimesh backend

The scimesh bridge is already camera-based: `view_angle_to_scimesh_camera()`
(`R/scimesh_bridge.R`) computes `eye`/`center`/`up` in plain R and positions a
camera, leaving the meshes unrotated. Projection already matches (rgl `fov=0`
orthographic == scimesh `projection="orthographic"`); only the orientation
mechanism differs. Until this is unified, backend parity is guaranteed by
`examples/rgl_vs_scimesh/camera_verification.R`, not by shared code.

## Plan

### Step 1 (do now, as part of the scimesh work)
Centralize the transform in ONE plain-R helper — `apply.transform(renderable,
matrix)` with S3 dispatch — and route both the rgl path
(`vis.rotated.coloredmeshes()`) and `handle.rglactions.highlight.points()`
through it, plus any scimesh-side geometry. The helper rotates vertex normals
too (rigid rotation) to match `rgl::rotate3d`. This removes the per-type
duplication with NO change to rendered output.

### Step 2 (separate follow-up, NOT part of scimesh integration)
Migrate the rgl path from mesh-rotation to camera/view-transform, reusing the
same plain-R view→camera math as the scimesh bridge. This requires
reproducing the exact orthographic framing of today's views (bounding-box fit
vs per-view projected fit). Gate it with:

- `examples/rgl_vs_scimesh/camera_verification.R`
- visual regression against the fsbrain gallery / vignettes / README figures

Do not fold this into the scimesh branch — it changes every existing figure
and is orthogonal to rgl-vs-scimesh.
