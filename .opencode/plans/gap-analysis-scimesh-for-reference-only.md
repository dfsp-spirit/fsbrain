# Plan: scimesh-fsbrain Gap Analysis & Camera/Volume Assessment

## 1. Feature Gap: scimesh vs fsbrain rgl requirements

### 25 rgl functions used in fsbrain — status update

| # | rgl function | scimesh status | Notes |
|---|-------------|----------------|-------|
| 1 | `open3d` | **Done** | Headless by design |
| 2 | `par3d` | **Done** | `render_options()` |
| 3 | `bg3d` | **Done** | `background_color` |
| 4 | `shade3d` | **Done** | `render_mesh()`/`render_scene()` |
| 5 | `view3d` | **Done** | `.view_to_camera()` + `camera_auto()` |
| 6 | `layout3d` + `next3d` | **Done** | `compose_layout()` |
| 7 | `bgplot3d` + `image.plot` | **Done** | `colorbar_horizontal()`/`vertical()` |
| 8 | `rgl.snapshot` | **Done** | `write_png()` |
| 9 | `rgl.postscript` | **N/A** | Not needed (PNG is primary) |
| 10 | `addNormals` | **Done** | Auto-computed in C++ |
| 11 | `tmesh3d` | **Done** | `scimesh::Mesh` |
| 12 | `rgl.useNULL` | **Done** | Always headless |
| 13 | `close3d` | **N/A** | No device |
| 14 | `material3d` | **Done** | `wireframe`, `specular`, `shininess`, `backface_culling` |
| 15 | `triangles3d` | **Done** | `render_triangles_raw()` (this session) |
| 16 | `spheres3d` | **Done** | `generate_multi_spheres()` + `render_spheres()` |
| 17 | `rotate3d` | **Done** | `transform_mesh()`, `translate_mesh()`, `scale_mesh()`, `rotate_mesh()` |
| 18 | `segments3d` | **Done** | `render_lines()` via cylinder meshes |
| 19 | `translate3d` | **Done** | `translate_mesh()` |
| 20 | `text3d` | **R level** | View labels as 2D overlay (fsbrain-side code) |
| 21 | `rglwidget` | **Impossible** | WebGL requires GPU |
| 22 | `play3d` | **Impossible** | Real-time rotation requires GPU |
| 23 | `movie3d` | **Impossible** | Animation requires GPU |
| 24 | `spin3d` | **Impossible** | Rotation controller requires GPU |
| 25 | `rgl.snapshot` | **Done** | PNG export |

### Remaining scimesh work

**Nothing.** All features that a software renderer can provide are complete.
The 4 "Impossible" items are inherently GPU-bound (60+ fps real-time
interaction) and cannot be done by any CPU software rasterizer.
The `text3d` item is kept in R (fsbrain would add 2D text overlays).

### Remaining fsbrain integration work

These live in fsbrain's repo, not scimesh:

1. **`coloredmeshes.to.scimesh()`** — converter from fsbrain's `fs.coloredmesh`
   format to scimesh `Mesh` descriptors (~30 lines of R)

2. **`renderer_backend` parameter** — branch in each vis function:
   `vis.subject.morph.native()`, `vislayout.from.coloredmeshes()`,
   `brainview.t4()`, `brainview.t9()`, `brainview.si()`.
   (~250 lines across 5 files)

3. **Style mapping**: fsbrain style names → scimesh render options
   (`"shiny"` → `specular_color + shininess`, etc.)

4. **Camera bridge**: fsbrain `view_angles` → scimesh camera vectors.
   Already implemented in scimesh (`.resolve_views()` + `.view_to_camera()`),
   just needs to be exposed at the integration level.

---

## 2. Camera / View Parity: Can scimesh Match rgl Output?

### rgl's view3d vs scimesh's camera_auto

rgl: `view3d(theta, phi, fov=0, zoom=1)`
- `theta` = rotation around y-axis, `phi` = rotation around x-axis
- Camera orbits around the scene center in spherical coordinates
- `fov=0` means orthographic (special rgl convention)

scimesh: `camera_auto(mesh, direction, up, fov, margin)`
- Uses explicit LookAt vectors: eye, center, up
- Camera placed at `center + direction * distance` then rendered with
  `glm::lookAt(eye, center, up)`

### Are the views identical?

**Yes, mathematically.** rgl internally computes a LookAt matrix from the
spherical coordinates. Both rgl and scimesh ultimately produce the same
kind of view matrix. The question is: do our pre-defined anatomical views
produce the same visual result as fsbrain's?

fsbrain uses a fundamentally different approach to multi-view rendering
than scimesh does, which makes direct A/B comparison less meaningful:

- **fsbrain with rgl**: Rotates the MESH to face the camera for each view,
  then uses a fixed camera. This means the camera stays at the same
  position and the mesh gets rotated by `rgl::rotate3d()` for each view.

- **scimesh**: Keeps the mesh in place and moves the CAMERA. The camera
  is fit to the mesh bbox for each view via `camera_auto()` with a
  per-view direction vector.

Both approaches produce equivalent output: the mesh is viewed from the
same anatomical direction. The differences are:
- scimesh may show slightly different scaling (camera distance computed
  from bbox vs rgl's fixed distance)
- Perspective vs orthographic: scimesh uses perspective (fov=45) by default;
  rgl in fsbrain uses `fov=0` (orthographic). Orthographic would need to be
  selected explicitly in scimesh via `render_options(projection="orthographic")`.
  
  NOTE: scimesh's render_options currently doesn't expose the projection
  type. The `camera()` function has a `projection` parameter but
  `render_options()` doesn't pipe it through. This is a missing feature.

### What needs to change for pixel-perfect visual parity?

**One missing feature**: expose `projection` in `render_options()` so the
user can switch between perspective (default) and orthographic. Currently
the `camera` struct has `projection` but `render_options` doesn't pass it.

This is a 2-line addition to `render_options()` and `build_options_from_r()`.

### Verdict on camera parity

**Practically yes, perfectly if we add orthographic support.** The views
are equivalent enough for publication figures. Minor differences in
camera distance and projection type can be adjusted. No fundamental
compatibility issue exists.

---

## 3. Volume Slice Visualization (volvis.lb.with.surface)

### What it does

```
MRI volume (3D)  →  extract 2D slices  →  overlay surface contours  →  lightbox grid
```

This is NOT 3D rendering at all. The pipeline is:

1. **Volume → slices**: Extract 2D images from 3D array along an axis
   (sagittal/coronal/axial). Uses `vol.slice()` which extracts a 2D matrix
   from the 3D volume array and converts to a magick image. No 3D rendering.

2. **Mesh → CRS space**: `mesh.ras2crs()` transforms surface vertices from
   RAS to voxel coordinate space using the inverse of the vox2ras matrix.
   Pure linear algebra in R.

3. **Mesh-plane intersection**: `mesh.slice.intersection()` computes which
   triangles cross the slice plane, then computes the line segment where
   each crossing triangle intersects the plane. Returns 2D line segments
   in voxel coordinates. Pure computational geometry in R.

4. **Draw contours on slices**: `draw.segments.on.image()` draws the line
   segments onto the magick slice image using `graphics::segments()`.
   2D drawing on existing images.

5. **Lightbox grid**: `volvis.lightbox()` arranges slice images in a grid
   using magick's image compositing.

### Is this in scimesh's scope?

**No.** None of these steps use 3D rendering. Neither rgl nor scimesh is
involved. The entire pipeline is:
- 2D image processing (volume → slices)
- Computational geometry (mesh-slice intersection)
- 2D line drawing on images (R graphics)
- Image compositing (magick)

A scimesh backend for fsbrain would not touch these functions at all.
They would continue to work as-is with the magick pipeline, since they
don't depend on rgl.

### What about 3D volume rendering?

For actual 3D volume visualization (e.g., `vis.volume.on.subject()` which
renders isosurfaces from volume data via `misc3d::contour3d()`), the output
is a mesh (triangle set). scimesh CAN render these — it would just receive
the isosurface triangles as a `Mesh`. The `vis.volume.on.subject()` function
computes the isosurface via `misc3d::contour3d()`, produces `Triangles3D`
objects, then renders them via `rgl::triangles3d()`. With scimesh, the
same `Triangles3D` objects could be converted to a `Mesh` (using the raw
triangle rendering path that already exists).

### Summary for volume features

| Function | Uses rgl? | Replaceable with scimesh? |
|----------|-----------|--------------------------|
| `volvis.lb.with.surface()` | **No** (pure R/magick) | N/A — already rgl-free |
| `vis.volume.on.subject()` | **Yes** (triangles3d) | **Yes** — convert contour3d → raw triangles → render |
| `rglvoxels()` | **Yes** (triangles3d) | **Yes** — same raw triangle path |

---

## 4. What's Actually Missing for the Swap?

### One tiny scimesh gap

| Feature | Effort |
|---------|--------|
| `projection` in `render_options()` (perspective vs orthographic) | 2 lines R + 2 lines C++ |

### fsbrain integration (not scimesh)

| Feature | Lines | Priority |
|---------|-------|----------|
| `coloredmeshes.to.scimesh()` converter | ~30 | **Critical** — bridge function |
| `renderer_backend` branch in vis functions | ~250 | **Critical** — the actual switch |
| Style name → render options mapping | ~20 | **High** — "shiny" etc. |
| 2D text label overlay during compositing | ~30 | **Low** — cosmetic |
| Camera bridge (view_angles → direction vectors) | ~15 | Already done in scimesh, just needs R wrapper |
