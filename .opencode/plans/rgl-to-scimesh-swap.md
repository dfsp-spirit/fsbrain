# Plan: rgl → scimesh Renderer Swap in fsbrain

## Executive Summary

fsbrain uses **25 distinct rgl functions** across 12 source files.  Of these,
scimesh already replaces ~14 for the core static rendering pipeline.  ~6 would
need new scimesh features (C++ and R).  ~5 are fundamentally rgl-specific
(WebGL, animation, interactive rotation) and would require fsbrain to keep rgl
for those paths.  A practical approach: static image export uses scimesh;
interactive/animated modes fall back to rgl.

---

## Part 1: What scimesh already covers ✓

| rgl function | scimesh equivalent | Status |
|---|---|---|
| `open3d` | Headless by design, no device needed | **Done** |
| `par3d` | `render_options()` controls output | **Done** |
| `bg3d` | `render_options(background_color=...)` | **Done** |
| `shade3d` | `render_mesh()`/`render_scene()` with vertex colors, smooth/flat shading | **Done** |
| `view3d` | `.view_to_camera()` + `camera_auto()` → anatomical view presets | **Done** |
| `layout3d` + `next3d` | `compose_layout()` → multi-view grid with per-row/col crop | **Done** |
| `bgplot3d` + `image.plot` | `colorbar_horizontal()`/`vertical()` with title, ticks, auto-scale | **Done** |
| `rgl.snapshot` | `write_png()` — in-memory RGBA → PNG | **Done** |
| `rgl.postscript` | Not needed; PNG output is the primary format | **N/A** |
| `addNormals` | C++ `compute_vertex_normals()` auto-runs if mesh has no normals | **Done** |
| `tmesh3d` | `scimesh::Mesh` with `vertices`, `triangles`, `colors` | **Done** |
| `material3d` | `render_options()` has `shading`, `backface_culling`, `invert_normals` | **Partial** |
| `rgl.useNULL` | Always headless | **Done** |
| `close3d` | No device; no cleanup needed | **N/A** |

**Result: ~14 of 25 rgl functions are directly replaceable today for the core
static rendering pipeline used by `vis.subject.morph.native()` and
`vislayout.from.coloredmeshes()`.**

---

## Part 2: What scimesh would need to add

### 2.1 C++ Level

#### `triangles3d` — Raw triangle rendering with per-face colors
- **Where used**: `vis.renderable()` for voxel meshes, `vis_volume_3d.R` for volume rendering
- **What rgl does**: Renders arbitrary triangle sets (not necessarily connected meshes) with per-vertex or per-face colors
- **scimesh gap**: Currently requires a `Mesh` struct with vertices + indices.  A "raw triangle" mode (3 vertices per triangle, no indexing) would be needed
- **Effort**: Small. Add a `render_triangles(float* verts, int n_tris, float* colors)` overload or a "raw" mesh mode in the existing pipeline
- **Priority**: Medium — needed for voxel rendering and misc3d isosurfaces

#### `segments3d` — 3D line rendering
- **Where used**: `helpers.R` for path visualization, `spherical.R` for debugging axes
- **What rgl does**: Draws line segments in 3D with configurable color, width, antialiasing
- **scimesh gap**: No line rasterizer at all
- **Effort**: Medium. Could implement Bresenham's 3D line algorithm in the rasterizer, or render thin quads (two triangles per segment)
- **Priority**: Low — not critical for the core brain surface visualization

#### `spheres3d` — Sphere primitive rendering
- **Where used**: `highlight.R` for highlighting points of interest
- **What rgl does**: Draws 3D spheres at coordinates with configurable radius and color
- **scimesh gap**: No sphere primitive.  Could be done with a sphere mesh generator + existing mesh rendering
- **Effort**: Medium. Generate an icosphere mesh, transform to position+radius, render via existing pipeline.  The R layer would do the mesh generation, C++ would render it normally
- **Priority**: Low — highlight points are a nice-to-have

#### `rotate3d` — Vertex rotation transform
- **Where used**: `vis_meshes.R` for multi-view brain layouts (rotating meshes for different viewing angles)
- **What rgl does**: Rotates mesh vertex coordinates by angle around an axis
- **scimesh gap**: scimesh handles view changes via camera transforms, not mesh transforms.  Mesh rotation is not needed — the same visual result is achieved by moving the camera
- **Effort**: None — camera transforms replace mesh rotation
- **Priority**: Not needed — handled architecturally by camera system

### 2.2 R Level

#### Style parameters mapping (`material3d` → `render_options`)
- **Where used**: `get.rglstyle()` for "default", "shiny", "semitransparent", "glass", "edges" styles
- **What rgl does**: Maps style names to `material3d` parameters (shininess, specular, alpha, front/back)
- **scimesh gap**: RenderOptions doesn't have specular highlights, transparency per mesh, or "edges" mode
- **What to do**: Add a `style_to_options()` mapping in R that translates fsbrain style names to scimesh render options.  Not all styles will transfer perfectly (e.g., "edges" = wireframe, which is declared but not implemented in scimesh)
- **Effort**: Small (R-only mapping function)
- **Priority**: Medium — needed for API compatibility

#### `rgl::tmesh3d` → `scimesh::Mesh` converter
- **Where used**: `coloredmesh.R` as the bridge between FreeSurfer and rgl
- **scimesh gap**: Need an R function `fs.coloredmesh.to.scimesh(cmesh)` that converts the fsbrain coloredmesh format to scimesh Mesh format
- **Effort**: Small (R-only conversion)
- **Priority**: High — foundational for the swap

#### Text labels (`text3d` → 2D overlay)
- **Where used**: `vis_multiview.R` for "lateral lh", "medial rh" labels in multi-view layouts
- **What rgl does**: Renders text positioned in 3D space
- **scimesh gap**: No text rendering
- **What to do**: Render text as 2D overlay during compositing (after rendering each view, add text via `grid::textGrob` or similar).  Or: skip labels (they're cosmetic).
- **Effort**: Medium for 2D overlay approach
- **Priority**: Low — cosmetic only

---

## Part 3: What can NEVER be replaced with scimesh

These rgl features are fundamentally tied to OpenGL/WebGL/GPU and cannot
be done with a CPU software rasterizer:

| Feature | Why unreplaceable | Impact |
|---|---|---|
| `rglwidget` | WebGL HTML widget for interactivity in Shiny/RMarkdown | **Must keep rgl** for this path |
| `play3d` | Real-time interactive rotation via OpenGL | **Must keep rgl** |
| `movie3d` | Animated GIF recording (frames captured from rgl) | **Must keep rgl** or use static compositing |
| `spin3d` | Rotation controller for interactive/animated rotation | **Must keep rgl** |

These are fundamentally interactive (60+ fps via GPU) — a software rasterizer
at 0.5-2 seconds per frame cannot replace them.

---

## Part 4: What changes fsbrain would need (rough estimate)

The goal: a `renderer_backend` parameter in fsbrain's vis functions that
routes to either rgl (current default) or scimesh (new path).

### 4.1 New functions fsbrain would need

```r
# Bridge: convert fsbrain coloredmesh to scimesh-compatible scene
coloredmeshes.to.scimesh <- function(coloredmeshes) {
    # For each coloredmesh:
    #   - Extract vertices (RAS coordinates from fs.surface)
    #   - Extract faces (triangle indices)
    #   - Extract vertex colors (from cmesh$col)
    #   - Build scimesh Mash descriptor list
    # Returns: list of scimesh Mesh descriptors (lh and rh)
}

# Style bridge: fsbrain style → scimesh render options
fsbrain.style.to.scimesh <- function(style, background_color) {
    # Map "default", "shiny", "semitransparent", "glass", "edges"
    # to scimesh::render_options() parameters
}

# Camera bridge: fsbrain view_angles → scimesh camera
fsbrain.views.to.scimesh <- function(view_angles) {
    # Already implemented in scimesh: .resolve_views() + .view_to_camera()
    # Just need to expose at the integration level
}
```

### 4.2 Functions fsbrain would modify

Each vis function (`vis.subject.morph.native`, `vislayout.from.coloredmeshes`,
`brainview.t4`, `brainview.t9`, `brainview.si`, `vis.coloredmeshes`) would
gain a branch:

```r
if (renderer_backend == "scimesh") {
    # 1. Convert coloredmeshes → scimesh scene
    scene <- coloredmeshes.to.scimesh(coloredmeshes)
    # 2. Resolve views → cameras
    cameras <- fsbrain.views.to.scimesh(view_angles)
    # 3. Render each view
    images <- list()
    for (cam in cameras) {
        img <- scimesh::render_scene(scene, cam, render_opts)
        images <- c(images, list(img))
    }
    # 4. Compose layout
    result <- scimesh::compose_layout(images, ..., crop = TRUE)
    # 5. Add colorbar if requested
    if (draw_colorbar) {
        cbar <- scimesh::colorbar_horizontal(...)
        result <- scimesh::compose_layout(images, colorbar = cbar, ...)
    }
    # 6. Export
    scimesh::write_png(result, output_img)
    return(invisible(result))
} else {
    # ... existing rgl code path ...
}
```

### 4.3 Estimated changes per file

| File | Change | Effort |
|---|---|---|
| `coloredmesh.R` | Add `coloredmeshes.to.scimesh()` converter | ~30 lines |
| `vis_meshes.R` | Add `renderer_backend` branch in `vis.coloredmeshes`, `vis.coloredmeshes.rotating` | ~40 lines |
| `vis_multiview.R` | Add scimesh branch in `brainview.t4`, `.t9`, `.si` | ~60 lines each |
| `brainview_magick.R` | Modify `vislayout.from.coloredmeshes` for scimesh path | ~40 lines |
| `cbar.R` | Wire scimesh colorbar in scimesh path | ~15 lines |
| **Total** | | **~250 lines of new R code** |

---

## Part 5: Recommended Implementation Order

### Phase A: Core rendering swap (highest impact)
1. **R-level**: `fs.coloredmesh.to.scimesh()` converter — bridge FreeSurfer mesh → scimesh Mesh
2. **R-level**: `fsbrain.style.to.scimesh.options()` — style name mapping
3. **Integration**: Modified `vis.subject.morph.native()` in fsbrain that also calls scimesh
4. **Integration**: Modified `vislayout.from.coloredmeshes()` for scimesh path

### Phase B: Volume/voxel support
5. **C++**: Raw triangle rendering mode (for voxel meshes from misc3d)
6. **R-level**: Voxel mesh → scimesh converter

### Phase C: Line and point primitives (low priority)
7. **C++**: Line rasterizer (thin quad approach)
8. **C++**: Sphere mesh generator
9. **R-level**: `segments3d` and `spheres3d` wrappers

### Phase D: Text and polish
10. **R-level**: Text overlay during compositing (for view labels)
11. **C++**: Wireframe mode (for "edges" style)

---

## Part 6: What scimesh would NOT need to add

- **Animation support**: Static image is scimesh's domain. Animation stays with rgl.
- **WebGL/HTML widget**: rglwidget stays with rgl.
- **Interactive rotation**: play3d/spin3d stay with rgl.
- **Real-time interaction**: scimesh is batch/headless.
- **3D text rendering**: Better done as 2D overlay in compositing stage.
- **Mesh rotation transforms**: Camera transforms achieve the same result.
