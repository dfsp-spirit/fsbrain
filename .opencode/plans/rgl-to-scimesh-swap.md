# Plan: rgl → scimesh Renderer Swap in fsbrain

## Executive Summary

scimesh is a headless, GPU-free C++ software renderer that now covers all 14
replaceable rgl functions used in fsbrain's static rendering pipeline.  The
remaining ~11 rgl calls fall into two categories: (a) fundamentally GPU-bound
(interactive animation, WebGL) that stay with rgl, and (b) compositing/colorbar
calls that fsbrain already handles better with its existing magick pipeline.

**Strategy**: A single integration point in `vislayout.from.coloredmeshes()`
routes to scimesh when `options("fsbrain.renderer_backend") == "scimesh"`.
Everything else — the magick compositing, the colorbar pipeline, and all vis.*
functions — remains unchanged.

**Zero changes needed in scimesh.** All neuro-specific view mapping lives in
fsbrain's bridge code.

---

## Part 1: Call Chain Analysis

Every core fsbrain vis function converges on the same path:

```
vis.subject.morph.native()      ─┐
vis.subject.morph.standard()    ─┤
vis.subject.label()             ─┤
vis.subject.annot()             ─┤
vis.data.on.subject()           ─┤  → brainviews() → brainview.sd/t4/t9/si/sr → rgl
vis.symmetric.data.on.subject() ─┤        (interactive rendering, stays rgl)
vis.region.values.on.subject()  ─┤
vis.data.on.fsaverage()         ─┘
                                       OR
                                  ──→ vislayout.from.coloredmeshes()
                                       (export path, gains scimesh branch)
```

The paper-figure workflow is:
1. `coloredmeshes <- vis.subject.morph.native(..., views=NULL)` — build data, no rendering
2. `vislayout.from.coloredmeshes(coloredmeshes, ...)` — render + composite + export

**Only step 2 needs a scimesh branch.** All vis.* functions are untouched.

---

## Part 2: What scimesh already covers

| rgl function | scimesh equivalent | Notes |
|---|---|---|
| `open3d` | Headless by design | No device needed |
| `par3d` | `render_options()` | Width, height, etc. |
| `bg3d` | `render_options(background_color=...)` | |
| `shade3d` | `render_scene()` with vertex colors, smooth/flat shading | |
| `view3d` | `camera_auto()` + direction vectors | View mapping in fsbrain bridge |
| `rgl.snapshot` | `write_png()` | |
| `rgl.useNULL` | Always headless | |
| `close3d` | No device; no cleanup | |
| `tmesh3d` | `mesh_from_rgl()` converter | scimesh already ships this |
| `material3d` | `render_options()` | Specular, shininess, wireframe, backface culling, orthographic projection — all implemented |
| `addNormals` | `compute_vertex_normals()` | Auto-computed in C++ |
| `triangles3d` | `render_triangles()` | Raw triangle mode |
| `spheres3d` | `render_spheres()` | Via icosphere mesh generation |
| `segments3d` | `render_lines()` | Via cylinder meshes |
| `rotate3d` | `rotate_mesh()` | Mesh-level rotation |
| `translate3d` | `translate_mesh()` | Mesh-level translation |
| `rgl.postscript` | N/A | PNG is primary format |

**All features that a software renderer can provide are complete.**

---

## Part 3: What fsbrain keeps (unchanged)

| Component | Why kept | File |
|---|---|---|
| `arrange.brainview.images()` | Better quality layout compositing than scimesh's `compose_layout()` | `brainview_magick.R` |
| `coloredmesh.plot.colorbar.separate()` | Publication-quality colorbars via `fields::image.plot` | `cbar.R` |
| `combine.colorbar.with.brainview.image()` | Merges colorbar + brain views properly | `helpers_magick.R` |
| `brainview.si()` | Interactive viewer — GPU-bound | `vis_multiview.R` |
| `brainview.sr()` | Rotating animation — GPU-bound | `vis_multiview.R` |
| `vis.coloredmeshes.rotating()` | Animated rotation — GPU-bound | `vis_meshes.R` |
| `vis.rglwidget()` | WebGL widget — GPU-bound | `vis_rglwidget.R` |
| `rgl.postscript` / vector export | rgl-specific format | `rglactions.R` |
| `volvis.lb.with.surface()` | Lightbox volume rendering — already rgl-free | `vis_volume.R` |

---

## Part 4: Implementation Plan

### 4.1 New file: `R/scimesh_bridge.R` (~120 lines)

```r
# Color conversion
hex_to_rgba(hex)                    # "#FF0000" → c(1, 0, 0, 1)

# Mesh bridge
coloredmeshes_to_scimesh(cms)       # fs.coloredmesh → scimesh mesh descriptor list
                                     # Uses scimesh::mesh_from_rgl() internally.
                                     # Extracts vertices, triangles, vertex colors.
                                     # Handles lh/rh hemilist structure.
                                     # Skips meshes with render=FALSE.

# Style bridge
fsbrain_style_to_scimesh_options(style, bg_rgba)
                                     # "default" → render_options(shading="smooth", ...)
                                     # "shiny"  → render_options(specular_color=..., shininess=50)
                                     # "semitransparent" → alpha blending
                                     # "glass"  → alpha + backface culling
                                     # "edges"  → render_options(wireframe=TRUE)

# Camera bridge
view_angle_to_scimesh_camera(scene_meshes, view_angle)
                                     # "lateral_lh" → direction=c(-1,0,0), up=c(0,0,1), hemi_filter="lh"
                                     # "medial_lh"  → direction=c(1,0,0),  up=c(0,0,1), hemi_filter="lh"
                                     # "lateral_rh" → direction=c(1,0,0),  up=c(0,0,1), hemi_filter="rh"
                                     # "medial_rh"  → direction=c(-1,0,0), up=c(0,0,1), hemi_filter="rh"
                                     # "dorsal"     → direction=c(0,0,1),  up=c(0,1,0), hemi_filter="both"
                                     # "ventral"    → direction=c(0,0,-1), up=c(0,-1,0), hemi_filter="both"
                                     # "rostral"    → direction=c(0,1,0),  up=c(0,0,1), hemi_filter="both"
                                     # "caudal"     → direction=c(0,-1,0), up=c(0,0,1), hemi_filter="both"
                                     # Uses scimesh::camera_auto() for framing.
```

### 4.2 Camera Direction Mapping (FreeSurfer RAS coordinates)

FreeSurfer RAS: X = left→right, Y = posterior→anterior, Z = inferior→superior.

| view_angle | hemi to show | camera direction | Notes |
|---|---|---|---|
| `lateral_lh` | lh only | `c(-1, 0, 0)` | Look from left |
| `medial_lh` | lh only | `c(1, 0, 0)` | Look from right at medial wall |
| `lateral_rh` | rh only | `c(1, 0, 0)` | Look from right |
| `medial_rh` | rh only | `c(-1, 0, 0)` | Look from left at medial wall |
| `dorsal` | both | `c(0, 0, 1)` | Look from above |
| `ventral` | both | `c(0, 0, -1)` | Look from below |
| `rostral` | both | `c(0, 1, 0)` | Look from front |
| `caudal` | both | `c(0, -1, 0)` | Look from back |

**This mapping requires empirical verification.** fsbrain currently rotates meshes
(with `rgl::rotate3d()`) and then uses a fixed camera (`rgl::view3d()`).  If the
simple direction mapping above doesn't produce equivalent views, we can fall back
to replicating fsbrain's approach: use scimesh's `rotate_mesh()` to transform
mesh vertices per view, then use a fixed camera direction.

### 4.3 Modified file: `R/brainview_magick.R` (~50 lines in `vislayout.from.coloredmeshes()`)

In the per-view loop, add a branch before the existing `brainviews()` call:

```r
vislayout.from.coloredmeshes <- function(coloredmeshes, view_angles, ...) {
    ...
    view_images <- tempfile(view_angles, fileext = ".png")

    if (getOption("fsbrain.renderer_backend", "rgl") == "scimesh") {
        if (!requireNamespace("scimesh", quietly = TRUE)) {
            stop("Renderer backend is 'scimesh' but the scimesh package is not installed.")
        }
        scene <- coloredmeshes_to_scimesh(coloredmeshes)
        bg_rgba <- hex_to_rgba(background_color)
        opts <- fsbrain_style_to_scimesh_options(style, bg_rgba)
        for (view_idx in seq_len(length(view_angles))) {
            view <- view_angles[[view_idx]]
            cam <- view_angle_to_scimesh_camera(scene, view)
            renderable <- filter_scene_by_view(scene, view)
            img <- scimesh::render_scene(renderable, cam, opts)
            scimesh::write_png(img, view_images[[view_idx]])
        }
    } else {
        # --- existing rgl path (unchanged) ---
        for (view_idx in seq_len(length(view_angles))) {
            view <- view_angles[[view_idx]]
            ...
            brainviews(c(view), coloredmeshes, rgloptions = rgloptions,
                       rglactions = final_rglactions, style = style,
                       background = background_color)
        }
    }

    # Common: magick compositing (unchanged)
    return(invisible(arrange.brainview.images(view_images, output_img, ...)))
}
```

### 4.4 Modified file: `R/helpers.R` (+8 lines)

```r
#' @title Get the current renderer backend
#'
#' @description Returns the current fsbrain renderer backend. Defaults
#'   to "rgl" if the option \code{fsbrain.renderer_backend} is not set.
#'   Set it with \code{options(fsbrain.renderer_backend = "scimesh")}.
#'
#' @return character string, either "rgl" or "scimesh"
#'
#' @export
get.fsbrain.renderer.backend <- function() {
    getOption("fsbrain.renderer_backend", default = "rgl")
}
```

### 4.5 Modified file: `DESCRIPTION` (+1 line)

Add `scimesh` to `Suggests:`.

---

## Part 5: What is NOT changed

- **scimesh itself** — Zero changes. Neuro-specific mapping is in fsbrain.
- **All `vis.subject.*` and `vis.data.*` functions** — They build coloredmeshes and call `brainviews()`. No changes.
- **All `brainview.*` functions** — They remain as rgl-only direct render paths.
- **`coloredmesh.from.*` functions** — They produce tmesh3d objects which the bridge converts.
- **Magick compositing pipeline** — `arrange.brainview.images()`, all helpers. Used by both backends.
- **Colorbar pipeline** — `coloredmesh.plot.colorbar.separate()`, `combine.colorbar.with.brainview.image()`.
- **Volume rendering** — Already rgl-free.
- **Group-level vis functions** — They call the same `vis.subject.morph.*` functions internally.

---

## Part 6: Open Questions / Risks

1. **Camera direction mapping** — The direction vectors above are theoretical.
   They must be verified empirically with actual FreeSurfer brain renders.
   If they don't match, we use scimesh's `rotate_mesh()` to replicate fsbrain's
   `rotate3d()` approach exactly, then apply a fixed camera.

2. **Alpha handling** — fsbrain uses hex colors like `"#FF0000"` and a special
   `col.na` color. scimesh uses float RGBA [0-1]. The `hex_to_rgba()` converter
   needs to handle the `col.na` value (typically `"#FEFEFE"`) and map it to
   fully transparent (`c(1, 1, 1, 0)` or similar). The actual col.na value must
   be extracted from the coloredmeshes' metadata.

3. **shift_hemis_apart** — Modifies tmesh3d vertices before rendering. Since
   the bridge extracts vertices from the already-shifted tmesh3d, the shift
   is automatically reflected. No special handling needed.

4. **Highlight points** — The scimesh path can support these via
   `scimesh::render_spheres()`. Phase 2 enhancement.

5. **View labels** ("lateral lh", "medial rh") — These are 3D text in rgl.
   Not supported in the initial scimesh path. Can be added as 2D annotations
   during compositing. Phase 2 enhancement.

---

## Part 7: Implementation Phases

### Phase A (this plan) — Core static rendering
1. Write `R/scimesh_bridge.R` with all bridge functions
2. Add `get.fsbrain.renderer.backend()` to `R/helpers.R`
3. Add scimesh branch in `vislayout.from.collected.meshes()` in `R/brainview_magick.R`
4. Add `scimesh` to `DESCRIPTION` Suggests
5. Adjust `NAMESPACE` (re-generated from roxygen)
6. **~180 lines of new/modified code, 3 files touched**

### Phase B — Verification and refinement
6. Empirical camera direction verification and tuning
7. Alpha/transparency edge case handling
8. Style mapping fine-tuning

### Phase C — Extensions (future work)
9. Highlight points via `render_spheres()`
10. View labels via 2D overlay
11. Volume isosurface rendering via `render_triangles()`
