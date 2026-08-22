# TODO: scimesh renderer backend integration (fsbrain)

Status of the `feature/scimesh-backend` branch. Relevant code:
`R/scimesh_bridge.R` and the scimesh branch in `R/brainview_magick.R`
(`vislayout.from.coloredmeshes()`). Backend switch:
`options(fsbrain.renderer_backend = "scimesh")`, read via
`get.fsbrain.renderer.backend()`. scimesh stays a **pure renderer** and is
**optional** (`Suggests`, never `Imports`); rgl stays the default (`Imports`).

## Design principle

- scimesh is a thin renderer. Geometry (transforms, hemisphere shift), style
  resolution, camera math, and color/alpha are done in plain fsbrain R and
  shared between backends.
- Minimize per-package calls. The only irreducible per-backend steps are:
  submit mesh+material, set camera, snapshot/write the frame.
- magick is KEPT for image composition/colorbars for BOTH backends (do not
  remove it — no gain, only more if/else).
- The rgl path currently rotates meshes; the scimesh path uses a camera. See
  `TODO_FSBRAIN_RGL_CAM.md` for the plan to unify this.

## Decisions (2026-08-22)

- **Normals**: `apply.transform()` rotates vertex normals too (rigid rotation)
  so it matches `rgl::rotate3d`.
- **Styles**: `edges` -> scimesh `wireframe`. `semitransparent`/`glass` ->
  plain alpha on the RGBA A channel; the rgl `back="lines"` look is NOT
  reproduced (document the difference). No two-pass rendering.
- **`from_mesh`**: per-mesh style resolution is a bug - support it now.
- **`highlight_points`**: supported via `scimesh::render_spheres()` (rare
  special case, low priority but do it).
- **rglactions**: `clip_data` must stay renderer-independent (plain R);
  `no_vis` does not apply to scimesh (it only suppresses opening an rgl
  window) - ignore it in the scimesh backend; the rest are handled/warned
  per-backend.
- **Hemi shift**: opt-in only (via `shift_hemis_apart`), keep current
  behavior - it is a hack for weird/inflated meshes from some FreeSurfer
  versions; do not make it default, do not break it.
- **README**: do not over-promise - scimesh applies to static image export
  only, interactive views remain rgl.
- **scimesh version**: `Suggests: scimesh (>= 0.3.4)` (first CRAN version).
- **Output resolution**: new global option (not the `rgloptions$windowRect`
  overload), defaulting to a publication-quality size (~1920x1080). `export()`
  already has a `quality` knob; decide how it interacts.

## Status

- [x] scimesh on CRAN (v0.3.4). README/README_MACOS_TAHOE.md use
      `install.packages("scimesh")`; no GitHub-install workaround remains.
- [x] Backend switch + bridge functions exist (`R/scimesh_bridge.R`).
- [ ] T1: shared plain-R helpers.
- [ ] T2: style alpha/transparency.
- [ ] T3: rglactions handling.
- [ ] T4: hemisphere shift for both-hemi views.
- [ ] T5: tests + CI leg.

## Open points

### T1. Shared plain-R helpers (foundational)
Centralize geometry/style/color logic so both backends use one code base.

- [ ] `apply.transform(renderable, matrix)` (S3): rotate/translate a
      renderable's geometry in base R, rotating vertex normals too (rigid
      rotation) to match `rgl::rotate3d`. Route the rgl path
      (`vis.rotated.coloredmeshes()` in `R/vis_meshes.R`) and
      `handle.rglactions.highlight.points()` (`R/vis_multiview.R`) through it
      so the per-type `rotate3d` duplication disappears, with NO output change.
- [ ] Reuse the existing plain-R `shift.hemis.apart()` (`R/vis_multiview.R`)
      for the scimesh path (it is not rgl-specific).
- [ ] `apply.style.alpha(colors, style)`: set the RGBA A channel from the
      resolved style alpha. (Note the inherent asymmetry: rgl applies
      material `alpha`, scimesh uses per-vertex A.)

### T2. Style fidelity: alpha/transparency
- [ ] Use `apply.style.alpha()` in the scimesh path so `semitransparent`
      (alpha=0.5) and `glass` (alpha=0.4) render semi-transparent. `edges`
      already maps to `wireframe`.
- [ ] Fix `style = "from_mesh"`: resolve style per coloredmesh (alpha, etc.)
      instead of passing the literal string through.
- [ ] Document that the rgl `back="lines"` look is not reproduced in scimesh.
- [ ] Add rgl-vs-scimesh comparison figures for the transparent styles.

### T3. rglactions (plain R where possible)
- `clip_data` is renderer-independent (plain R) - keep it that way.
- `no_vis` does not apply to scimesh (it only suppresses opening an rgl
  window) - ignore it in the scimesh backend.
- Geometry actions go through T1 (plain R, identical for both backends).
- `snapshot_png` is the only renderer-bound action (write the frame):
  mirror the rgl branch's "ignored, use `output_img`" warning in the scimesh
  branch.
- `highlight_points`: support via `scimesh::render_spheres()`.
- Any other unsupported render-time action: explicit `warning()`/`stop()`.

### T4. Hemisphere shift for both-hemisphere views
- [ ] Apply the shared plain-R `shift.hemis.apart()` for
      `dorsal`/`ventral`/`rostral`/`caudal` in the scimesh path, matching
      rgl's shift. Opt-in only (via `shift_hemis_apart` rglactions), same as
      rgl - do not make it default.

### T5. Tests, CI, docs
- [ ] CI leg that installs scimesh from CRAN and runs a headless smoke test
      with `options(fsbrain.renderer_backend = "scimesh")` (no X11/xvfb).
- [ ] Unit tests for the plain-R helpers (`apply.transform`,
      `apply.style.alpha`, camera mapping, empty-scene handling).
- [ ] Keep this file and `TODO_FSBRAIN_RGL_CAM.md` up to date.

## Abandoned
- Dropping magick from the scimesh path: magick stays for composition for
  both backends. (Decision 2026-08-22.)
