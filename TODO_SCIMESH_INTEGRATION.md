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
      renderable's geometry in base R. Route the rgl path
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
      (alpha=0.5) and `glass` (alpha=0.4) render semi-transparent.
- [ ] Handle `style = "from_mesh"` per coloredmesh (alpha resolved per mesh).
- [ ] Add rgl-vs-scimesh comparison figures for the two transparent styles.

### T3. rglactions (plain R where possible)
- Data-level actions (`clip_data`, `no_vis`, colormap options) are already
  plain R and shared — nothing to do.
- Geometry actions go through T1 (plain R, identical for both backends).
- `snapshot_png` is the only renderer-bound action (write the frame):
  mirror the rgl branch's "ignored, use `output_img`" warning in the scimesh
  branch.
- Any unsupported render-time action: explicit `warning()`/`stop()`.

### T4. Hemisphere shift for both-hemisphere views
- [ ] Apply the shared plain-R `shift.hemis.apart()` for
      `dorsal`/`ventral`/`rostral`/`caudal` in the scimesh path, matching
      rgl's shift.

### T5. Tests, CI, docs
- [ ] CI leg that installs scimesh from CRAN and runs a headless smoke test
      with `options(fsbrain.renderer_backend = "scimesh")` (no X11/xvfb).
- [ ] Unit tests for the plain-R helpers (`apply.transform`,
      `apply.style.alpha`, camera mapping, empty-scene handling).
- [ ] Keep this file and `TODO_FSBRAIN_RGL_CAM.md` up to date.

## Abandoned
- Dropping magick from the scimesh path: magick stays for composition for
  both backends. (Decision 2026-08-22.)
