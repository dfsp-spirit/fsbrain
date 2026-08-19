# TODO: scimesh renderer backend integration (fsbrain)

Status of the `feature/scimesh-backend` branch and open points for the next
working session. Relevant code: `R/scimesh_bridge.R` and the scimesh branch in
`R/brainview_magick.R` (`vislayout.from.coloredmeshes()`). Backend switch:
`options(fsbrain.renderer_backend = "scimesh")`, read via
`get.fsbrain.renderer.backend()`.

---

## Open points

### 1. Drop the magick dependency for merge/crop in the scimesh path
The scimesh branch in `vislayout.from.coloredmeshes()` still requires the
`magick` package to merge/crop the per-view PNGs (and to create blank
placeholders for empty views). More recent scimesh versions have their own
image merge / compose / layout functions (`compose_layout`, `stack_horizontal`,
`stack_vertical`, image crop/scale, ...), so this fsbrain branch simply
pre-dates that functionality.

- [ ] Update the fsbrain scimesh branch to use scimesh's own
      merge/compose/crop for the scimesh backend instead of
      `arrange.brainview.images()` / magick.
- [ ] After that, remove the `magick` hard requirement from the scimesh path
      (keep it only for the rgl branch).
- [ ] Re-verify `vislayout.from.coloredmeshes()` output still matches the rgl
      layout (grid-like arrangement, cropping to content).

### 2. (Status note — NOT an open point) scimesh CRAN submission
scimesh is being submitted to CRAN soon (R CMD check already passes). Once it
is on CRAN, remove any GitHub install workaround from README/CI/docs.

### 3. Style fidelity: alpha/transparency not passed through
`fsbrain_style_to_scimesh_options()` maps `front`/`back`, backface culling,
specular, shininess, and wireframe, but `alpha` only triggers
`shading = "smooth"`. Per-mesh transparency from the `semitransparent` /
`glass` styles is NOT carried into the vertex/face alpha channel.

- [ ] Pass mesh alpha into the scimesh colors (RGBA A channel) for the
      transparent styles.
- [ ] Add an rgl-vs-scimesh comparison figure for `semitransparent` and
      `glass` styles.

### 4. rglactions not consumed in the scimesh branch
`vislayout.from.coloredmeshes()`'s scimesh branch ignores `rglactions`
entirely (the rgl branch at least warns about `snapshot_png`). Data-level
actions like `clip_data` are applied earlier, when the coloredmeshes are
built, so those still work — but render-time rgl actions do not transfer.

- [ ] Decide which `rglactions` are meaningful for the scimesh backend and
      either handle them or explicitly warn/error (e.g. `snapshot_png`).

### 5. No hemisphere shift for both-hemisphere views
Medial/lateral views correctly filter to the target hemisphere via
`hemi_filter`, but the scimesh path has no equivalent of rgl's
`shift.hemis.apart` — in `dorsal`/`ventral`/`rostral`/`caudal` views the two
hemispheres are rendered in place (overlapping at the midline). Cosmetic.

- [ ] Optionally add a hemisphere-shift transform for both-hemisphere views,
      or document that scimesh output differs slightly from rgl here.
