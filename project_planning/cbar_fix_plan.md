# Plan: Fix the fsbrain colorbar (missing / white in live plots)

Status: draft — capturing discussion from 2026-08-06, to be fleshed out properly later.
Scope: fsbrain only (colorbar handling). Not the scimesh renderer integration (that lives on its own branch).

## 1. Problem statement

- **Live / interactive rgl plots** (`vis.*` / `brainview.*` with `draw_colorbar=TRUE`):
  the colorbar is **silently missing or renders as a white/blank region** when the
  rgl window (i.e., the monitor resolution) is too small.
- Users have been told to "use a larger monitor" — unacceptable.
- The `export()` API is **not** affected (see §2), because it already uses the
  standalone colorbar path.

## 2. The two existing colorbar paths

### Path A — in-scene live colorbar (the broken one)
- Entry points: `draw_colorbar` param in `vis.coloredmeshes` (`R/vis_meshes.R`),
  `brainview.t4` / `brainview.t9` (`R/vis_multiview.R`), `vis.rotated.coloredmeshes`.
- Mechanism: `rgl::layout3d(...)` reserves a subviewport (e.g. right column, width
  ratio 1/10 in `t9`), then `draw.colorbar()` (`R/cbar.R`) calls
  `rgl::bgplot3d({ ... fields::image.plot(legend.only=TRUE, ...) })`.
- **Why it breaks:** `bgplot3d` renders a **bitmap** (PNG) of the expression at the
  **subviewport pixel size** and uses it as a background texture. Small window → tiny
  subviewport → `image.plot` produces nothing legible. Additionally, errors raised
  inside `bgplot3d` are **not surfaced** → blank background instead of a colorbar
  (the "white image"). Confirmed in rgl docs: background plots are bitmaps and
  "do not resize very gracefully."

### Path B — standalone colorbar + compositing (used by `export()`, works fine)
- `export()` / `vis.export.from.coloredmeshes()` (`R/brainview_magick.R`):
  1. Render brainview tiles via rgl → arrange into grid PNG with magick
     (`vislayout.from.coloredmeshes`, `arrange.brainview.images`).
  2. Render the colorbar **separately** into its own fixed-size device
     (`coloredmesh.plot.colorbar.separate`, `R/cbar.R`, default 1400×1400 `png()`).
  3. Composite colorbar PNG onto brainview PNG
     (`combine.colorbar.with.brainview.image`, `R/cbar_magick.R`).
- The colorbar here is resolution-independent of the monitor → no "missing" bug.

## 3. Goals / non-goals

Goals:
- Live plots must not fail **silently** (warn/skip gracefully when too small).
- Keep live plots **instant** (no heavy rendering added to the interactive path).
- Provide a clean seam for a future proper colorbar renderer.

Non-goals (for now):
- Making the live colorbar pixel-perfect (it is interactive-only, not for
  publication).
- Fixing the screen-resolution cap on the **rgl brainview tiles** in `export()`
  (separate issue — the `windowRect = 1000*quality` tiles are capped by screen
  resolution; real fix is a software renderer like scimesh).

## 4. Phased changes

### Phase 1 — make the live failure non-silent (cheap, ship-worthy, ~minutes)
Touch: `R/cbar.R` (`draw.colorbar`), maybe `R/vis_meshes.R` / `R/vis_multiview.R`.
- Wrap the `bgplot3d` call in `tryCatch`; on error emit a message instead of a
  blank background.
- Before drawing, check the current subviewport size (`rgl::par3d("viewport")`);
  if below a minimum threshold, `message()` a clear hint
  ("colorbar skipped: window too small, resize to show") and return.
- Optionally reduce the legend font for the live path (the export path's large
  `cex` values are tuned for 1400 px and are absurd in an 80 px cell).

### Phase 2 — make the live colorbar reliably visible (small-moderate, optional)
- Pre-render the colorbar once at fixed resolution (reuse the standalone renderer)
  and attach with `rgl::bg3d(texture=...)` into the colorbar subviewport, or
  bump `bgplot3d(magnify=...)`. Slightly soft when enlarged; fine and instant for
  interactive use.

### Phase 3 — proper standalone renderer (deferred, long-term)
- Replace the `fields::image.plot`-based `coloredmesh.plot.colorbar.separate` with
  a grid-based standalone colorbar renderer (grid + `scales` + `farver` + `scico`/
  `viridisLite`, output via `svglite`/`ragg`). This is the seam the new drawer
  plugs into; `combine.colorbar.with.brainview.image` stays as the compositor.
- Feature targets (from the scibar feature set): linear/log/diverging/categorical
  scales, log + reversed axes, colormap reversal, H/V orientation, smart ticks +
  minor ticks, themes, SVG + raster output, custom fonts.
- Decision recorded: a pure-R implementation is preferred over an Rcpp wrapper
  around scibar for the R side; wrapper only if bit-exact parity with C++
  scimesh/scibar output ever becomes a hard requirement.
- **Live path long-term:** once the scimesh renderer branch lands (waiting on CRAN),
  live views get scibar-based colorbars (crisp, resolution-independent), so
  `bgplot3d` becomes legacy default only — avoid over-investing in it.

### Why this matters (opportunity map)
fsbrain's current colorbar is base-graphics (`fields::image.plot`, device-drawn, no
retained object) and split across two paths (in-scene `bgplot3d` + separate PNG)
that can drift apart. A single grid-based colorbar grob fixes all of these:

| Improvement | Pain point it kills |
|---|---|
| Produce a **grob** (`colorbarGrob()`), not a device draw | Issue #51 — users can extract/compose/re-arrange it, work with `patchwork`/`plot_grid` |
| **One renderer** for live and `export()` paths | Two implementations no longer need to stay in sync |
| **Resolution-independent** (re-render grob at any device size) | "Use a larger monitor" problem, both paths |
| **Feature set** (log/diverging/categorical, axis & colormap reversal, minor ticks, themes, SVG+raster) | Parity with a modern colorbar / scibar feature list |
| **Fixed-res texture** for the live path (`bg3d(texture=...)` from the grob) | Issue #37 (M1 `bgplot3d` narrow-viewport bug) + small-window "white colorbar" |
| **Drop the `fields` dependency** | One less heavyweight transitive dep (grid + `scales` + `farver` cover it) |

Net: #37, #51, and the "monitor too small" saga all trace back to the same
architectural choice; one replacement fixes all of them and unlocks the features.
Pairs cleanly with the scimesh branch: scimesh renders the mesh, this handles the
colorbar; both compose.

## 5. File / function touch points

| File | Function | Role |
|---|---|---|
| `R/cbar.R` | `draw.colorbar` | live in-scene colorbar (bgplot3d) — Phase 1 |
| `R/cbar.R` | `coloredmesh.plot.colorbar.separate` | standalone colorbar — Phase 3 seam |
| `R/vis_meshes.R` | `vis.coloredmeshes` | live layout (draw_colorbar) |
| `R/vis_multiview.R` | `brainview.t4`, `brainview.t9`, ... | live layout matrices |
| `R/brainview_magick.R` | `export`, `vis.export.from.coloredmeshes` | standalone path wiring |
| `R/cbar_magick.R` | `combine.colorbar.with.brainview.image` | compositor (unchanged) |

## 6. Decision log — do NOT wrap scibar for R (2026-08-06)

To prevent re-litigating this: a thin Rcpp wrapper around the C++ scibar library
was considered as a way to get scibar's features in R. **Rejected.** Recorded here
so future-me does not revisit it.

Reasons:
1. **Interface mismatch (decisive).** scibar is an *exporter* ("pick a filename,
   write now") that hands back pixel buffers (PNG/TGA/PPM) or a standalone SVG
   file. R's plotting model is *object retention*: build a grob (the spec), then
   re-render to any device/size later. A wrapper produces no standard-R object
   users can postprocess (no grob/ggplot, no compose with `patchwork`/`cowplot`) —
   the exact thing fsbrain users ask for (see issue #51).
2. **Pure R already has the features.** `grid` + `scales` (log/diverging breaks,
   reverse axis, label formatting) + `farver`/`scico` (vik/viridis, interpolation)
   + `svglite`/`ragg` (SVG/raster) cover the scibar feature list, and the result
   is a composable grid object.
3. **R-native output is better integrated.** Fonts via `systemfonts`, gradients via
   grid, devices via grDevices — things a wrapped C++ engine would have to
   re-solve, with device/OS variance.
4. **scibar's differentiators don't translate.** Zero-dependency, embedded font,
   PPM/TGA are C++-ecosystem concerns that don't exist inside R.

The only case for a wrapper ever: **bit-exact parity** between C++ scimesh/scibar
output and R output, if that ever becomes a hard requirement. It's technically
cheap to add later (scibar writes files itself), so there is no lock-in — build the
pure-R version now, wrap only if/when parity is required.

(Companion discussion: standalone grid colorbar as a separate micro-package vs
inside fsbrain — see Open Questions.)

## 7. Open questions

- Minimum colorbar subviewport size threshold for Phase 1?
- Should Phase 2 (texture/magnify) be done at all, or is Phase 1 + scimesh enough?
- Where should the standalone renderer live: new small package vs. inside fsbrain?
  (Earlier discussion leans toward a standalone micro-package; revisit when scimesh
  CRAN status is settled.)
