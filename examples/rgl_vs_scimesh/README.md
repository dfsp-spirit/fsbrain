# rgl vs scimesh comparison scripts

Three scripts live here to compare the rgl and scimesh renderer backends. All
require the current (development) fsbrain to be installed, e.g.:

```bash
R CMD build .
R CMD INSTALL ./fsbrain_*.tar.gz
```

## 1. validate_rgl_vs_scimesh.R (recommended) -- high-level feature check

Faithful plain-R port of `web/Rmd_web_examples/fsbrain_with_scimesh.Rmd`.
Renders each important high-level feature (export() API variants, region- and
vertex-based results, manual mesh workflow) with ONE backend and writes
numbered PNGs `<NN>_<feature>_<backend>.png` into the current directory. Run
once per backend, then compare the two image sets in your OS viewer:

```bash
Rscript examples/rgl_vs_scimesh/validate_rgl_vs_scimesh.R --backend rgl
Rscript examples/rgl_vs_scimesh/validate_rgl_vs_scimesh.R --backend scimesh
```

Optionally build side-by-side montages (rgl top, scimesh bottom) from existing
`_rgl`/`_scimesh` pairs, saved as `<NN>_<feature>_SIDE.png`:

```bash
Rscript examples/rgl_vs_scimesh/validate_rgl_vs_scimesh.R --montage
```

Pass `--outdir <dir>` to write images somewhere other than the current
directory. Data (subject1 + fsaverage) is downloaded automatically if missing;
the rgl backend needs a display or Xvfb, scimesh is fully headless.

## 2. camera_orientation_compare.R -- per-view orientation/framing + cube debug

Renders all 8 anatomical views with both backends plus an optional
face-coloured cube (`--cube`) for orientation debugging, and writes side-by-side
montages to a dedicated output dir (default `~/fsbrain_camera_compare2`).

## 3. camera_verification.R -- legacy framing-only check

Older script; renders all 9 single views + a t4 composite for one measure
(`sulc`) with `_rgl.png` / `_scimesh.png` suffixes in the CWD. Superseded by
`validate_rgl_vs_scimesh.R` for feature coverage, still useful for a quick
framing-only check. Compare dimensions / perspective with ImageMagick, e.g.
`identify *_rgl.png *_scimesh.png`.