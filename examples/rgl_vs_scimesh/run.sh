#!/usr/bin/env bash
#
# run.sh -- Run the rgl_vs_scimesh comparison example.
#
# Runs validate_rgl_vs_scimesh.R with the backend given by
# FSBRAIN_RENDERER_BACKEND (default: headless 'scimesh'). This exercises the
# important fsbrain visualization features and writes one PNG per feature.
#
# To also render with rgl for a side-by-side comparison on a machine with a
# display, run manually:
#   Rscript validate_rgl_vs_scimesh.R --backend rgl --outdir .
# and then compare the _rgl.png / _scimesh.png image sets (optionally build
# montages with '--montage').
#
# Exit code: 0 on success, 1 on failure.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/../common.sh"

fsbrain_require_rscript || exit 1
fsbrain_require_scimesh || exit 1
fsbrain_export_backend

SUBJECTS_DIR="$(fsbrain_resolve_subjects_dir)" || exit 1
echo "Using subjects_dir: ${SUBJECTS_DIR} (backend: $(fsbrain_backend))"

cd "${SCRIPT_DIR}" || exit 1

echo "Running validate_rgl_vs_scimesh.R (--backend $(fsbrain_backend))..."
if ! "${RSCRIPT}" validate_rgl_vs_scimesh.R --backend "$(fsbrain_backend)" --outdir .; then
    echo "ERROR: validate_rgl_vs_scimesh.R failed." >&2
    exit 1
fi

n_pngs=$(ls -1 ./*_scimesh.png 2>/dev/null | wc -l)
if [[ "${n_pngs}" -lt 1 ]]; then
    echo "ERROR: no *_scimesh.png output images were created." >&2
    exit 1
fi

echo "rgl_vs_scimesh: OK (wrote ${n_pngs} scimesh PNG(s))."
exit 0
