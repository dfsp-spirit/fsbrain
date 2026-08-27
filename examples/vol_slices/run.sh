#!/usr/bin/env bash
#
# run.sh -- Run the vol_slices example.
#
# Exports enlarged MRI volume slices with surface contours and per-axis
# lightbox overview images for the demo subject (subject1). The slice images
# are rendered with magick, so no rgl/scimesh backend is needed.
#
# Exit code: 0 on success, 1 on failure.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/../common.sh"

fsbrain_require_rscript || exit 1

SUBJECTS_DIR="$(fsbrain_resolve_subjects_dir)" || exit 1
echo "Using subjects_dir: ${SUBJECTS_DIR}"

cd "${SCRIPT_DIR}" || exit 1

echo "Running vol_slices.R ..."
if ! "${RSCRIPT}" vol_slices.R; then
    echo "ERROR: vol_slices.R failed." >&2
    exit 1
fi

n_slices=$(ls -1 subject1_axis*_slice*.png 2>/dev/null | wc -l)
n_lightboxes=$(ls -1 subject1_lightbox_*.png 2>/dev/null | wc -l)
if [[ "${n_slices}" -lt 1 || "${n_lightboxes}" -lt 1 ]]; then
    echo "ERROR: expected slice images (found ${n_slices}) and lightbox images (found ${n_lightboxes})." >&2
    exit 1
fi

echo "vol_slices: OK (wrote ${n_slices} slice image(s) and ${n_lightboxes} lightbox image(s))."
exit 0
