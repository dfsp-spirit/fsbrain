#!/usr/bin/env bash
#
# run.sh -- Run the white matter tracts example.
#
# Visualizes the XTRACT atlas of the major white matter tracts on the fs_LR_32
# (HCP-style) template surface with 'fsbrain::vis.tracts()': once colored by a
# value per bundle (with a colorbar), once colored by the direction of the
# streamline segments (the classic DTI look), and once without the
# semi-transparent brain surface. The resulting images (tracts_bundles.png,
# tracts_orientation.png, tracts_no_context.png) are written into this directory.
#
# The XTRACT tract atlas and the fs_LR_32 template meshes are downloaded into the
# package cache by 'fsbrain::download_xtract_tracts()' and
# 'fsbrain::download_fs_LR_32_meshes()' (called by the R script). They are not
# part of FreeSurfer and are not subject to the FreeSurfer license.
#
# Environment variables (all optional):
#   FSBRAIN_RENDERER_BACKEND    renderer backend (default: 'scimesh'; use 'rgl' on a
#                               machine with a working display).
#   FSBRAIN_TRACTS_ATLAS        the tract atlas to use (default: 'xtract_tiny').
#   FSBRAIN_TRACTS_TCK          a whole-brain tractogram in TCK format to draw in
#                               addition (default: none). The last test data of the
#                               fsbrain CI is not a tractogram, so this is opt-in.
#   FSBRAIN_TRACTS_MAX_TRACKS   number of streamlines to read from the tractogram
#                               (default: 20000).
#
# Exit code: 0 on success, 1 on failure.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/../common.sh"

fsbrain_require_rscript || exit 1
fsbrain_require_scimesh || exit 1
fsbrain_export_backend

ATLAS="${FSBRAIN_TRACTS_ATLAS:-xtract_tiny}"
MAX_TRACKS="${FSBRAIN_TRACTS_MAX_TRACKS:-20000}"

EXTRA_ARGS=()
if [[ -n "${FSBRAIN_TRACTS_TCK:-}" ]]; then
    EXTRA_ARGS=(--tck "${FSBRAIN_TRACTS_TCK}" --max-tracks "${MAX_TRACKS}")
fi

OUTPUT_IMGS=("tracts_bundles.png" "tracts_orientation.png" "tracts_no_context.png")
if [[ -n "${FSBRAIN_TRACTS_TCK:-}" ]]; then
    OUTPUT_IMGS+=("tractogram.png")
fi

echo "Using tract atlas: ${ATLAS} (backend: $(fsbrain_backend))"

cd "${SCRIPT_DIR}" || exit 1

echo "Running tracts.R ..."
if ! "${RSCRIPT}" tracts.R . --renderer "$(fsbrain_backend)" --atlas "${ATLAS}" "${EXTRA_ARGS[@]}"; then
    echo "ERROR: tracts.R failed." >&2
    exit 1
fi

missing=0
for f in "${OUTPUT_IMGS[@]}"; do
    if [[ ! -f "${f}" ]]; then
        echo "ERROR: expected output image '${f}' was not created." >&2
        missing=1
    fi
done
if [[ ${missing} -eq 1 ]]; then
    exit 1
fi

echo "tracts: OK (wrote ${OUTPUT_IMGS[*]})."
exit 0
