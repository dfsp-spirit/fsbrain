#!/usr/bin/env bash
#
# run.sh -- Run the connectome example.
#
# Visualizes a synthetic connectivity matrix between the 400 regions of the Schaefer-400
# atlas on the fs_LR_32 (HCP-style) template surface with 'fsbrain::vis.connectome()':
# once in the 4 standard views, once with a colorbar for the edge weights, and once without
# the semi-transparent cortex (the 'spring layout' style). The resulting images
# (connectome_views.png, connectome_edge_colorbar.png, connectome_no_context.png) are
# written into this directory.
#
# The fs_LR_32 template meshes and atlases are downloaded into the package cache by
# 'fsbrain::download_fs_LR_32_meshes()' and 'fsbrain::download_fs_LR_32_atlases()' (called
# by the R script). They are not part of FreeSurfer and are not subject to the FreeSurfer
# license. Nothing is written into any user directory, except for the images of this example.
#
# Environment variables (all optional):
#   FSBRAIN_RENDERER_BACKEND     renderer backend (default: 'scimesh'; use 'rgl' on a
#                                machine with a working display).
#   FSBRAIN_CONNECTOME_ATLAS     the atlas to use (default: 'schaefer400').
#   FSBRAIN_CONNECTOME_MATRIX    a CSV file with a connectivity matrix (default: synthetic).
#
# Exit code: 0 on success, 1 on failure.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/../common.sh"

fsbrain_require_rscript || exit 1
fsbrain_require_scimesh || exit 1
fsbrain_export_backend

ATLAS="${FSBRAIN_CONNECTOME_ATLAS:-schaefer400}"
MATRIX_ARGS=()
if [[ -n "${FSBRAIN_CONNECTOME_MATRIX:-}" ]]; then
    MATRIX_ARGS=(--matrix "${FSBRAIN_CONNECTOME_MATRIX}")
fi

OUTPUT_IMGS=("connectome_views.png" "connectome_edge_colorbar.png" "connectome_no_context.png")

echo "Using atlas: ${ATLAS} (backend: $(fsbrain_backend))"

cd "${SCRIPT_DIR}" || exit 1

echo "Running connectome.R ..."
if ! "${RSCRIPT}" connectome.R . --renderer "$(fsbrain_backend)" --atlas "${ATLAS}" "${MATRIX_ARGS[@]}"; then
    echo "ERROR: connectome.R failed." >&2
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

echo "connectome: OK (wrote ${OUTPUT_IMGS[*]})."
exit 0
