#!/usr/bin/env bash
#
# run.sh -- Run the volume_clusters example.
#
# Visualizes the clusters of a synthetic statistical map (three Gaussian blobs, two of them
# positive and one negative, in the grid of a FreeSurfer conformed volume) with
# 'fsbrain::vis.volume.clusters()': once as nested, semi-transparent iso-surface shells inside the
# semi-transparent cortex of the fsaverage template subject, once with a single opaque shell per
# cluster, and once without any context mesh. The resulting images
# (volume_clusters_views.png, volume_clusters_single_shell.png, volume_clusters_no_context.png)
# are written into this directory.
#
# The cortical surfaces of the fsaverage template subject are downloaded into the package cache
# by 'fsbrain::download_fsaverage()' (see common.sh). Nothing is written into any user directory,
# except for the images of this example.
#
# Environment variables (all optional):
#   FSBRAIN_DEMO_SUBJECTS_DIR    subjects_dir to use instead of the fsbrain demo one.
#   FSBRAIN_RENDERER_BACKEND     renderer backend (default: 'scimesh'; use 'rgl' on a
#                                machine with a working display).
#
# Exit code: 0 on success, 1 on failure.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/../common.sh"

fsbrain_require_rscript || exit 1
fsbrain_require_scimesh || exit 1
fsbrain_export_backend

SUBJECTS_DIR="$(fsbrain_resolve_subjects_dir)" || exit 1

OUTPUT_IMG="volume_clusters_views.png"
OUTPUT_IMG_SINGLE="volume_clusters_single_shell.png"
OUTPUT_IMG_NOCTX="volume_clusters_no_context.png"

echo "Using subjects_dir: ${SUBJECTS_DIR} (backend: $(fsbrain_backend))"

cd "${SCRIPT_DIR}" || exit 1

echo "Running volume_clusters.R ..."
if ! "${RSCRIPT}" volume_clusters.R "${SUBJECTS_DIR}" . --renderer "$(fsbrain_backend)"; then
    echo "ERROR: volume_clusters.R failed." >&2
    exit 1
fi

missing=0
for f in "${OUTPUT_IMG}" "${OUTPUT_IMG_SINGLE}" "${OUTPUT_IMG_NOCTX}"; do
    if [[ ! -f "${f}" ]]; then
        echo "ERROR: expected output image '${f}' was not created." >&2
        missing=1
    fi
done
if [[ ${missing} -eq 1 ]]; then
    exit 1
fi

echo "volume_clusters: OK (wrote ${OUTPUT_IMG}, ${OUTPUT_IMG_SINGLE} and ${OUTPUT_IMG_NOCTX})."
exit 0
