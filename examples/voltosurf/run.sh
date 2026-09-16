#!/usr/bin/env bash
#
# run.sh -- Run the 'voltosurf' example.
#
# Projects volume data onto cortical surfaces with 'subject.vol2surf()' and
# 'template.vol2surf()':
#
#   1) The 'brain' volume of the demo subject is projected onto its own white
#      surface, and onto the mid-cortical surface between white and pial. Both
#      images are rendered on the inflated surface.
#   2) A synthetic group-level statistical map in MNI152 space (created by the
#      R script) is projected onto the HCP-style fs_LR 32k midthickness surface,
#      thresholded, and rendered with the medial wall masked.
#
# The images (voltosurf_subject_white.png, voltosurf_subject_midcortex.png,
# voltosurf_fsLR32_statmap.png) are written into this directory, together with
# the synthetic stat map (demo_stat_map_mni152.mgz).
#
# Data: the demo subject (subject1) and the fsaverage template are downloaded
# into the package cache by the shared example helpers, and the fs_LR 32k
# template meshes and labels are downloaded by the R script. The download
# functions do nothing if the files are already cached.
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
OUTPUT_IMAGES=("voltosurf_subject_white.png" "voltosurf_subject_midcortex.png" "voltosurf_fsLR32_statmap.png")

echo "Using subjects_dir: ${SUBJECTS_DIR} (backend: $(fsbrain_backend))"

cd "${SCRIPT_DIR}" || exit 1

echo "Running voltosurf.R ..."
if ! "${RSCRIPT}" voltosurf.R "${SUBJECTS_DIR}" . --renderer "$(fsbrain_backend)" "$@"; then
    echo "ERROR: voltosurf.R failed." >&2
    exit 1
fi

missing=0
for f in "${OUTPUT_IMAGES[@]}"; do
    if [[ ! -s "${f}" ]]; then
        echo "ERROR: expected output image '${f}' is missing or empty." >&2
        missing=1
    fi
done
if [[ ${missing} -eq 1 ]]; then
    exit 1
fi

echo "voltosurf: OK (wrote ${OUTPUT_IMAGES[*]})."
exit 0
