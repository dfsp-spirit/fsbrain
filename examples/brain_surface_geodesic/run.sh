#!/usr/bin/env bash
#
# run.sh -- Run the brain_surface_geodesic example.
#
# Computes geodesic distances on the fsaverage surface meshes and renders
# images with the renderer backend given by FSBRAIN_RENDERER_BACKEND
# (default: headless 'scimesh').
#
# Exit code: 0 on success, 1 on failure.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/../common.sh"

fsbrain_require_rscript || exit 1
fsbrain_require_scimesh || exit 1
fsbrain_export_backend

# Ensure the demo data is available (fsaverage) and resolve subjects_dir.
SUBJECTS_DIR="$(fsbrain_resolve_subjects_dir)" || exit 1
SUBJECT="$(fsbrain_demo_subject)"
echo "Using subjects_dir: ${SUBJECTS_DIR} (subject: ${SUBJECT}, backend: $(fsbrain_backend))"

cd "${SCRIPT_DIR}" || exit 1

echo "Running brain_surface_geodesic.R (--renderer $(fsbrain_backend) --vis)..."
if ! "${RSCRIPT}" brain_surface_geodesic.R --renderer "$(fsbrain_backend)" --vis; then
    echo "ERROR: brain_surface_geodesic.R failed." >&2
    exit 1
fi

expected_files=(highlighted_vertices.png dist_euclid.png dist_geodesic.png
                dist_inflated_euclid.png dist_inflated_geodesic.png
                lh.disteuclid lh.distgeod)
missing=0
for f in "${expected_files[@]}"; do
    if [[ ! -f "${f}" ]]; then
        echo "ERROR: expected output file '${f}' was not created." >&2
        missing=1
    fi
done
if [[ ${missing} -eq 1 ]]; then
    exit 1
fi

echo "brain_surface_geodesic: OK (wrote $(ls -1 *.png 2>/dev/null | wc -l) PNG(s) and 2 morph files)."
exit 0
