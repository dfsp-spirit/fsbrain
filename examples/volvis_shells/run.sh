#!/usr/bin/env bash
#
# run.sh -- Run the volvis_shells example.
#
# Renders the T1 volume of the demo subject as nested, semi-transparent iso-surface
# shells, once in the 4 standard views and once cut open from the right, and writes
# the resulting images (shells_views.png, shells_cut_views.png) into this directory.
#
# Uses the renderer backend selected by fsbrain (rgl by default). Set the environment
# variable FSBRAIN_RENDERER_BACKEND=scimesh to render with the headless scimesh backend.
#
# Exit code: 0 on success, 1 on failure.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/../common.sh"

fsbrain_require_rscript || exit 1

SUBJECTS_DIR="$(fsbrain_resolve_subjects_dir)" || exit 1
echo "Using subjects_dir: ${SUBJECTS_DIR}"
export FSBRAIN_DEMO_SUBJECTS_DIR="${SUBJECTS_DIR}"

cd "${SCRIPT_DIR}" || exit 1

echo "Running volvis_shells.R ..."
if ! "${RSCRIPT}" volvis_shells.R "$@"; then
    echo "ERROR: volvis_shells.R failed." >&2
    exit 1
fi

for img in shells_views.png shells_cut_views.png shells_sequential_views.png; do
    if [[ ! -s "${img}" ]]; then
        echo "ERROR: expected output image '${img}' is missing or empty." >&2
        exit 1
    fi
done

echo "volvis_shells: OK (wrote shells_views.png, shells_cut_views.png and shells_sequential_views.png)."
exit 0
