#!/usr/bin/env bash
#
# run.sh -- Run the medial_mask example.
#
# Generates the medial wall mask for the demo subject (subject1) and writes
# it as MGZ files. This example does not render any 3D scene, so it works
# with any renderer backend.
#
# Exit code: 0 on success, 1 on failure.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/../common.sh"

fsbrain_require_rscript || exit 1

SUBJECTS_DIR="$(fsbrain_resolve_subjects_dir)" || exit 1
SUBJECT="$(fsbrain_demo_subject)"
echo "Using subjects_dir: ${SUBJECTS_DIR} (subject: ${SUBJECT})"

cd "${SCRIPT_DIR}" || exit 1

out_lh="lh_mask_${SUBJECT}.mgz"
out_rh="rh_mask_${SUBJECT}.mgz"
echo "Running medial_mask.R ..."
if ! "${RSCRIPT}" medial_mask.R "${SUBJECTS_DIR}" "${SUBJECT}" "${out_lh}" "${out_rh}"; then
    echo "ERROR: medial_mask.R failed." >&2
    exit 1
fi

missing=0
for f in "${out_lh}" "${out_rh}"; do
    if [[ ! -f "${f}" ]]; then
        echo "ERROR: expected output file '${f}' was not created." >&2
        missing=1
    fi
done
if [[ ${missing} -eq 1 ]]; then
    exit 1
fi

echo "medial_mask: OK (wrote ${out_lh} and ${out_rh})."
exit 0
