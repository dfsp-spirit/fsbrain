#!/usr/bin/env bash
#
# run.sh -- Run the facecheck example.
#
# Creates an image of the MRI volumes of the demo subject (subject1) to check
# that defacing (anonymization) worked. Renders with the renderer backend
# given by FSBRAIN_RENDERER_BACKEND (default: headless 'scimesh').
#
# Exit code: 0 on success, 1 on failure.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/../common.sh"

fsbrain_require_rscript || exit 1
fsbrain_require_scimesh || exit 1
fsbrain_export_backend

SUBJECTS_DIR="$(fsbrain_resolve_subjects_dir)" || exit 1
SUBJECT="$(fsbrain_demo_subject)"
echo "Using subjects_dir: ${SUBJECTS_DIR} (subject: ${SUBJECT}, backend: $(fsbrain_backend))"

cd "${SCRIPT_DIR}" || exit 1

out_img="facecheck_subject_${SUBJECT}.png"
echo "Running facecheck.R ..."
if ! "${RSCRIPT}" facecheck.R "${SUBJECTS_DIR}" "${SUBJECT}" "${out_img}"; then
    echo "ERROR: facecheck.R failed." >&2
    exit 1
fi

if [[ ! -f "${out_img}" ]]; then
    echo "ERROR: expected output image '${out_img}' was not created." >&2
    exit 1
fi

echo "facecheck: OK (wrote ${out_img})."
exit 0
