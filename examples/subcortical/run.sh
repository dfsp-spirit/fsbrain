#!/usr/bin/env bash
#
# run.sh -- Run the subcortical atlas example.
#
# The subcortical atlas (see 'dev_tools/subcortial/enigma_aseg/') is not part of the
# standard FreeSurfer distribution. The files for the fsaverage template subject are
# downloaded into the package cache by 'fsbrain::download_optional_data()' (which also
# calls 'fsbrain::download_fsaverage_atlases()'), together with the cortical surfaces
# of fsaverage (downloaded by 'fsbrain::download_fsaverage()'). Nothing is written into
# any user directory, the package cache is used.
#
# The region values are then visualized with 'subcortical.R': once for the structures
# on their own, and once for the structures inside a semi-transparent cortex. The two
# resulting images are written into this directory.
#
# Environment variables (all optional):
#   FSBRAIN_SUBCORTICAL_SUBJECT  subject id to visualize (default: 'fsaverage'). The
#                                subject must contain the atlas files and, for the
#                                cortex context, the cortical surfaces.
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

# This also downloads the subcortical atlas: common.sh calls download_optional_data(), which in
# turn ensures that the atlas files of the fsaverage template subject are available.
SUBJECTS_DIR="$(fsbrain_resolve_subjects_dir)" || exit 1
SUBJECT="${FSBRAIN_SUBCORTICAL_SUBJECT:-fsaverage}"
OUTPUT_IMG="subcortical_region_values.png"
OUTPUT_IMG_CTX="subcortical_region_values_in_cortex.png"

echo "Using subjects_dir: ${SUBJECTS_DIR} (subject: ${SUBJECT}, backend: $(fsbrain_backend))"

cd "${SCRIPT_DIR}" || exit 1

echo "Running subcortical.R ..."
if ! "${RSCRIPT}" subcortical.R "${SUBJECTS_DIR}" "${SUBJECT}" . --renderer "$(fsbrain_backend)"; then
    echo "ERROR: subcortical.R failed." >&2
    exit 1
fi

missing=0
for f in "${OUTPUT_IMG}" "${OUTPUT_IMG_CTX}"; do
    if [[ ! -f "${f}" ]]; then
        echo "ERROR: expected output image '${f}' was not created." >&2
        missing=1
    fi
done
if [[ ${missing} -eq 1 ]]; then
    exit 1
fi

echo "subcortical: OK (wrote ${OUTPUT_IMG} and ${OUTPUT_IMG_CTX})."
exit 0
