#!/usr/bin/env bash
#
# run.sh -- Run the subcortical atlas example.
#
# The subcortical atlas (see 'dev_tools/subcortial/enigma_aseg/') is not part of the
# standard FreeSurfer distribution, so it has to be installed into the subject before
# it can be used by fsbrain. This script does that for you by copying the atlas files
# from the fsbrain source tree into the subjects_dir:
#
#   dev_tools/subcortial/enigma_aseg/lh.subcortical        -> <subjects_dir>/<subject>/surf/lh.subcortical
#   dev_tools/subcortial/enigma_aseg/rh.subcortical        -> <subjects_dir>/<subject>/surf/rh.subcortical
#   dev_tools/subcortial/enigma_aseg/lh.subcortical.annot  -> <subjects_dir>/<subject>/label/lh.subcortical.annot
#   dev_tools/subcortial/enigma_aseg/rh.subcortical.annot  -> <subjects_dir>/<subject>/label/rh.subcortical.annot
#
# The files are only copied if they are not present yet, so the script can be run
# repeatedly. If they are missing in the source tree (e.g., after a fresh checkout
# without the generated files), they are re-created by running the conversion script
# 'dev_tools/subcortial/enigma_aseg/convert_to_fs.R'. Note that this conversion
# requires a FreeSurfer installation, as the colors of the structures are read from
# '$FREESURFER_HOME/FreeSurferColorLUT.txt'.
#
# The values are then visualized with 'subcortical.R' and the resulting image is
# written into this directory.
#
# Environment variables (all optional):
#   FSBRAIN_SUBCORTICAL_SUBJECT  subject id to install the atlas into and to visualize
#                                (default: 'fsaverage'). The subject has to exist in
#                                the subjects_dir (it does not have to be a cortical
#                                subject, the atlas is independent of the cortical
#                                surfaces).
#   FSBRAIN_DEMO_SUBJECTS_DIR    subjects_dir to use instead of the fsbrain demo one.
#   FSBRAIN_RENDERER_BACKEND     renderer backend (default: 'scimesh'; use 'rgl' on a
#                                machine with a working display).
#
# Exit code: 0 on success, 1 on failure.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/../common.sh"

REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
ATLAS_SRC_DIR="${REPO_ROOT}/dev_tools/subcortial/enigma_aseg"

fsbrain_require_rscript || exit 1
fsbrain_require_scimesh || exit 1
fsbrain_export_backend

SUBJECTS_DIR="$(fsbrain_resolve_subjects_dir)" || exit 1
SUBJECT="${FSBRAIN_SUBCORTICAL_SUBJECT:-fsaverage}"
OUTPUT_IMG="subcortical_region_values.png"

echo "Using subjects_dir: ${SUBJECTS_DIR} (subject: ${SUBJECT}, backend: $(fsbrain_backend))"

# Make sure the converted atlas files are available in the fsbrain source tree.
ensure_source_atlas_files() {
    local expected=("lh.subcortical" "rh.subcortical" "lh.subcortical.annot" "rh.subcortical.annot")
    local f missing=0
    for f in "${expected[@]}"; do
        [[ -f "${ATLAS_SRC_DIR}/${f}" ]] || missing=1
    done
    if [[ ${missing} -eq 0 ]]; then
        return 0
    fi

    echo "Atlas files missing in '${ATLAS_SRC_DIR}', running 'convert_to_fs.R' (requires FreeSurfer)..."
    if ! ( cd "${ATLAS_SRC_DIR}" && "${RSCRIPT}" convert_to_fs.R ); then
        echo "ERROR: could not create the FreeSurfer version of the subcortical atlas." >&2
        echo "       Please run 'dev_tools/subcortial/enigma_aseg/convert_to_fs.R' manually" >&2
        echo "       and copy the resulting files into '${SUBJECTS_DIR}/${SUBJECT}/surf/'" >&2
        echo "       and '${SUBJECTS_DIR}/${SUBJECT}/label/'." >&2
        return 1
    fi
    return 0
}

# Copy the subcortical atlas (surface mesh and annotation) into the given subject directory.
# Existing files are left alone, so the script can be run repeatedly.
install_atlas_for_subject() {
    local sdir="$1" subject="$2" f
    if ! mkdir -p "${sdir}/${subject}/surf" "${sdir}/${subject}/label"; then
        echo "ERROR: could not create the directories for subject '${subject}' in '${sdir}'." >&2
        return 1
    fi
    for f in lh.subcortical rh.subcortical; do
        if [[ -f "${sdir}/${subject}/surf/${f}" ]]; then
            echo "  already installed: ${sdir}/${subject}/surf/${f}"
        else
            cp "${ATLAS_SRC_DIR}/${f}" "${sdir}/${subject}/surf/${f}" || return 1
            echo "  installed: ${sdir}/${subject}/surf/${f}"
        fi
    done
    for f in lh.subcortical.annot rh.subcortical.annot; do
        if [[ -f "${sdir}/${subject}/label/${f}" ]]; then
            echo "  already installed: ${sdir}/${subject}/label/${f}"
        else
            cp "${ATLAS_SRC_DIR}/${f}" "${sdir}/${subject}/label/${f}" || return 1
            echo "  installed: ${sdir}/${subject}/label/${f}"
        fi
    done
    return 0
}

ensure_source_atlas_files || exit 1
echo "Installing the subcortical atlas into subject '${SUBJECT}'..."
install_atlas_for_subject "${SUBJECTS_DIR}" "${SUBJECT}" || exit 1

cd "${SCRIPT_DIR}" || exit 1

echo "Running subcortical.R ..."
if ! "${RSCRIPT}" subcortical.R "${SUBJECTS_DIR}" "${SUBJECT}" "${OUTPUT_IMG}" --renderer "$(fsbrain_backend)"; then
    echo "ERROR: subcortical.R failed." >&2
    exit 1
fi

if [[ ! -f "${OUTPUT_IMG}" ]]; then
    echo "ERROR: expected output image '${OUTPUT_IMG}' was not created." >&2
    exit 1
fi

echo "subcortical: OK (wrote ${OUTPUT_IMG})."
exit 0
