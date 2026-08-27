#!/usr/bin/env bash
#
# run_all_examples.sh -- Run all fsbrain example scripts and report a summary.
#
# Usage:
#   ./run_all_examples.sh              # run all examples
#   ./run_all_examples.sh <example>    # run only one example (e.g. 'facecheck')
#
# This script:
#   1. Ensures the fsbrain demo data is available: downloads the fsaverage
#      template and the optional demo subject once
#      (fsbrain::download_fsaverage + fsbrain::download_optional_data; both
#      are no-ops if the files are already cached).
#   2. Exports the resulting subjects_dir, subject id and renderer backend
#      via environment variables, so each examples/<example>/run.sh can use
#      them (each run.sh also resolves the data itself when run standalone).
#   3. Runs every examples/<example>/run.sh, collects the exit codes and
#      prints a summary: number of examples run, number of successes and
#      number of failures.
#
# Exit code: 0 if all examples succeeded, 1 if any of them failed (so it can
# be used as a CI step).
#
# Environment variables (all optional):
#   FSBRAIN_DEMO_SUBJECTS_DIR  subjects_dir to use instead of the demo cache.
#   FSBRAIN_DEMO_SUBJECT       subject id to use (default: 'subject1').
#   FSBRAIN_RENDERER_BACKEND   renderer backend for 3D rendering
#                              (default: 'scimesh'; use 'rgl' on a machine
#                              with a working display).

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

fsbrain_require_rscript || exit 1

cd "${SCRIPT_DIR}" || exit 1

# Optional: restrict the run to a single example directory given on the
# command line, e.g. './run_all_examples.sh facecheck'.
EXAMPLE="${1:-}"
runshs=()
if [[ -n "${EXAMPLE}" ]]; then
    EXAMPLE="${EXAMPLE%/}"
    if [[ ! -d "${EXAMPLE}" || ! -f "${EXAMPLE}/run.sh" ]]; then
        echo "ERROR: example directory '${EXAMPLE}' not found (expected '${EXAMPLE}/run.sh')." >&2
        echo "Available examples:" >&2
        for d in */run.sh; do
            [[ -f "${d}" ]] && echo "  - $(dirname "${d}")" >&2
        done
        exit 1
    fi
    runshs=("${EXAMPLE}/run.sh")
else
    runshs=(*/run.sh)
fi

echo "============================================================"
echo "fsbrain example runner"
echo "============================================================"
if [[ -n "${EXAMPLE}" ]]; then
    echo "Running only example: ${EXAMPLE}"
else
    echo "Running all examples"
fi

echo "Ensuring fsbrain demo data (fsaverage + optional demo subject)..."
SUBJECTS_DIR="$(fsbrain_ensure_demo_data)" || exit 1
export FSBRAIN_DEMO_SUBJECTS_DIR="${SUBJECTS_DIR}"
export FSBRAIN_DEMO_SUBJECT="${FSBRAIN_DEMO_SUBJECT:-subject1}"
fsbrain_export_backend
echo "  subjects_dir : ${FSBRAIN_DEMO_SUBJECTS_DIR}"
echo "  subject      : ${FSBRAIN_DEMO_SUBJECT}"
echo "  backend      : ${FSBRAIN_RENDERER_BACKEND}"
echo ""

total=0
success=0
failures=0
failed_examples=()

for runsh in "${runshs[@]}"; do
    if [[ ! -f "${runsh}" ]]; then
        continue
    fi
    example_dir="$(dirname "${runsh}")"
    total=$(( total + 1 ))
    echo ""
    echo "---- Running example: ${example_dir} ----"
    if [[ -x "${runsh}" ]]; then
        ( cd "${example_dir}" && ./run.sh )
    else
        echo "  (run.sh is not executable, invoking via 'bash run.sh')"
        ( cd "${example_dir}" && bash run.sh )
    fi
    rc=$?
    if [[ ${rc} -eq 0 ]]; then
        success=$(( success + 1 ))
        echo "---- ${example_dir}: OK ----"
    else
        failures=$(( failures + 1 ))
        failed_examples+=("${example_dir}")
        echo "---- ${example_dir}: FAILED (exit code ${rc}) ----"
    fi
done

echo ""
echo "============================================================"
echo "SUMMARY: total=${total}, success=${success}, failures=${failures}"
echo "============================================================"
if [[ ${failures} -gt 0 ]]; then
    echo "Failed examples:"
    for ex in "${failed_examples[@]}"; do
        echo "  - ${ex}"
    done
    exit 1
fi
exit 0
