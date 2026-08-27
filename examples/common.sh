#!/usr/bin/env bash
#
# common.sh -- Shared helpers for the fsbrain example runner scripts.
#
# This file is meant to be *sourced* (not executed) by
#   * examples/run_all_examples.sh   (the top-level runner)
#   * each examples/<example>/run.sh (the per-example runner)
#
# It provides helpers for locating R, verifying the headless 'scimesh'
# renderer backend, and resolving the fsbrain demo FreeSurfer data
# (subjects_dir / subject) that several examples need.
#
# Data strategy
# =============
# Examples that need real FreeSurfer subject data (a subjects_dir and a
# subject id) use the demo data shipped with fsbrain:
#   * run_all_examples.sh downloads it once via
#         fsbrain::download_fsaverage(accept_freesurfer_license = TRUE)
#         fsbrain::download_optional_data()
#     and exports the resulting subjects_dir via the environment variables
#     FSBRAIN_DEMO_SUBJECTS_DIR and FSBRAIN_DEMO_SUBJECT.
#   * Individual run.sh scripts use FSBRAIN_DEMO_SUBJECTS_DIR if it is set,
#     and otherwise resolve (and, if needed, download) the data themselves,
#     so that they also work standalone.
# The download functions are no-ops once the files are cached, so running
# them on every invocation is cheap and safe for CI.
#
# Renderer backend
# =================
# By default the runner forces the headless 'scimesh' backend so the examples
# can run on machines without a display (CI, servers, ...). Override with the
# environment variable FSBRAIN_RENDERER_BACKEND, e.g. to 'rgl' on a
# workstation with a working display.

set -u

if [[ -z "${RSCRIPT:-}" ]]; then
    RSCRIPT="$(command -v Rscript 2>/dev/null || true)"
fi

# The renderer backend to use for this run (default: 'scimesh').
fsbrain_backend() {
    echo "${FSBRAIN_RENDERER_BACKEND:-scimesh}"
}

# Exit with an error if the Rscript executable cannot be found.
fsbrain_require_rscript() {
    if [[ -z "${RSCRIPT}" ]]; then
        echo "ERROR: Rscript not found on PATH." >&2
        return 1
    fi
}

# Export the renderer backend to use for this run (default: 'scimesh').
fsbrain_export_backend() {
    export FSBRAIN_RENDERER_BACKEND="$(fsbrain_backend)"
}

# Make sure the renderer backend can be used. Fails if 'scimesh' is selected
# but the scimesh package is not installed, or if an unknown backend is given.
# ('rgl' is only checked at render time, since it needs a display.)
fsbrain_require_scimesh() {
    local backend
    backend="$(fsbrain_backend)"
    if [[ "${backend}" != "scimesh" ]]; then
        if [[ "${backend}" != "rgl" ]]; then
            echo "ERROR: unknown renderer backend '${backend}' (supported: 'scimesh', 'rgl')." >&2
            return 1
        fi
        return 0
    fi
    if ! "${RSCRIPT}" -e 'if(! requireNamespace("scimesh", quietly = TRUE)) quit(status = 1);' >/dev/null 2>&1; then
        echo "ERROR: the scimesh R package is required for headless rendering but is not installed." >&2
        echo "       Install it with: install.packages('scimesh')" >&2
        return 1
    fi
}

# Download (if needed) the fsbrain demo data (fsaverage + subject1) and print
# the path to the subjects_dir on stdout. Returns non-zero on failure.
fsbrain_ensure_demo_data() {
    local tmpfile sdir
    tmpfile="$(mktemp)" || return 1
    if ! "${RSCRIPT}" -e '
        suppressPackageStartupMessages(library(fsbrain));
        fsbrain::download_fsaverage(accept_freesurfer_license = TRUE);
        fsbrain::download_optional_data();
        writeLines(fsbrain::get_optional_data_filepath("subjects_dir"), con = commandArgs(TRUE)[1]);
    ' "${tmpfile}" >/dev/null 2>&1; then
        rm -f "${tmpfile}"
        echo "ERROR: could not download or verify the fsbrain demo data." >&2
        return 1
    fi
    sdir="$(cat "${tmpfile}")"
    rm -f "${tmpfile}"
    if [[ -z "${sdir}" || ! -d "${sdir}" ]]; then
        echo "ERROR: could not determine the demo subjects_dir (got: '${sdir}')." >&2
        return 1
    fi
    echo "${sdir}"
}

# Resolve the demo subjects_dir to use. Uses the environment variable
# FSBRAIN_DEMO_SUBJECTS_DIR if set (by run_all_examples.sh), otherwise
# ensures + resolves the data locally (standalone use).
fsbrain_resolve_subjects_dir() {
    if [[ -n "${FSBRAIN_DEMO_SUBJECTS_DIR:-}" ]]; then
        echo "${FSBRAIN_DEMO_SUBJECTS_DIR}"
        return 0
    fi
    fsbrain_ensure_demo_data
}

# The demo subject id to use for examples that need a FreeSurfer subject.
fsbrain_demo_subject() {
    echo "${FSBRAIN_DEMO_SUBJECT:-subject1}"
}
