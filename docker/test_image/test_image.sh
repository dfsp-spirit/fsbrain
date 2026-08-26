#!/usr/bin/env bash
#
# test_image.sh
# =============
#
# Runs the fsbrain example script (input/run_fsbrain_example.R) inside a
# docker image and collects the generated images in output/.
#
# This is a development/test harness for the fsbrain docker images (see e.g.
# docker/fsbrain0.7.0/Dockerfile). It validates that a published image can
# actually run fsbrain example code and produce output. The R script inside
# the container is a plain-R port of the example code from the
# 'fsbrain with the scimesh rendering backend' vignette, so it runs fully
# headless (no X11/OpenGL/GPU).
#
# Usage:
#   ./test_image.sh <IMAGE> [--cache <DIR>] [--pull]
#
#   <IMAGE>   Full docker image reference, e.g. 'dfspspirit/fsbrain:0.7.0'.
#   --cache <DIR>
#             Host directory that is bind-mounted into the container and used
#             as the fsbrain data cache (download_optional_data() /
#             download_fsaverage()). The container R script sets
#             options(pkgfilecache.cachedir = '/fsbrain_data'); pkgfilecache
#             appends the package name, so the final cache path is
#             /fsbrain_data/fsbrain, which is exactly where this dir is
#             mounted. Default: the same location fsbrain uses on this
#             machine, i.e. ${XDG_DATA_HOME:-$HOME/.local/share}/R/fsbrain
#             (the Linux value of tools::R_user_dir('fsbrain', 'data')). Use
#             --cache if your cache lives elsewhere; note that this default
#             lookup may differ between pkgfilecache versions.
#   --pull    Always pull the image from its registry, even if a local image
#             with this tag already exists. By default the image is only
#             pulled if it is not present locally (so you can also test a
#             locally built image, e.g. 'fsbrain' or 'dfspspirit/fsbrain:0.7.0').
#
# Environment:
#   DOCKER    Docker command to use. Default: 'docker' (falls back to
#             'sudo docker' if the plain command needs root). Example:
#             DOCKER='sudo docker' ./test_image.sh dfspspirit/fsbrain:0.7.0
#
# Directory layout (all relative to this script's directory):
#   input/    mounted read-only at /home/input   (contains the R script)
#   output/   mounted at /home/output            (generated images land here)
#   <cache>   mounted at /fsbrain_data/fsbrain   (downloaded data cache)
#
# Note: the container runs as root (rocker default), so files it writes into
# output/ (and into the cache dir) are owned by root on your host. Clean up
# with e.g.:  sudo chown -R "$USER" output/
#

set -euo pipefail;

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)";
INPUT_DIR="$SCRIPT_DIR/input";
OUTPUT_DIR="$SCRIPT_DIR/output";
R_SCRIPT="run_fsbrain_example.R";

usage() {
    cat <<'EOF'
Usage: test_image.sh <IMAGE> [--cache <DIR>] [--pull]

  <IMAGE>     Full docker image reference, e.g. 'dfspspirit/fsbrain:0.7.0'.
  --cache <DIR>
              Host dir mounted at /fsbrain_data (fsbrain data cache).
              Default: ${XDG_DATA_HOME:-$HOME/.local/share}/R/fsbrain.
  --pull      Always pull the image, even if present locally.

Environment:
  DOCKER      Docker command to use (default 'docker', falls back to
              'sudo docker').
EOF
    exit 1;
}

# ---- Parse arguments ---------------------------------------------------------
IMAGE="";
CACHE_HOST="";
DO_PULL=0;
while [ $# -gt 0 ]; do
    case "$1" in
        --cache)
            if [ $# -lt 2 ]; then echo "Option '--cache' requires an argument." >&2; usage; fi
            CACHE_HOST="$2"; shift 2;;
        --pull)
            DO_PULL=1; shift;;
        -h|--help)
            usage;;
        --)
            shift; break;;
        -*)
            echo "Unknown option: $1" >&2; usage;;
        *)
            if [ -n "$IMAGE" ]; then echo "Unexpected extra argument: $1" >&2; usage; fi
            IMAGE="$1"; shift;;
    esac
done
if [ -z "$IMAGE" ]; then echo "No image given." >&2; usage; fi

# ---- Resolve the docker command (never hardcode sudo) ------------------------
detect_docker() {
    local cmd="${DOCKER:-docker}";
    read -r -a DOCKER_CMD <<< "$cmd";
    if "${DOCKER_CMD[@]}" info >/dev/null 2>&1; then
        return 0;
    fi
    if [ "$cmd" = "docker" ] && command -v sudo >/dev/null 2>&1 && sudo -n docker info >/dev/null 2>&1; then
        DOCKER_CMD=(sudo docker);
        return 0;
    fi
    echo "Cannot use docker: tried 'docker' and 'sudo docker'." >&2;
    echo "Either add your user to the 'docker' group (then log out/in), or set DOCKER='sudo docker'." >&2;
    return 1;
}

# ---- Prepare directories ------------------------------------------------------
# Default host cache dir: mirror what pkgfilecache would use on this machine.
if [ -z "$CACHE_HOST" ]; then
    CACHE_HOST="${XDG_DATA_HOME:-$HOME/.local/share}/R/fsbrain";
fi
if [ ! -d "$CACHE_HOST" ]; then
    echo "Cache dir '$CACHE_HOST' does not exist yet; it will be created (first run will download the example data)." >&2;
    mkdir -p "$CACHE_HOST";
fi
mkdir -p "$INPUT_DIR" "$OUTPUT_DIR";
if [ ! -f "$INPUT_DIR/$R_SCRIPT" ]; then
    echo "R script not found: $INPUT_DIR/$R_SCRIPT" >&2;
    exit 1;
fi

detect_docker;

# ---- Pull the image (unless it is already available locally) -----------------
if [ "$DO_PULL" = "1" ]; then
    "${DOCKER_CMD[@]}" pull "$IMAGE";
elif ! "${DOCKER_CMD[@]}" image inspect "$IMAGE" >/dev/null 2>&1; then
    echo "Image '$IMAGE' not found locally, pulling it...";
    "${DOCKER_CMD[@]}" pull "$IMAGE";
fi

# ---- Run the test -------------------------------------------------------------
echo "Running '$IMAGE' (cache dir: '$CACHE_HOST')...";
"${DOCKER_CMD[@]}" run --rm \
    -v "$INPUT_DIR:/home/input:ro" \
    -v "$OUTPUT_DIR:/home/output" \
    -v "$CACHE_HOST:/fsbrain_data/fsbrain" \
    "$IMAGE" \
    Rscript "/home/input/$R_SCRIPT";

echo;
echo "Done. Output images are in: $OUTPUT_DIR";
ls -la "$OUTPUT_DIR";
