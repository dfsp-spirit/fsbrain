# Docker image test harness for fsbrain

This directory contains a small development/test harness for the fsbrain
Docker images (e.g. `docker/fsbrain0.7.0/Dockerfile`). It runs example code
from the *"fsbrain with the scimesh rendering backend"* vignette (a plain-R
port) inside a given image and collects the generated images, so you can
verify that a published image actually works.

## Usage

```bash
./test_image.sh <IMAGE> [--cache <DIR>] [--pull]
```

Example:

```bash
./test_image.sh dfspspirit/fsbrain:0.7.0
```

The image is pulled first (unless it already exists locally, so you can also
test a locally built image). The container runs
`Rscript /home/input/run_fsbrain_example.R`, which writes 10 numbered PNGs to
`output/` on your machine.

## Options

- `--cache <DIR>` — host directory used as the fsbrain data cache
  (`download_optional_data()` / `download_fsaverage()`). Default:
  `${XDG_DATA_HOME:-$HOME/.local/share}/R/fsbrain`, i.e. the same cache
  fsbrain already uses on this machine.
- `--pull` — always pull the image from its registry, even if present locally.

## Environment

- `DOCKER` — docker command to use. Default: `docker`, with an automatic
  fallback to `sudo docker` if needed (no hardcoded sudo).

## Directory layout

| Host dir  | Mounted at      | Purpose                              |
|-----------|-----------------|--------------------------------------|
| `input/`  | `/home/input`   | R script (read-only)                 |
| `output/` | `/home/output`  | generated images land here           |
| `<cache>` | `/fsbrain_data/fsbrain` | downloaded example data cache |

Note: the R script sets `options(pkgfilecache.cachedir = "/fsbrain_data")`;
pkgfilecache appends the package name, so the actual cache path is
`/fsbrain_data/fsbrain` — that is why the cache dir is mounted there.

## Notes

- The container runs as root (rocker default), so files it writes into
  `output/` (and into the cache) are root-owned on your host. Clean up with
  e.g. `sudo chown -R "$USER" output/`.
- The default cache lookup may differ between pkgfilecache versions; use
  `--cache` if your cache lives elsewhere.
