# The fsbrain Docker files: development website

This directory contains Dockerfiles for fsbrain. You do **not** need the files in this directory to **use** the fsbrain Docker containers.

If you want to run fsbrain via Docker, please go to [Dockerhub](https://hub.docker.com/r/dfspspirit/fsbrain) and/or read the Docker installation instructions on the [fsbrain repo website](https://github.com/dfsp-spirit/fsbrain).

The developer information is in [README_DEV_DOCKER.md](./README_DEV_DOCKER.md).

## Quick start: just trying it out

The recent fsbrain images (e.g. `dfspspirit/fsbrain:0.7.0`) run fully headless (scimesh software renderer, no X11/OpenGL/GPU needed). To use one, you put your R script in `input/`, run the container, and collect the generated images from `output/` — both directories are bind-mounted into the container (at `/home/input` and `/home/output`).

The easiest way to see this in action is the test harness in [test_image/](./test_image/). It runs a ready-made example script that downloads the fsbrain demo data via pkgfilecache (`download_optional_data()` / `download_fsaverage()`, cached on first run) and writes one numbered PNG per feature to `output/`:

```bash
cd docker/test_image
# Run the fsbrain example inside the image; downloads the demo data on first
# run (cached via pkgfilecache), then writes the PNGs to output/ on your host:
./test_image.sh \
    dfspspirit/fsbrain:0.7.0 \
    --pull
```

Afterwards the generated images are in `docker/test_image/output/`. To run your own code instead, drop an R script into `input/` (or mount your own volumes with `docker run -v ...:...`) and write to `/home/output` from within the container, e.g. via `export()`.
