# fsbrain Docker Images

This is the official hub.docker.com repo for [fsbrain](https://github.com/dfsp-spirit/fsbrain)  Docker containers, maintained by [Tim Schäfer](https://ts.rcmd.org/).

The fsbrain package is a GNU R library for structural neuroimaging. It provides high-level functions to access (read and write) and visualize surface-based brain morphometry data, e.g. cortical thickness, for individual subjects and groups. See [github.com/dfsp-spirit/fsbrain](https://github.com/dfsp-spirit/fsbrain) for more details on fsbrain, including the documentation, installation instructions, and issue tracker.


## Using the fsbrain Docker images

### Rendering backends: headless (scimesh) by default, rgl still available

The 0.7.0 image renders static images with the **scimesh** software renderer by default: it is fully headless (CPU-only, no X11 server, OpenGL, or GPU required), so you can run it on servers, HPC nodes, and CI runners and get publication-ready images via `export()` without any display.

Long-time users do **not** need to change their workflow: the image still contains **rgl** and **Xvfb**, so the traditional pre-0.7.0 way of rendering keeps working unchanged. To opt out of scimesh and use rgl again, just run R under a virtual display and switch the backend:

```
xvfb-run -a R
options(fsbrain.renderer_backend = "rgl")
```

Note that truly *interactive* 3D viewers (rotating a mesh in a window) still need a real display or X11 forwarding into the container, so they are not really supported here; the intended workflow is static image export.

### Minimal Example

* Make sure that you have [Docker installed](https://docs.docker.com/get-docker/) on the machine on which you want to run fsbrain via Docker
* Pull the image and make sure to specify the fsbrain version you want. E.g., for fsbrain 0.7.0: `sudo docker pull dfspspirit/fsbrain:0.7.0`
* You can now run an interactive R session inside the container, e.g.: `sudo docker run --rm -it dfspspirit/fsbrain:0.7.0`

In that R session, you can now load fsbrain: `library("fsbrain");`

That's great, but you are inside the container, and *cannot access the host filesystem* of your computer! Read on to learn how to access it, because you will most likely need it to get your data and R scripts, and of course to learn how to also *save your results* to the host computer in the end. (Simply type `q()` in R to exit the interactive R session; with `--rm` the container is removed automatically afterwards, and you are back on your host.).


### Making your data available inside the container and keeping results

Typically you will want to mount some part of the host filesystem (e.g., your input neuroimaging data in a FreeSurfer $SUBJECTS_DIR or a folder above that which also includes R scripts for the statistical analysis) into the container, work on that data in an interactive session, and save the results to a separate directory that will persist on your host after you exit the container. Here is an example that does this. We start by creating an output directory on the host, and assume that the data and scripts you need for your analysis are stored in `~/data/study1/` on the host.

**Note:** I use `sudo` before any `docker` command in the following examples. This may not be needed on your system, depending on how Docker was installed.

```
mkdir ~/fsbrain_docker_results
sudo docker run --rm -v ~/fsbrain_docker_results:/home/output -v ~/data/study1:/home/input:ro -it dfspspirit/fsbrain:0.7.0
```
Now you are in an R session inside the container, and your data from the host system is available under `/home/input/`. You must write any results you want to keep after the container is destroyed to `/home/output/`.

If you prefer a single read-write directory instead of separate input/output folders (e.g. because you want to modify the input data), you can simply mount just one directory:

```
sudo docker run --rm -v ~/data/study1:/home/input -it dfspspirit/fsbrain:0.7.0
```

**Note:** the container runs as root, so files it writes to `/home/output` are owned by root on your host. Clean up with e.g. `sudo chown -R "$USER" ~/fsbrain_docker_results`.

### Running a shell (or something else) instead of R in the container

If you have data analysis and visualization scripts ready and prefer to run them from a shell instead of being dropped in an interactive R session, you can of course specify a custom command to run when the container is started. Here we start an interactive bash session instead of R:

```
sudo docker run --rm -v ~/data/study1:/home/input -it dfspspirit/fsbrain:0.7.0 /bin/bash
```


## Citation

To cite fsbrain in publications use:


```
    Tim Schaefer, Christine Ecker (2020). fsbrain: an R package for the visualization of structural neuroimaging data. bioRxiv doi: 10.1101/2020.09.18.302935
```

A BibTeX entry for LaTeX users is

```
  @Misc{,
    title = {fsbrain: an {R} package for the visualization of structural neuroimaging data},
    author = {Tim Schaefer and Christine Ecker},
    year = {2020},
    url = {https://www.biorxiv.org/content/10.1101/2020.09.18.302935v1},
    doi = {10.1101/2020.09.18.302935},
  }

```