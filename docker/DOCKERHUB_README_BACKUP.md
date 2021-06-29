# fsbrain Docker Images

This is the official hub.docker.com repo for [fsbrain](https://github.com/dfsp-spirit/fsbrain)  Docker containers, maintained by [Tim Schäfer](http://rcmd.org/ts/).

The fsbrain package is a GNU R library for structural neuroimaging. It provides high-level functions to access (read and write) and visualize surface-based brain morphometry data, e.g. cortical thickness, for individual subjects and groups. See [github.com/dfsp-spirit/fsbrain](https://github.com/dfsp-spirit/fsbrain) for more details on fsbrain, including the documentation, installation instructions, and issue tracker.


## Using the fsbrain Docker images

### Minimal Example

* Make sure that you have [Docker installed](https://docs.docker.com/get-docker/) on the machine on which you want to run fsbrain via Docker
* Pull the image and make sure to specify the fsbrain version you want. E.g., for fsbrain 0.4.3: `sudo docker pull dfspspirit/fsbrain:0.4.3`
* You can now run an interactive R session inside the container, e.g.: `sudo docker run -it dfspspirit/fsbrain:0.4.3`

In that R session, you can now load fsbrain: `library("fsbrain");` That's great, but read on to learn how to get your data and scripts, and of course how to save your results to the host computer. (Simply type `q()` in R to exit the interactive R session and destroy the container. Afterwards you are back on your host.).

### Making your data available inside the container and keeping results

Typically you will want to mount some part of the host filesystem (e.g., your input neuroimaging data in a FreeSurfer $SUBJECTS_DIR or a folder above that which also includes R scripts for the statistical analysis) into the container, work on that data in an interactive session, and save the results to a separate directory that will persist on your host after you exit the container. Here is an example that does this. We start by creating an output directory on the host, and assume that the data and scripts you need for your analysis are stored in `~/data/study1/` on the host.

**Note:** I use `sudo` before any `docker` command in the following examples. This may not be needed on your system, depending on how Docker was installed.

```
mkdir ~/fsbrain_docker_results
sudo docker run -v ~/fsbrain_docker_results:/home/output -v ~/data/study1:/home/input:ro -it dfspspirit/fsbrain:0.4.3
```
Now you are in an R session inside the container, and your data from the host system is available under `/home/input/`. You must write any results you want to keep after the container is destroyed to  `/home/output/`.

### Using a single input/output directory instead

The approach above has the advantage that you cannot accidentally overwrite or change your input data because it is mounted read-only in the container. If having separate directories for the input and output seems unintuitive or you want to change the input data, you can simply mount a single directory in read-write mode:

```
sudo docker run -v ~/data/study1:/home/input -it dfspspirit/fsbrain:0.4.3
```

Now you are in an R session inside the container, and your data from the host system is available under `/home/input/`. You must write any results you want to keep after the container is destroyed to the input directory.


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
