# Docker build for distributing an fsbrain container.
# This file is part of fsbrain, https://github.com/dfsp-spirit/fsbrain
#
# This image uses the scimesh software renderer backend by default, so it is
# fully headless: static images are rendered in software (CPU only), with no
# X11 server, OpenGL, or GPU required. See vignette("fsbrain_with_scimesh").
#
# See the very bottom of this script for info on how to run it.

FROM rocker/r-ver:4.6.1

# Shell setup:
WORKDIR /root

## Pin the site library as the stable, persistent package location. Without
## this, R may install packages into a per-session library that is not on the
## path of the next RUN layer ("there is no package called 'X'" failures).
ENV R_LIBS_USER="/usr/local/lib/R/site-library"

##### Install tools and system dependencies #####
# Note: 'cmake' and 'libuv1-dev' are needed to build the R 'fs' package from
# source (fs needs libuv1-dev, or cmake to compile its vendored libuv).
# 'cmake' is also required by the R 'sass' package (rmarkdown dependency).
RUN apt-get update && apt-get install -y apt-utils
RUN apt-get update && apt-get install -y \
  build-essential \
  cmake \
  git \
  libcurl4-openssl-dev \
  libfreetype6-dev \
  libgl1-mesa-dev \
  libglu1-mesa-dev \
  libmagick++-dev \
  libssh-dev \
  libuv1-dev \
  libx11-dev \
  libxml2-dev \
  mesa-common-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  && rm -rf /var/lib/apt/lists/*

##### Install required R packages #####

## Get fsbrain dependencies. 'remotes' is much lighter than 'devtools' (which
## pulls in roxygen2, stringi/ICU, fs, xml2, ...) and provides install_version()
## for reproducible, version-pinned installs.
RUN R -e "install.packages('remotes', repos = 'https://cloud.r-project.org/')"

## We want freesurferformats with all optional dependencies for full file format support.
## Version 1.0.1 fixes many issues of the old 0.x series.
RUN R -e "remotes::install_version('freesurferformats', version = '1.0.1', dependencies=TRUE, repos = 'https://cloud.r-project.org')"

## rgl is a hard dependency of fsbrain (see the 'Imports' field in its
## DESCRIPTION), so it must be installed. It is NOT the active renderer in
## this image though: we render with scimesh (see below).
RUN R -e "install.packages('rgl', repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages('viridis', repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages('RColorBrewer', repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages('misc3d', repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages('igraph', repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages('magick', repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages('Rvcg', repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages('pracma', repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages('reshape', repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages('squash', repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages('data.table', repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages('methods', repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages('fields', repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages('pkgfilecache', repos = 'https://cloud.r-project.org/')"

## The scimesh software renderer backend, the default renderer of this image.
## It is a headless, GPU-free C++17 renderer, so it needs no display server.
RUN R -e "remotes::install_version('scimesh', version = '0.3.4', dependencies=TRUE, repos = 'https://cloud.r-project.org')"

## Now get fsbrain
RUN R -e "remotes::install_version('fsbrain', version = '0.7.0', dependencies=TRUE, repos = 'https://cloud.r-project.org')"

## Make scimesh the default renderer backend for every R session in this
## container, and keep the 2k output resolution of the old rgl-based images.
## You can still switch back per session with:
##   options(fsbrain.renderer_backend = "rgl")
RUN echo 'options(fsbrain.renderer_backend = "scimesh");' >> /root/.Rprofile
RUN echo 'options(fsbrain.scimesh.output_dims = c(2560, 1440));' >> /root/.Rprofile

## Create folders for mapping. You should write output to /home/output in the R session.
RUN mkdir /home/input
RUN mkdir /home/output

## Run R. No xvfb is needed: the scimesh backend is fully headless and renders
## all static images (e.g., vis.* with views=NULL followed by export()) in
## software. Note that interactive 3D viewers and vis.rglwidget() still use
## the rgl backend (and need a display) and will NOT work in this image.
CMD ["/bin/sh", "-c", "cd /home/input && R"]

## Now run your analyses and write the results/images to /home/output.
