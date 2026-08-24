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

##### Install tools and system dependencies #####
RUN apt-get update && apt-get install -y apt-utils
RUN apt-get update && apt-get install -y \
  build-essential \
  git \
  libcurl4-openssl-dev \
  libfreetype6-dev \
  libgl1-mesa-dev \
  libglu1-mesa-dev \
  libmagick++-dev \
  libssh-dev \
  libx11-dev \
  libxml2-dev \
  mesa-common-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  && rm -rf /var/lib/apt/lists/*

##### Install required R packages #####

## Get fsbrain dependencies.
RUN R -e "install.packages('devtools', repos = 'http://cran.rstudio.com/')"

## We want freesurferformats with all optional dependencies for full file format support.
## Version 1.0.1 fixes many issues of the old 0.x series.
RUN R -e "devtools::install_version('freesurferformats', version = '1.0.1', dependencies=TRUE, repos = 'http://cran.rstudio.com')"

## rgl is a hard dependency of fsbrain (see the 'Imports' field in its
## DESCRIPTION), so it must be installed. It is NOT the active renderer in
## this image though: we render with scimesh (see below).
RUN R -e "install.packages('rgl', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('viridis', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('RColorBrewer', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('misc3d', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('igraph', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('magick', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('Rvcg', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('pracma', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('reshape', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('squash', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('data.table', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('methods', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('fields', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('pkgfilecache', repos = 'http://cran.rstudio.com/')"

## The scimesh software renderer backend, the default renderer of this image.
## It is a headless, GPU-free C++17 renderer, so it needs no display server.
RUN R -e "devtools::install_version('scimesh', version = '0.3.4', dependencies=TRUE, repos = 'http://cran.rstudio.com')"

## Now get fsbrain
RUN R -e "devtools::install_version('fsbrain', version = '0.7.0', dependencies=TRUE, repos = 'http://cran.rstudio.com')"

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
CMD cd /home/input && R

## Now run your analyses and write the results/images to /home/output.
