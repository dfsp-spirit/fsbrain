# Docker build for distributing an fsbrain container.
# This file is part of fsbrain, https://github.com/dfsp-spirit/fsbrain
#
# See the very bottom of this script for info on how to run it.

FROM rocker/r-ver:4.1.0

# Shell setup:
WORKDIR /root

##### Install tools and system dependencies #####
RUN apt-get update && apt-get install -y \
  build-essential \
  git \
  libcurl4-openssl-dev \
  libfreetype6-dev \
  libgfortran4 \
  libgl1-mesa-dev \
  libglu1-mesa-dev \
  libmagick++-dev \
  libssh-dev \
  libx11-dev \
  libxml2-dev \
  mesa-common-dev \
  xvfb \
  && rm -rf /var/lib/apt/lists/*

##### Install required R packages #####

## Get fsbrain dependencies.
RUN R -e "install.packages('devtools', repos = 'http://cran.rstudio.com/')"

## We want freesurferformats with all optional dependencies for full file format support.
RUN R -e "devtools::install_version('freesurferformats', version = '0.1.14', dependencies=TRUE, repos = 'http://cran.rstudio.com')"

RUN R -e "install.packages('rgl', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('viridis', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('RColorBrewer', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('misc3d', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('igraph', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('magick', repos = 'http://cran.rstudio.com/')"

## Now get fsbrain
RUN R -e "devtools::install_version('fsbrain', version = '0.4.3', dependencies=TRUE, repos = 'http://cran.rstudio.com')"

## Create folders for mapping. You should write output to /home/output in the R session.
RUN mkdir /home/input
RUN mkdir /home/output

## Run R in xvfb with 2k resolution.
CMD cd /home/input \
  && xvfb-run --server-args="-screen 0, 2560x1440x24" R

## Now run your analyses and write the results/images to /home/output.

