# Docker build for distributing an fsbrain container.
# This file is part of fsbrain, https://github.com/dfsp-spirit/fsbrain

FROM rocker/r-ver:4.1.0

# Shell setup:
WORKDIR /root

##### Install tools and system dependencies #####
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
  xvfb \
  && rm -rf /var/lib/apt/lists/*

##### Install required R packages #####

## We want freesurferformats with all optional dependencies for full file format support.
RUN R -e "install.packages('freesurferformats', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"

## Get fsbrain dependencies.
RUN R -e "install.packages('devtools', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('rgl', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('viridis', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('RColorBrewer', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('misc3d', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('igraph', repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('magick', repos = 'http://cran.rstudio.com/')"
# We need Rvcg from Github
#RUN R -e "install.packages('Rvcg', repos = 'http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('zarquon42b/Rvcg')"

## Now get fsbrain
RUN R -e "devtools::install_github('dfsp-spirit/fsbrain', ref='geodesic')"


##### Prepare for plotting ######

## Download fsaverage using fsbrain, this is needed for standard space visualizations of FreeSurfer data.
RUN R -e "fsbrain::download_optional_data(); fsbrain::download_fsaverage(TRUE);"


##### Run a demo R script #####






