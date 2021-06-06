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
# We could also remove this here and leave it to the script that will be executed below.
RUN R -e "fsbrain::download_optional_data(); fsbrain::download_fsaverage(TRUE);"


##### Copy a demo R script over and run it using xvfb #####
# This is for testing and demonstration purposes only, it will be removed later.

RUN mkdir /home/input
RUN mkdir /home/output

#COPY myscript.R /home/input/myscript.R
# We assume here that myscript.R produces output in the current working directory.
# TODO: We want to xvfb_run here (also see https://stackoverflow.com/questions/32151043/xvfb-docker-cannot-open-display)
#CMD cd /home/input \
#  && R -e "source('myscript.R')" \
#  && cp lh.distgeod lh.disteuclid /home/output/

COPY myscript_plot.R /home/input/myscript_plot.R
CMD cd /home/input \
  && xvfb-run --server-args="-screen 0, 1920x1080x24" ./myscript_plot.R \
  && cp lh.distgeod lh.disteuclid fsbrain_arranged.png /home/output/


##### Usage information #####
## To build this container
#
#    cd <fsbrain>/docker/
#    sudo docker build -t fsbrain .
#
## To run the script inside the container and get the output on the host machine,
## run the following commands in your system shell on the host:
#
#    mkdir ~/fsbrain_docker_results
#    sudo docker run -v ~/fsbrain_docker_results:/home/output fsbrain
#


##### Develop information (for my future self only)
#
# To create a new version of the image on docker.io, one needs to build
# the new image as described above, and then:
#
# 1) login to docker.io:
#
#    sudo docker login -u "dfspspirit" -p "thedockerhubpwd" docker.io
#
# 2) tag the image with the dockerhub user/repo and a version,  e.g.:
#    
#    sudo docker tag fsbrain dfspspirit/fsbrain:0.5.dev
#
# 3) push the container to dockerhub:
#
#    sudo docker push dfspspirit/fsbrain:0.5.dev
#  


