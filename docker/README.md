# The fsbrain Docker files: development website

This directory contains Dockerfiles for fsbrain. You do not need the files in this directory to use the fsbrain Docker containers. If you want to run fsbrain via Docker, please go to [Dockerhub](https://hub.docker.com/r/dfspspirit/fsbrain) and/or read the Docker installation instructions on the [fsbrain repo website](../).

## Development information

The files are organized in subfolders by fsbrain version, and in there by R version.

To create a new version of the image on docker.io, one needs to build the new image as described above, and then:

1) login to docker.io:

    sudo docker login -u "dfspspirit" -p "thedockerhubpwd" docker.io

2) tag the image with the dockerhub user/repo and a version,  e.g.:
    
    sudo docker tag fsbrain dfspspirit/fsbrain:0.5.dev

3) push the container to dockerhub:

    sudo docker push dfspspirit/fsbrain:0.5.dev


