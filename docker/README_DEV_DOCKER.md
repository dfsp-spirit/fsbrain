
# Dockerfile Development Information for fsbrain

Note: This is relevant only if you are an fsbrain developer.

The files are organized in subfolders by fsbrain version, and in there by R version.

## Creating a new image

* Create a new directory following the naming structure and copy an existing Dockerfile from another directory as a template.
* Edit the Dockerfile for the new version.
* Build the new container:

```
cd <fsbrain>/docker/<subdir>
sudo docker build -t fsbrain .
```

## Publishing the new version on Dockerhub

To create a new version of the image on docker.io, one needs to build the new image as described above, and then:

1) login to docker.io:

```
sudo docker login -u "dfspspirit" --password-stdin docker.io
```

2) tag the image with the dockerhub user/repo and a version,  e.g.:

```
sudo docker tag fsbrain dfspspirit/fsbrain:0.5.dev
```

3) push the container to dockerhub:
```
sudo docker push dfspspirit/fsbrain:0.5.dev
```


