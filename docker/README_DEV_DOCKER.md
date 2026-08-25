
# Dockerfile Development Information for fsbrain

Note: This is relevant only if you are an fsbrain developer.

The Dockerfiles are organized in subfolders by fsbrain version. The older
images additionally use an R version subfolder (e.g.,
`docker/fsbrain0.5.3/R4.1.0/`), while the newer ones live directly in the
version folder (e.g., `docker/fsbrain0.7.0/Dockerfile`).

## Renderer backends in the Docker images

* The `fsbrain0.7.0` image uses the **scimesh software renderer backend** by
  default (`options(fsbrain.renderer_backend = "scimesh")` is set in the
  container's `.Rprofile`). scimesh renders static images in software, so the
  container is fully headless: no X11, OpenGL, or GPU is required, and no
  `xvfb` is needed. See `vignette("fsbrain_with_scimesh")` for details.
* The older images (`fsbrain0.5.x` etc.) use the rgl/OpenGL backend and rely
  on `xvfb` to provide a virtual display for rendering.

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
docker login -u "dfspspirit" docker.io
```

2) tag the image with the dockerhub user/repo and a version,  e.g.:

```
sudo docker tag fsbrain dfspspirit/fsbrain:0.7.0
```

3) push the container to dockerhub:
```
sudo docker push dfspspirit/fsbrain:0.7.0
```


