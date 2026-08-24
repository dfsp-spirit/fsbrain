## Installing fsbrain from from source

Some platforms, like Linux, default to building fsbrain from source.

While fsbrain is written in plain R, some of its dependencies are written in C++, and building from source thus requires a compiler and the specific libraries required to build each dependency. These are known as system dependencies.


### System dependencies

A *system dependency* is a **non-R** software that is needed for the installation of a package. System dependencies cannot be installed automatically using the R package system, so you need to install them manually or using the package manager of your operating system.

The *fsbrain* package itself does not have any system dependencies, however, it uses *rgl* for rendering. You can check the *SystemRequirements* section on the [rgl page at CRAN](https://CRAN.R-project.org/package=rgl) for the full list of rgl dependencies or read on. To get GIFTI format support, you will also need `libxml2-dev`.

**Note**: If you use the [scimesh](https://CRAN.R-project.org/package=scimesh) backend, none of the rgl system dependencies are required — scimesh is a pure C++ software renderer with no external library dependencies beyond a C++ compiler.

To install the system dependencies for *rgl* and *xml2*:

#### Linux System dependencies (or: building from source)

R packages are compiled from source by default under Linux, so you need some development libraries. Before installing *fsbrain*, run the following command in your system shell (not in R):

* for deb-based Linux distributions (Debian, Ubuntu, ...):
```shell
sudo apt-get install build-essential cmake libmagick++-dev libx11-dev libgl1-mesa-dev libglu1-mesa-dev mesa-common-dev libfreetype6-dev libxml2-dev libssh-dev libcurl4-openssl-dev gfortran libblas-dev liblapack-dev libgfortran5
```

Note: For older Ubuntu versions, you may have to replace ```libgfortan5``` in the command above with ```libgfortan4```.


* for rpm-based Linux distributions (Fedora, CentOS, RHEL, ...):
```shell
sudo yum install ImageMagick-c++-devel libX11-devel mesa-libGLU-devel freetype-devel libxml2-devel
```

If you want to compile the package under any other operating system, you will need the libraries as well, of course.

#### MacOS System dependencies

Recent macOS versions do not ship with an X11 environment. If you want to use the default rgl backend for interactive viewing, you will need to install [XQuartz](https://www.xquartz.org/). If you want to create GIF movies, make sure you have imagemagick installed (easiest via [homebrew](https://brew.sh/): `brew install imagemagick@6`).

**Recommended for static image export**: Use the [scimesh backend](#optional-scimesh-rendering-backend-for-headless-environments), which requires neither X11 nor XQuartz and produces identical publication-ready images.

Note that X11 is not needed for rendering, but only for opening interactive windows. If you only need publication-quality static images (which is the typical use case), the scimesh backend or the browser-based `rglwidget` are better options.
