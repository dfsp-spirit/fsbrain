# Visualize contour of a volume.

Compute a smoothed surface from the voxel intensities in the given
volume and render it. Requires the `misc3d` package to be installed,
which is an optional dependency.

## Usage

``` r
volvis.contour(volume, level = 80, show = TRUE, frame = 1L, color = "white")
```

## Arguments

- volume:

  a 3D brain volume

- level:

  numeric, intensity threshold for the data. Voxels with intensity value
  smaller than `level` will be ignored when creating the contour
  surface.

- show:

  logical, whether to display the triangles. Defaults to `TRUE`.

- frame:

  integer, the frame to show in case of a 4D input volume. Can also be
  the character string 'all' to draw the contents of all frames at once.
  Useful to plot white matter tracts from DTI data, where each tract is
  stored in a different frame.

- color:

  the color to use when plotting. Can be a vector of colors when
  plotting all frames of a 4D image (one color per frame).

## Value

the rendered triangles (a `Triangles3D` instance) with coordinates in
surface RAS space if any, `NULL` otherwise. This will be a list if you
pass a 4D volume and select 'all' frames.

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   brain = subject.volume(subjects_dir, 'subject1', 'brain');
   # Plot all voxels of the brain:
   volvis.contour(brain);
} # }
```
