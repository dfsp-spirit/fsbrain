# Voxel-based visualization of volume mask at surface RAS positions.

Plots a 3D box at every *foreground* voxel in the given volume. All
voxels which do not have their intensity value set to `NA` are
considered *foreground* voxels. The locations at which to plot the
voxels is computed from the voxel CRS indices using the FreeSurfer
[`vox2ras_tkr`](https://dfsp-spirit.github.io/fsbrain/reference/vox2ras_tkr.md)
matrix. This means that the position of the rendered data fits to the
surface coordinates (in files like `surf/lh.white`), and that you can
call this function while an active surface rendering window is open
(e.g., from calling
[`vis.subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md)),
to superimpose the surface and volume data. **On coloring the voxels**
(using *rgl materials*): Note that you can call this function several
times for the active plot, and color the voxels differently by passing
different material properties in each call. Alternatively, check the
`voxelcol` parameter.

## Usage

``` r
volvis.voxels(volume, render_every = 1, voxelcol = NULL, ...)
```

## Arguments

- volume:

  numeric 3d array, voxels which should not be plotted must have value
  `NA`. Take care not to plot too many.

- render_every:

  integer, how many to skip before rendering the next one (to improve
  performance and/or see deeper structures). Use higher values to see a
  less dense representation of your data that usually still allows you
  to see the general shape, but at lower computational burden. Set to 1
  to render every (foreground) voxel.

- voxelcol:

  character string or a *voxel coloring*. A *voxel coloring* can be
  specified in three ways: 1) the string 'from_intensity' will compute
  colors based on the intensity values of the foreground voxels in the
  volume, applying normalization of the intensity values if needed. 2)
  an array of RGB color strings: will be used to retrieve the colors for
  all foreground vertices, at their CRS indices. 3) A vector with length
  identical to the number of foreground voxels in the volume: will be
  applied directly. Obvisouly, you should not pass a color material
  parameter (see `...`) when using this.

- ...:

  material properties, passed to
  [`triangles3d`](https://dmurdoch.github.io/rgl/dev/reference/primitives.html).
  Example: `color = "#0000ff", lit=FALSE`.

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   brain = subject.volume(subjects_dir, 'subject1', 'brain');
   # Plot all voxels of the brain:
   brain[which(brain==0L, arr.ind = TRUE)] = NA;  # mark background
   brain = vol.hull(brain); # remove inner triangles
   volvis.voxels(brain);
} # }
```
