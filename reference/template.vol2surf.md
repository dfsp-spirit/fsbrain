# Project a volume onto a template surface like fsaverage or fs_LR_32.

Samples a volume at the positions of the vertices of a template surface,
e.g., for group-level statistical maps. The volume and the template
surface must be defined in the same coordinate space: `fsaverage` is the
FreeSurfer template and its surfaces are in MNI305 (surface RAS) space,
while the `fs_LR_32` template (the HCP-style 32k surface space) is in
MNI152 space. Projecting an MNI152 volume onto fsaverage (or vice versa)
will produce wrong results, but this function will usually detect it and
warn (see `check_fov`).

This is the equivalent of the `project_vol2surf` function of the Python
package `yabplot` for the `fs_LR_32` template, and of `mri_vol2surf` for
`fsaverage`. See
[`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md)
for the per-subject variant, which uses the native space of a subject.

## Usage

``` r
template.vol2surf(
  volume,
  template = "fsaverage",
  surface = NULL,
  hemi = "both",
  subjects_dir = NULL,
  surface_frac = NULL,
  frac_surface = "pial",
  interpolation = "trilinear",
  frame = 1L,
  cortex_only = FALSE,
  clamp = FALSE,
  check_fov = TRUE,
  affine = NULL,
  vox2ras = "auto"
)
```

## Arguments

- volume:

  character string or named list, see
  [`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md).
  For template projections, this is typically the filepath of a NIfTI
  file containing a group-level map, e.g., in MNI152 space.

- template:

  character string, the template identifier. One of 'fsaverage' (the
  FreeSurfer template, in MNI305 space) or 'fs_LR_32' (the HCP-style 32k
  template, in MNI152 space). Additional templates can be used by
  passing the respective `subjects_dir` and a template identifier that
  matches a directory in it.

- surface:

  character string, the name of the surface to sample. Examples:
  'white', 'pial', 'midthickness', 'inflated'. Defaults to 'white' for
  'fsaverage' and to 'midthickness' for 'fs_LR_32' if `surface` is
  `NULL`.

- hemi:

  character string, see
  [`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md).

- subjects_dir:

  character string or `NULL`, the subjects dir which contains the
  template subject. If `NULL` (the default), the fsbrain data cache (see
  [`get_optional_data_filepath`](https://dfsp-spirit.github.io/fsbrain/reference/get_optional_data_filepath.md))
  is searched first, then the FreeSurfer installation. If the required
  files are missing, the respective download function is suggested in
  the error message.

- surface_frac:

  numeric scalar or `NULL`, see
  [`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md).

- frac_surface:

  character string, see
  [`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md).

- interpolation:

  character string, see
  [`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md).

- frame:

  positive integer scalar, see
  [`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md).

- cortex_only:

  logical, see
  [`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md).
  For 'fsaverage', the required `label/?h.cortex.label` file is part of
  the FreeSurfer installation, for 'fs_LR_32' it can be downloaded with
  [`download_fs_LR_32_labels`](https://dfsp-spirit.github.io/fsbrain/reference/download_fs_LR_32_labels.md).

- clamp:

  logical, see
  [`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md).

- check_fov:

  logical, see
  [`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md).

- affine:

  numeric 4x4 matrix or `NULL`, see
  [`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md).

- vox2ras:

  character string, see
  [`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md).

## Value

a numerical vector of per-vertex values, or a hemilist of such vectors
if `hemi` is 'both'. See
[`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md).

## Note

The `fs_LR_32` template meshes can be downloaded with
[`download_fs_LR_32_meshes`](https://dfsp-spirit.github.io/fsbrain/reference/download_fs_LR_32_meshes.md).
Note that the surface files of the templates are not part of the fsbrain
package, and that the `fsaverage` data is subject to the FreeSurfer
license.

## See also

Other volume to surface projection functions:
[`subject.vol2surf()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   # Project a group-level stat map (e.g., in MNI152 space) onto the HCP-style fs_LR_32 template:
   fsbrain::download_fs_LR_32_meshes();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   stat_map = template.vol2surf("/path/to/your_statmap.nii.gz", template = "fs_LR_32",
       surface = "midthickness", subjects_dir = subjects_dir);
   # Project an MNI305 map onto fsaverage:
   stat_map = template.vol2surf("/path/to/your_mni305_map.nii.gz", template = "fsaverage",
       surface = "white");
} # }
```
