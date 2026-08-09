# Retrieve surface mesh data for a group of subjects.

Retrieve surface mesh data for a group of subjects.

## Usage

``` r
group.surface(
  subjects_dir,
  subjects_list,
  surface,
  hemi = "both",
  force_hemilist = TRUE
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subjects_list:

  vector of strings. The subject identifiers.

- surface:

  character string, the surface to load. Something like 'white' or
  'pial'.

- hemi:

  string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to
  construct the names of the mesh files to be loaded.

- force_hemilist:

  logical, whether to force the individual values inside the named
  return value list to be hemilists (even if the 'hemi' parameter is not
  set to 'both'). If this is FALSE, the inner values will contain the
  respective (lh or rh) surface only.

## Value

named list of surfaces: Each name is a subject identifier from
subjects_list, and the values are hemilists of `fs.surface` instances.

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   subjects_list = c("subject1", "subject2");
   surfaces = group.surface(subjects_dir, subjects_list, 'white', "both");
} # }
```
