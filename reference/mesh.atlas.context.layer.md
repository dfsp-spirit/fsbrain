# Compute the context layer of a mesh atlas visualization.

Compute the optional context mesh (typically a semi-transparent cortex)
that is rendered together with the data meshes of a mesh atlas.

## Usage

``` r
mesh.atlas.context.layer(subjects_dir, subject_id, cortex)
```

## Arguments

- subjects_dir:

  string, the subjects dir to use for the context mesh.

- subject_id:

  string, the subject id to use for the context mesh.

- cortex:

  the context definition. Either a character string (the name of the
  surface mesh to use, e.g., 'white'), a named list of options for the
  context mesh (supported entries are 'surface', 'color', 'alpha',
  'style', 'subjects_dir' and 'subject_id'), an `fs.coloredmesh`
  instance, or a hemilist of such instances (which is then used as-is).

## Value

a named list with entries 'lh' and 'rh', the context coloredmeshes.
