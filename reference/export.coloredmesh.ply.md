# Export a coloredmeshes with vertexcolors in PLY format.

Exports coloredmeshes with vertex coloring to standard mesh files in
Stanford Triangle (PLY) format. This is very hand for rendering in
external standard 3D modeling software like Blender.

## Usage

``` r
export.coloredmesh.ply(filepath, coloredmesh)
```

## Arguments

- filepath:

  The export filepath, including file name and extension.

- coloredmesh:

  an 'fs.coloredmesh' instance, as returned (silently) by all surface
  visualization functions, like
  [`vis.subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md).

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   coloredmeshes = vis.subject.morph.native(subjects_dir, 'subject1', 'thickness');
   export.coloredmesh.ply('~/subject1_thickness_lh.ply', coloredmeshed$lh);
} # }
```
