# Create a submesh including only the given vertices.

Create a submesh including only the given vertices.

## Usage

``` r
submesh.vertex(surface_mesh, old_vertex_indices_to_use, ret_mappings = FALSE)
```

## Arguments

- surface_mesh:

  an fs.surface instance, the original mesh

- old_vertex_indices_to_use:

  integer vector, the vertex indices of the 'surface_mesh' that should
  be used to construct the new sub mesh.

- ret_mappings:

  whether to return the vertex mappings. If TRUE, the return value
  becomes a list with entries 'submesh', 'vmap_full_to_submesh', and
  'vmap_submesh_to_full'.

## Value

the new mesh, made up of the given 'old_vertex_indices_to_use' and all
(complete) faces that exist between the query vertices in the source
mesh. But see 'ret_mapping' parameter.

## Examples

``` r
if (FALSE) { # \dontrun{
sjd = fsaverage.path(T);
sj = "fsaverage";
mesh = subject.surface(sjd, sj, hemi="lh");
lab = subject.label(sjd, sj, "cortex", hemi = "lh");
sm = fsbrain:::submesh.vertex(mesh, lab);
vis.fs.surface(mesh);
vis.fs.surface(sm);
} # }
```
