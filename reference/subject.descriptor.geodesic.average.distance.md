# Compute mean geodesic distance descriptor for a subject.

For all vertices: compute the mean pseudo-geodesic distance from this
vertex to all others in the same hemisphere. Computes `|V|^2` geodesic
distances.

## Usage

``` r
subject.descriptor.geodesic.average.distance(
  subjects_dir,
  subject_id,
  surface = "white",
  hemi = "both",
  ...
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier.

- surface:

  string. The display surface. E.g., "white", "pial", or "inflated".
  Defaults to "white".

- hemi:

  string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to
  construct the names of the label data files to be loaded.

- ...:

  extra parameters passed on to `geodesic.average.distance`. Ignored if
  'cortex_only' is TRUE.

## Value

a
[`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md)
containing vectors with the descriptor data for the requested
hemisphere(s). The length of the vectors is the number of vertices in
the surface, and the value for a vertex is the mean geodesic distance to
all other vertices for this vertex.

## Note

This takes quite a while for full-resolution meshes. Use down-sampled
versions to quickly try it (see example).

## Examples

``` r
if (FALSE) { # \dontrun{
  download_fsaverage3(TRUE);
  sjd = fsaverage.path();
  dist = subject.descriptor.geodesic.average.distance(sjd,
    "fsaverage3", surface = "white", hemi = "both");
  vis.data.on.subject(sjd, "fsaverage3", morph_data_lh = dist$lh);
} # }
```
