# Read tract (streamline) files into named bundles.

Reads one or more tract files in TRK or TCK format (see the details) and
returns them as a named list of bundles, i.e., one entry per file. This
is the input format of
[`vis.tracts`](https://dfsp-spirit.github.io/fsbrain/reference/vis.tracts.md).
A single directory can be passed to read a whole tract atlas (one file
per bundle, e.g. the XTRACT atlas, see
[`download_xtract_tracts`](https://dfsp-spirit.github.io/fsbrain/reference/download_xtract_tracts.md)).

## Usage

``` r
read.tract.bundles(
  paths,
  coords = "ras",
  transform_matrix = NULL,
  max_tracks = Inf,
  skip_tracks = 0L,
  bbox = NULL,
  pattern = "\\.(trk|tck)(\\.gz)?$",
  silent = FALSE
)
```

## Arguments

- paths:

  character string, either the path to a directory containing tract
  files, or a vector of paths to tract files.

- coords:

  character string or NULL, the coordinate system of the returned
  coordinates for TRK files, see
  [`read.dti.trk`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.trk.html).
  The default 'ras' returns RAS+ mm coordinates, which is required to
  combine the tracts with the fsbrain template surfaces: 'native'
  returns the coordinates as stored, which for TRK files is usually a
  voxel grid, so the result will be mirrored when plotted. Note that
  this parameter has no effect for TCK files, which do not store a
  transformation and are always returned as stored.

- transform_matrix:

  a 4x4 numeric matrix or NULL. If given, it is applied to the
  coordinates of all files after reading them, e.g. to move a tract
  atlas from MNI space into the space of the surfaces, see the note in
  [`vis.tracts`](https://dfsp-spirit.github.io/fsbrain/reference/vis.tracts.md).

- max_tracks:

  numeric, the maximum number of streamlines to read per file. Use `Inf`
  (the default) to read all of them. Reading a subset is the way to
  handle whole-brain tractograms, which can contain millions of
  streamlines and cannot be plotted (or held in memory) as a whole.

- skip_tracks:

  integer, the number of streamlines to skip per file, see
  [`read.dti.tck`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.tck.html).

- bbox:

  numeric vector of length 6 or NULL, a bounding box to select the
  streamlines that pass through a region, see
  [`read.dti.tck`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.tck.html).
  The box is interpreted in the coordinate system given by `coords`.

- pattern:

  character string, a regular expression to select the files in a
  directory. Ignored if `paths` is not a directory.

- silent:

  logical, whether to suppress the progress messages.

## Value

named list of `fs.tracts` instances, one per file (see
[`fs.tracts`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.tracts.html)).
The names are the file names without the extension. The attribute
'files' contains the file paths, and the attribute 'coords_space' the
coordinate system of the coordinates ('ras' or 'native', see the
parameter `coords`). An entry which was read from a TCK file is a TCK
entry, i.e., its `[[i]]` entries are coordinate matrices, while the
entries of a TRK file also contain the per-point scalars and per-track
properties of the file.

## Details

The files are read with the readers of the 'freesurferformats' package.
Both the TrackVis TRK format (used by FSL, DSI Studio, AFQ, TractSeg and
the XTRACT atlas) and the MRtrix3 TCK format (used by MRtrix3, QSIRecon,
MRtrix3_connectome) are supported; TCK files may be gzip-compressed, TRK
files may not. The bundle names are the file names without the
extension, e.g. 'CST_L.trk' becomes the bundle 'CST_L'.

Note that TRK files store a transformation matrix which is not
necessarily the identity: a TRK file can contain coordinates in a voxel
grid (e.g. all files of the XTRACT atlas do). With the default
`coords = "ras"`, the coordinates are returned in RAS+ mm space, which
is what the template surfaces of fsbrain are in, see the parameter
documentation. Pass `coords = "native"` to get the coordinates exactly
as stored in the file.

## See also

Other tracts functions:
[`download_xtract_tracts()`](https://dfsp-spirit.github.io/fsbrain/reference/download_xtract_tracts.md),
[`vis.tracts()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.tracts.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Read a whole tract atlas from a directory, one bundle per file:
  atlas_dir = file.path(get_optional_data_filepath("tracts"), "xtract_tiny");
  bundles = read.tract.bundles(atlas_dir);
  names(bundles);
  fs.tracts.lengths(bundles$CST_L);

  # Read a single bundle:
  cst = read.tract.bundles(file.path(atlas_dir, "CST_L.trk"));
} # }
```
