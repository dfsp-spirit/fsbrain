# Load an annotation for a subject.

Load a brain surface annotation, i.e., a cortical parcellation based on
an atlas, for a subject.

## Usage

``` r
subject.annot(subjects_dir, subject_id, hemi, atlas)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier

- hemi:

  string, one of 'lh' or 'rh'. The hemisphere name. Used to construct
  the names of the annotation and morphometry data files to be loaded.

- atlas:

  string. The atlas name. E.g., "aparc", "aparc.2009s", or
  "aparc.DKTatlas". Used to construct the name of the annotation file to
  be loaded.

## Value

the annotation, as returned by
[`read.fs.annot`](https://rdrr.io/pkg/freesurferformats/man/read.fs.annot.html).
It is a named list, enties are: "vertices" vector of n vertex indices,
starting with 0. "label_codes": vector of n integers, each entry is a
color code, i.e., a value from the 5th column in the table structure
included in the "colortable" entry (see below). "label_names": the n
brain structure names for the vertices, already retrieved from the
colortable using the code. "hex_colors_rgb": Vector of hex color for
each vertex. The "colortable" is another named list with 3 entries:
"num_entries": int, number of brain structures. "struct_names": vector
of strings, the brain structure names. "table": numeric matrix with
num_entries rows and 5 colums. The 5 columns are: 1 = color red channel,
2=color blue channel, 3=color green channel, 4=color alpha channel,
5=unique color code. "colortable_df": The same information as a
dataframe. Contains the extra columns "hex_color_string_rgb" and
"hex_color_string_rgba" that hold the color as an RGB(A) hex string,
like "#rrggbbaa".

## See also

Other atlas functions:
[`get.atlas.region.names()`](https://dfsp-spirit.github.io/fsbrain/reference/get.atlas.region.names.md),
[`group.agg.atlas.native()`](https://dfsp-spirit.github.io/fsbrain/reference/group.agg.atlas.native.md),
[`group.agg.atlas.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/group.agg.atlas.standard.md),
[`group.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/group.annot.md),
[`group.label.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/group.label.from.annot.md),
[`label.from.annotdata()`](https://dfsp-spirit.github.io/fsbrain/reference/label.from.annotdata.md),
[`label.to.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/label.to.annot.md),
[`regions.to.ignore()`](https://dfsp-spirit.github.io/fsbrain/reference/regions.to.ignore.md),
[`spread.values.over.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/spread.values.over.annot.md),
[`spread.values.over.hemi()`](https://dfsp-spirit.github.io/fsbrain/reference/spread.values.over.hemi.md),
[`spread.values.over.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/spread.values.over.subject.md),
[`subject.atlas.agg()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.atlas.agg.md),
[`subject.label.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.label.from.annot.md),
[`subject.lobes()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.lobes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   annot_lh = subject.annot(subjects_dir, "subject1", "lh", "aparc");
} # }
```
