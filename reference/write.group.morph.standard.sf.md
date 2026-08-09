# Reshape and write combined per-vertex data for a group to a single MGH file.

Write morphometry data for a group into a single MGH or MGZ file. In
neuroimaging, the first 3 dimensions in the resulting 4D volume file are
space, and the 4th is the time/subject dimension.

## Usage

``` r
write.group.morph.standard.sf(filepath, data)
```

## Arguments

- filepath:

  character string, path to the target file, should end with '.mgh' or
  '.mgz'.

- data:

  numerical 2D matrix, with the rows identifying the subjects and the
  columns identifying the vertices.

## Note

The file will contain no information on the subject identifiers. The
data can be for one or both hemispheres. See
[`group.morph.standard.sf`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.standard.sf.md)
to read the data back into R.

## Examples

``` r
if (FALSE) { # \dontrun{
# create per-vertex data for 5 subjects.
mat = matrix(rnorm(5 * 163842, 3.0, 0.5), nrow=5, ncol = 163842);
fsbrain::write.group.morph.standard.sf("~/group_pvd.mgz", mat);
} # }
```
