# Generate skeleton dataframe for FreeSurfer QDEC long file from subjects list.

Generate skeleton dataframe for FreeSurfer QDEC long file from subjects
list.

## Usage

``` r
qdec.table.skeleton(
  subjects_list,
  isi = rep(0.8, length(subjects_list)),
  isi_name = "years",
  timepoint_names = c("_MR1", "_MR2")
)
```

## Arguments

- subjects_list:

  vector of character strings, the Freesurfer subject IDs
  (cross-sectional names, without any suffixes like `_MR1, long,` etc.)

- isi:

  numerical vector, the inter-scan interval for the subjects, in a unit
  of your choice. Typically in years.

- isi_name:

  character string, the name for the isi columns. Defaults to "years".

- timepoint_names:

  vector of character strings, the timepoint names. These are mandatory
  for QDEC, so there should be very little reason to change them. Leave
  along unless you know what you are doing.

## Value

data.frame with 3 columns named fsid and fsid-base and 'isi_name', a
data.frame to use with the
[`demographics.to.qdec.table.dat`](https://dfsp-spirit.github.io/fsbrain/reference/demographics.to.qdec.table.dat.md)
function.

## See also

The function
[`demographics.to.qdec.table.dat`](https://dfsp-spirit.github.io/fsbrain/reference/demographics.to.qdec.table.dat.md)
to write the result to a QDEC file.

## Examples

``` r
    dem = data.frame("ID"=paste("subject", seq(5), sep=""),
      "age"=sample.int(20, 5)+10L, "isi"=rnorm(5, 2.0, 0.1)); #sample data.
    qdec.table.skeleton(dem$ID, dem$isi);
#>            fsid fsid-base    years
#> 1  subject1_MR1  subject1 0.000000
#> 2  subject1_MR2  subject1 2.052919
#> 3  subject2_MR1  subject2 0.000000
#> 4  subject2_MR2  subject2 2.080892
#> 5  subject3_MR1  subject3 0.000000
#> 6  subject3_MR2  subject3 1.957278
#> 7  subject4_MR1  subject4 0.000000
#> 8  subject4_MR2  subject4 1.813419
#> 9  subject5_MR1  subject5 0.000000
#> 10 subject5_MR2  subject5 1.973049
```
