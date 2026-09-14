# Reshape aggregated region data from long to wide format.

Internal helper. Takes a long-format dataframe with one row per subject
and atlas region (as returned by
[`subject.atlas.agg`](https://dfsp-spirit.github.io/fsbrain/reference/subject.atlas.agg.md))
and reshapes it into a wide dataframe with one row per subject and one
column per atlas region, plus a leading character column named 'subject'
which holds the subject identifiers. Region columns are sorted
alphabetically. Cells for which no data is available are set to NA.

## Usage

``` r
agg.res.long.to.wide(agg_all_subjects, subjects_list, agg_fun = NULL)
```

## Arguments

- agg_all_subjects, :

  dataframe in long format. Must contain the columns 'subject', 'region'
  and 'aggregated'. Each combination of 'subject' and 'region' must be
  unique.

- subjects_list, :

  vector of character strings, the subject identifiers. Also determines
  the row order of the result.

- agg_fun, :

  function or NULL. If given, it is used to aggregate all values which
  occur for the same subject and region combination. If NULL, the single
  value for each combination is used directly.

## Value

dataframe in wide format, with one row per subject. The first column is
named 'subject' and holds the subject identifiers, the remaining columns
are named after the atlas regions and hold the aggregated values.
