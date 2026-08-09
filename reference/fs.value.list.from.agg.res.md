# Create a named value list from a dataframe.

Given the result of the group.agg.atlas.native() function, extract a
named region value list (typically for use with the
spread.values.over.annot() function) for a single subject.

## Usage

``` r
fs.value.list.from.agg.res(agg_res, subject_id)
```

## Arguments

- agg_res, :

  a dataframe. The result of calling group.agg.atlas.native().

- subject_id, :

  string. A subject identifier, must occur in the subject column of the
  dataframe agg_res.

## Value

region_value_list, named list of strings. Each name must is a region
name from the annotation, and the value is a scalar that resulting from
aggregating the morphometry data for that region and subject.
