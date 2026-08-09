# Perform tests for group differences on paired or unpaired data for two groups.

This function is intended to give you a quick overview of your
demographics data, it is in no way intended to replace a detailed
analysis of your data. You should always visualize and analyze your data
interactively instead of relying on automated methods like this.
Outliers and are very common in real-world data while perfectly normal
data is very rare, multiple testing may affect your results. Look at
your data!

## Usage

``` r
test.numerical.meandiff(
  colname,
  group1_name,
  group2_name,
  group1_data_column,
  group2_data_column,
  paired
)
```

## Arguments

- colname:

  string, the name of the data (used to label the data in the output)

- group1_name:

  string, the name of the first group (used to label the data in the
  output)

- group2_name:

  string, the name of the first group (used to label the data in the
  output)

- group1_data_column:

  the data for group1 as a numerical vector. Typically a column from
  your demographics dataframe.

- group2_data_column:

  the data for group2 as a numerical vector. Typically a column from
  your demographics dataframe.

- paired:

  logical, whether the data is paired (repeated measures).

## Value

vector of strings, the lines of the report. You can print to STDOUT or
write it to a file.
