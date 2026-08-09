# Perform tests for group differences on paired data (repeated measurements) for two conditions or time points.

This function is intended to give you a quick overview of your
demographics data, it is in no way intended to replace a detailed
analysis of your data. You should always visualize and analyze your data
interactively instead of relying on automated methods like this.
Outliers and are very common in real-world data while perfectly normal
data is very rare, multiple testing may affect your results. Look at
your data!

## Usage

``` r
test.numerical.meandiff.paired(
  colname,
  condition1_name,
  condition2_name,
  condition1_data_column,
  condition2_data_column
)
```

## Arguments

- colname:

  string, the name of the data (used to label the data in the output)

- condition1_name:

  string, the name of the first condition (used to label the data in the
  output)

- condition2_name:

  string, the name of the first condition (used to label the data in the
  output)

- condition1_data_column:

  the data for condition1 as a numerical vector. Typically a column from
  your demographics dataframe.

- condition2_data_column:

  the data for condition2 as a numerical vector. Typically a column from
  your demographics dataframe.

## Value

vector of strings, the lines of the report. You can print to STDOUT or
write it to a file.
