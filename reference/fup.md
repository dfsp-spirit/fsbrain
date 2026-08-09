# Transform first character of a string to uppercase.

Transform first character of a string to uppercase. This is useful when
labeling plots. Important: this function does not know about different
encodings, languages or anything, it just calls
[`toupper`](https://rdrr.io/r/base/chartr.html) for the first character.

## Usage

``` r
fup(word)
```

## Arguments

- word, :

  string. Any string.

## Value

string, the input string with the first character transformed to
uppercase.

## Examples

``` r
   word_up = fup("word");
```
