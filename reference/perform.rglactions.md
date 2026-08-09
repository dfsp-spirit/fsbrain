# Perform rglactions, like taking screenshots.

Take a list specifying actions, see
[`rglactions`](https://dfsp-spirit.github.io/fsbrain/reference/rglactions.md),
and execute them. This function should be called once an rgl scene has
been setup and rendered. A typical use case is to save a screenshot of
the scene.

## Usage

``` r
perform.rglactions(rglactions, at_index = NULL, silent = TRUE, ignore = c())
```

## Arguments

- rglactions, :

  named list. A list in which the names are from a set of pre-defined
  actions. The values can be used to specify parameters for the action.
  See
  [`rglactions`](https://dfsp-spirit.github.io/fsbrain/reference/rglactions.md).

- at_index:

  integer, the index to use in case of vectorized entries. Allows using
  different output_images for different views or similar.

- silent:

  logical, whether to suppress messages

- ignore:

  vector of character strings, actions to ignore
