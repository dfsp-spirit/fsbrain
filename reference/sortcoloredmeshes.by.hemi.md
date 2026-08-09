# Sort coloredmeshes into 2 lists by their 'hemi' property.

Sort coloredmeshes into 2 lists by their 'hemi' property.

## Usage

``` r
sortcoloredmeshes.by.hemi(coloredmeshes)
```

## Arguments

- coloredmeshes:

  list of coloredmeshes or other renderables

## Value

named list with two entries: "lh": list of coloredmeshes that have
property hemi set to 'lh'. "rh": list of coloredmeshes that have
property hemi set to 'rh'. The rest is ignored.
