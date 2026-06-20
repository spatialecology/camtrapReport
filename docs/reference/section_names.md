# Get available report section names

Returns the names of available report sections/modules. Users can
optionally keep only selected sections or exclude selected sections.

## Usage

``` r
section_names(keep, exclude)

# S4 method for class 'ANY'
section_names(keep, exclude)
```

## Arguments

- keep:

  Optional character vector of section/module names to keep.

- exclude:

  Optional character vector of section/module names to exclude.

## Value

A character vector of section/module names.
