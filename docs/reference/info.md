# Get or set field values in a `camReport` object

This function facilitates to extract specific fields (e.g., title,
authors, etc.) from a `camReport` object, or update the value of a
field.

## Usage

``` r
# S4 method for class 'camReport'
info(x, name)

# S4 method for class 'camReport'
info(x, name) <- value
```

## Arguments

- x:

  A `camReport` object

- name:

  character; names of the field from/to which the information is
  retrieved/assigned

- value:

  the new value to assign to the specified field (name)

## Details

The camReport object,created by the `camData` function, contains
information extracted from the camera-trap dataset. To control some of
these details go to report (e.g., title, subtitle, authors, institute,
siteName, description, sampling,...), a user can check their values
using the `info` function or update the value of a certain field.

## References

Ebrahimi et al. (2025) xxx

## Author

Elham Ebrahimi

## See also

camData

## Examples

``` r

if (FALSE) { # \dontrun{
cm <- camData("data-folder")

info(cm)

xi <- info(cm, name=c('title','authors'))

xi

info(cm,'title') <- 'This is NEW title...'

} # }
```
