# Return the name of available report sections or update/specify the report sections

This function can help to select a subset of sections for generating the
report.

## Usage

``` r
section_names(keep,exclude)

sections(x,n)
```

## Arguments

- keep:

  a character vector (default = 'all') specifies which report sections
  should be kept in the report

- exclude:

  an optional character vector to specify which sections should be
  excluded

- x:

  The \`camReport\` object created using the `camData` function

- n:

  a character vector to specify the names of report sections to update
  which report sections should be used in the report

## Details

The \`section_names\` function can help to retrieve and specify the
right section names in the \`sections\` function

## References

Ebrahimi et al. (2025) xxx

## Author

Elham Ebrahimi

## See also

updateReportSections

## Examples

``` r

if (FALSE) { # \dontrun{
cm <- camData('file.zip')

n <- section_names()

n

section_name(cm,n)

report(cm)

} # }
```
