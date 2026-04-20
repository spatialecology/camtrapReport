# Updating content of report sections

This function can be used to update the content (text, code) of a
section (module) within a `camReport` object.

## Usage

``` r
updateReportSection(x,section,text,title,code,code_name,code_setting,packages,append_text,append_code)
  
  listReportSections(x)
```

## Arguments

- x:

  A camReport object created by the `camData` function

- section:

  A character to refer to a report section (name or title of the report)

- text:

  optional, to update the text body of the report section

- title:

  optional, a new title for the section

- code:

  optional, to update or add the R code chunk

- code_name:

  optional, if R code chunk is provided, its name can be specified

- code_setting:

  optional, setting for the new R chunk

- packages:

  optional, name of R packages required by the new R chunk

- append_text:

  optional; whether the new text should be appended to the previous text
  (default: FALSE)

- append_code:

  optional; whether the new R code should be appended to the previous R
  code (default: FALSE)

## Details

To find a report section, either title or name can be used in the
`section` argument. The `listReportSections` is useful to list all
sections added to a camReport object and check its exact name.

## References

Ebrahimi et al. (2025) xxx

## Author

Elham Ebrahimi

## See also

info

## Examples

``` r

if (FALSE) { # \dontrun{
cm <- camData("data-folder")

listReportSections(cm)



} # }
```
