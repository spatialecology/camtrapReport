# Generating automated reports based on a `camReport` object

These functions facilitate generating two reports (data quality status,
and ecological insights) automatically. Each report is an HTML rendered
by creating an RMarkdown file based on the modules added to the
`camReport` object.

## Usage

``` r
report(object,filename,view)

status(object,filename,view)
```

## Arguments

- object:

  The camReport object created by the `camData` function

- filename:

  optional (default is "report"); a character specifying filename or
  path/filename to write the report and rmarkdown files. The default
  location to write the files is the data folder

- view:

  logical (default = TRUE); specifies whether the renderred html file
  should be displayed!

## Details

By default (if filename is not specified), two files including
"report.html" and "report.rmd" ("data_status.html" and "data_status.rmd"
for the data status report) will be written in the folder of the
camera-trap dataset.

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

report(cm, view=T) # ecological insights

status(cm, view=T) #  data quality status



} # }
```
