# Create camera trap data object

The camera-trap data, read as camtraptor data package, is the input.

## Usage

``` r
camData(data,habitat,study_area,...)
```

## Arguments

- data:

  either character which is the filename of the cameratrap data (as ZIP
  or Json file), or a datapackage object read through camtraptor (the
  `read_camtrap_dp` function)

- habitat:

  data.frame of habitat types

- study_area:

  either name of a shapefile or a SpatVector object defining spatial
  boundary of a study site.

- ...:

  additional arguments

## Details

The records of the input data package are used to build a camReport
object, containing processed camera-trap data and texts, graphs, etc.
used to automate generating a report.

## Value

a ReferenceClass

## References

ebrahimni et al. XXX

## Author

Elham Ebrahimi <eebrahimi.bio@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{

# filename of dataset: "veleuw.zip"

habitat <- read.csv('hanitat.csv')

cm <- camData("veleuw.zip",habitat,study_area='study_area.shp')

cm

} # }
```
