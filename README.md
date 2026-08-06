<p align="center">
  <img src="inst/report-assets/camtrapReport-logo.png" width="200" alt="camtrapReport"/>
</p>

<h1 align="center">camtrapReport</h1>

<p align="center">
  <em>A modular R package for automating camera-trap data reporting in wildlife monitoring.</em>
</p>

<p align="center">
  <a href="https://github.com/spatialecology/camtrapReport/actions/workflows/R-CMD-check.yaml"><img src="https://github.com/spatialecology/camtrapReport/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R-CMD-check"></a>
  <a href="https://app.codecov.io/github/spatialecology/camtrapReport"><img src="https://codecov.io/github/spatialecology/camtrapReport/graph/badge.svg?token=9VBXAR9XOD" alt="Codecov test coverage"></a>
  <a href="https://www.repostatus.org/#active"><img src="https://www.repostatus.org/badges/latest/active.svg" alt="Project Status: Active"></a>
  <a href="https://www.r-project.org/"><img src="https://img.shields.io/badge/R-%E2%89%A5%204.1.0-276DC3?logo=r&logoColor=white" alt="R ≥ 4.1.0"></a>
  <a href="https://github.com/spatialecology/camtrapReport/blob/main/LICENSE.md"><img src="https://img.shields.io/badge/license-MIT-yellow.svg" alt="MIT"></a>
  <a href="https://doi.org/10.5281/zenodo.18405441"><img src="https://img.shields.io/badge/DOI-10.5281%2Fzenodo.18405441-blue.svg" alt="DOI"></a>
  <a href="https://spatialecology.github.io/camtrapReport/"><img src="https://img.shields.io/badge/docs-pkgdown-orange.svg" alt="pkgdown"></a>
</p>

---

`camtrapReport` turns standardised camera-trap datasets into structured, reproducible ecological reports through an automated workflow. Drop in a [Camtrap DP](https://camtrap-dp.tdwg.org/) ZIP file, and the package will diagnose data quality, run a suite of ecological analyses, and compile narrative text, figures, maps, and tables into a single article-style HTML document.

<p align="center">
  <img src="vignettes/articles/figures/workflow.png" width="900" alt="Schematic overview of the camtrapReport workflow"/>
  <br>
  <em>Schematic overview of the automated <code>camtrapReport</code> workflow.</em>
</p>


## Install

```r
remotes::install_github("spatialecology/camtrapReport")
library(camtrapReport)
install_All()   # Install all package dependencies required for full functionality
```

## Create the camReport object

The only required input is a single `.zip` file containing the dataset in Camtrap DP format.

```r
cm <- camData("Leuven-data.zip")   # build the camReport object
```

## Optional input

Optional inputs can be supplied directly to `camData()`, including habitat data as a CSV file ([see template](https://drive.google.com/file/d/1lo_CwpLQmuxOVB5193tIAsEq7WF9v0t-/view?usp=sharing)) and the study-area boundary as a polygon shapefile.

```r
habitat <- read.csv("C:/Users/Data/habitat.csv")

head(habitat) # check if the data.frame follows the required structure:
#   locationName      Habitat
# 1       VEL-01     Sandhill
# 2       VEL-02       Forest
# 3       VEL-03 Dry_heathland

# Spatial polygon of the study area:
bnd <- vect("C:/Users/Data/polygon.shp")

# Read the camera-trap data together with habitat data and the study area boundary:
cm <- camData(
  data = "C:/Users/Data/Leuven-data.zip",
  habitat = habitat,
  study_area = bnd
)

cm # shows brief information about the camReport object
```

Once the `camReport` object is built, two reports can be generated: the **Data Status Check** and the **Ecological Insight Report**.

## Data status report

To generate a Data Status Report and review the quality and completeness of the input data, use `status()`:

```r
status(cm, view = TRUE)  # With view = TRUE, the generated report opens automatically
```

## Ecological report

Once the input data have been prepared, a full ecological report can be generated using the `report()` function:

```r
report(cm, view = TRUE)  # Opens the report automatically after creation
```

> **Tip:** Reports are saved in your current working directory.

## Customising the report

The contents of the `camReport` object are extracted or inferred from the main dataset. These metadata can be viewed and modified using the `info()` function:

```r
info(cm)  # Shows metadata for available fields

# You can also retrieve information for specific fields:
info(cm, name = c("title", "subtitle"))
info(cm, name = "authors")

# You can override the information:
info(cm, name = "authors") <- c("Elham Ebrahimi", "Patrick Jansen")

# View the updated metadata:
info(cm)
```

## Adjusting the focus group

The Ecological Report can be generated for a selected taxonomic group, referred to as the `focus_group`. By default, this is set to `"large_mammals"`.

Several predefined groups are already included in the package, such as `"large_mammals"`, `"wild_animals"`, `"birds"`, `"amphibians"`, `"domestic"`, and `"human_observation"`.

Users can also define a new group by assigning records based on one or more of the following fields:

- `scientificName`
- `class`
- `order`
- `observationType`

```r
# Check which group is currently set as the focus group
cm$setting$focus_groups

# Check which groups are available
names(cm$group_definition)

# Change the focus group
cm$set_focus_group(x = "wild_animals")

# Check the rule used to define specific groups
cm$get_group("large_mammals")
cm$get_group("wild_mammals")

# Add a new group or modify an existing group definition
cm$add_group(
  name = "wild_animals",
  x = list(
    scientificName = c(
      "Mustela putorius",
      "Myodes glareolus",
      "Procyon lotor"
    )
  )
)

cm$setup()
```

## Selecting years and adjusting filters

The report can be generated for all years in the dataset or for a selected time period.

```r
# Check which years are covered by the records
cm$extractYears()

# Select a specific time window for report generation
cm$years <- 2023:2024

cm$setup()
```

The count filter defines the minimum number of observations required for a species to be included in the report.

```r
# Check the current count filter
cm$filterCount

# The default value is usually 25

# Modify the count filter
cm$filterCount <- 10
```

## Viewing and updating report sections

`camtrapReport` is a modular and extensible package, which means each report is built from independent sections.

You can inspect the available sections before editing the report. This helps you identify the exact section names that can be modified, updated, kept, or excluded.

The function `updateReportSection()` can be used to edit the content of an existing report section. For example, if the text in the introduction section needs to be changed, it can be updated as shown below.

```r
# View available report sections
listReportSections(cm)
sections(cm)
section_names(cm)

# Update the text of a specific report section
updateReportSection(
  cm,
  section = "introduction",
  text = "This is new text to replace the existing section content.",
  append_text = FALSE
)

# If append_text = TRUE, the new text is added to the existing text.
# If append_text = FALSE, the existing text is replaced.

# Check the report sections again after making changes
listReportSections(cm)
```

## Selecting which sections to include

Before generating the report, you can check which sections will be included.

If you want to exclude or keep certain sections, you can use the `sections()` function. In `sections()`, you specify the names of the sections that should be included in the report.

To make it easier to access section names, use the `section_names()` function.

```r
# Check what sections will be included in the report
listReportSections(cm)

# Show the names of all existing sections in the package
section_names()

# Exclude one section
section_names(exclude = "introduction")

# Exclude more than one section
section_names(exclude = c("acknowledgements", "appendix"))

# Keep only selected sections
section_names(
  keep = c("introduction", "methods", "study_area")
)

# Example:
# Include all sections except "richness" and "co_occurrence"
n <- section_names(
  exclude = c("richness", "co_occurrence")
)

# Check the names of the sections that will be used
n

# Update the sections in the cm object
sections(cm, n)

# Generate the report with the selected sections
report(cm, view = TRUE)

# Restore all available sections
sections(cm, n = section_names())
```

## Graphical user interface

A graphical user interface is available for exploring outputs, adjusting settings and generating reports interactively.

```r
gui(cm)
```

## Privacy-aware by design

Reports can be shared even when raw images or precise locations cannot — broadening the range of monitoring programmes that can contribute results to research, management and policy.

## Example data

Try the workflow using the open [Leuven Camtrap DP dataset](https://drive.google.com/file/d/1l-nSJKopM9agJgtTCzTx3tQiP8aTYH5c/view?usp=sharing). This example includes camera-trap data files based on the original Camtrap DP dataset, which is available from [GBIF](https://doi.org/10.15468/4u3wm4), together with optional supporting files such as `habitat.csv` and a study-area boundary shapefile.

This is a relatively large dataset, covering multiple years and more than 300 camera locations, so preprocessing and report generation may take some time.

## Learn more

[Package overview](https://spatialecology.github.io/camtrapReport/articles/Package-Overview.html) · [Data Status Report](https://spatialecology.github.io/camtrapReport/articles/data-status-report.html) · [Ecological Report](https://spatialecology.github.io/camtrapReport/articles/ecological-report.html) · [Module management](https://spatialecology.github.io/camtrapReport/articles/modules.html)

## Existing Packages to Work with Camera-trap Data

The R ecosystem includes complementary tools for different stages of the
camera-trap data lifecycle. [`camtrapdp`](https://inbo.github.io/camtrapdp/)
supports reading, validating, filtering, and transforming Camtrap DP datasets,
whereas [`camtraptor`](https://inbo.github.io/camtraptor/) and
[`ctdp`](https://git.wur.nl/camtrap/ctdp) support their exploration,
summarisation, and visualisation. 
[`camtrapDensity`](https://github.com/MarcusRowcliffe/camtrapDensity) provides
methods for estimating density from camera-trap data using Random Encounter
Model approaches. [`camtrapR`](https://cran.r-project.org/package=camtrapR)
provides a broader framework for media and metadata organisation, record-table
construction, detection histories, survey summaries, and ecological analyses,
and can import Camtrap DP records into its own internal structures. Other
packages provide integrated workflows or specialised methods, including `ct`
for camera-trap data processing and selected activity, diversity, abundance,
and density analyses; `activity` and `overlap` for diel activity patterns;
`Distance` for distance-sampling analyses; `iNEXT` for diversity interpolation
and extrapolation; and `unmarked` and `secr` for hierarchical occurrence,
abundance, and spatial capture–recapture models.

`camtrapReport` addresses a different organising challenge: it makes the
complete ecological report, rather than an individual dataset transformation
or statistical analysis, the reproducible unit of work. Starting from a
standardised Camtrap DP dataset, it coordinates data-quality assessment,
harmonised preprocessing, analytical settings, ecological modules, metadata,
provenance, visual outputs and data-informed narrative through a shared
`camReport` object. Its report-centred architecture maintains an explicit
connection between the underlying records, processing decisions, analytical
results and final communication product. The modular design further allows
users to select, omit, reorder or customise report sections and to develop,
register and reuse new analytical modules without modifying the package core.
`camtrapReport` therefore extends the existing camera-trap software ecosystem
with an auditable and extensible pathway from standardised records to coherent,
reproducible and shareable ecological synthesis.

## Citation

To cite `camtrapReport` in publications, run:

```r
citation("camtrapReport")
```

## Contributing

Contributions to `camtrapReport` are very welcome. These may include bug
reports, feature requests, documentation improvements, code contributions,
or proposals for new report modules.

You can contribute by opening a
[GitHub issue](https://github.com/spatialecology/camtrapReport/issues)
or starting a
[discussion](https://github.com/spatialecology/camtrapReport/discussions). Before contributing, please read the
[contributing guidelines](https://github.com/spatialecology/camtrapReport/blob/main/.github/CONTRIBUTING.md)
and the
[Code of Conduct](https://github.com/spatialecology/camtrapReport/blob/main/.github/CODE_OF_CONDUCT.md).