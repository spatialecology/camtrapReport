---
title: Get started
---
# Get started

## Package aims

[**`camtrapReport`**](https://github.com/spatialecology/camtrapReport) helps turn camera-trap data into standardised, reproducible ecological outputs through a workflow designed to automate as much of the reporting process as possible. The package combines data quality checks, summarisation, visualisation, and ecological analysis in a modular framework, helping users move efficiently from raw records to interpretable results. Its automated workflow improves comparability, scalability, and reproducibility across sites and over time, while reducing technical barriers and supporting transparent, efficient, and timely ecological reporting. The main functions of the package include:

-   **Data status report**, which performs automated diagnostics of data quality and completeness across key components of camera-trap datasets, including spatial and temporal information, key column availability, annotations, validation status, and observation types by capture method. These checks help users identify missing information, inconsistencies, and unusual values that may require correction or completion.

-   **Ecological report**, which produces standardised outputs through modular components. Reports can include descriptive information, summaries of key data, visualisations of important patterns, and ecological assessments and analyses.


## Data standard format

`camtrapReport` is designed for datasets provided in, or converted to, the [Camtrap-DP standard format](https://camtrap-dp.tdwg.org/), which is a community-developed data exchange format for camera-trap data. Several camera-trap data management systems support Camtrap-DP as an export format, including [Agouti](https://agouti.eu/) and [TRAPPER](https://os-conservation.org/trapper/). It is also possible to convert data from other camera-trap sources, including manually managed datasets and Wildlife Insights exports, into Camtrap-DP format for use in `camtrapReport`.

### Example input datasets

Several open-source camera-trap datasets based on the Camtrap-DP standard are available through [GBIF](https://www.gbif.org/composition/4fZGV2vrXjo3rNxySz41sj/exploring-camera-trap-data) and can be useful for testing `camtrapReport`.

::: {style="columns: 2; -webkit-columns: 2; -moz-columns: 2;"}
<ul>

<li><a href="https://album.wildlabs.net/dataset/c9cbc586-660e-4d89-ba14-0000c5770de1/download">Leuven dataset</a></li>

<li><a href="https://album.wildlabs.net/dataset/a209cef2-cfad-460b-8ed4-0ccf211a8240/download">Antwerp dataset</a></li>

<li><a href="https://album.wildlabs.net/dataset/8a5cbaec-2839-4471-9e1d-98df301095dd/download">MICA dataset</a></li>

<li><a href="https://album.wildlabs.net/dataset/3856c01f-5031-4cc1-a5b2-2daa9537411b/download">Colombia dataset</a></li>

<li><a href="https://album.wildlabs.net/dataset/13101e81-bc62-4553-9fd9-c5c8eb3fb9ab/download">Alpine-Tundra dataset</a></li>

<li><a href="https://album.wildlabs.net/dataset/f0a42d7d-1eda-4ec8-ac66-c1343acea3bc/download">Snapshot-Japan dataset</a></li>

<li><a href="https://album.wildlabs.net/dataset/f0963153-077b-4676-a337-891a06fab52a/download">Forest-Colombia dataset</a></li>

<li><a href="https://album.wildlabs.net/dataset/74196cd9-7ebc-4b20-bc27-3c2d22e31ed7/download">Amsterdam dataset</a></li>

<li><a href="https://album.wildlabs.net/dataset/77972fac-09bc-460b-a0d6-34b87b1b4b72/download">Cali dataset</a></li>

<li><a href="https://album.wildlabs.net/dataset/fc3f505a-05d8-4b3e-908c-8880fc9899f7/download">Luxembourg dataset</a></li>

</ul>
:::

## First use: installation

Install `camtrapReport` with the standard `remotes` workflow:

``` r
# install using the "remotes" package
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("spatialecology/camtrapReport")
```

### Load the package

In addition to loading `camtrapReport` package, depending on which report sections and methods you use, additional packages may be required. To ensure full functionality, run the helper function, `inatall_All`, that installs all package dependencies:

``` r
library(camtrapReport)
install_All()
```
*Tip: You may be prompted to approve package installations or compile packages from source. Make sure you have an active internet connection.*

## Create the camReport object
At the core of `camtrapReport` is a mutable Reference Class object called `camReport`, which acts as the central workflow container and module registry. It stores harmonised input data, user-defined settings, and intermediate results throughout the reporting process.

Use `camData()` to create the `camReport` object by reading and pre-processing your camera-trap dataset.

## Required input

To keep the workflow simple and reproducible, the only required input is a single `.zip` file containing the dataset in Camtrap-DP format.

```r
cm <- camData("C:/Users/ebrah010/Data/EOW-Veluwe.zip")
```
## Optional input

Additional input data can be provided to improve maps, add spatial context, and support more informative summaries and analyses.

### **Habitat data**

Habitat information may already be included in [deployment.csv](https://camtrap-dp.tdwg.org/data/#deployments). If it is missing, you can either add it in your data management systems (eg., [Agouti](https://agouti.eu/)) before exporting the dataset or provide it separately as a two-column CSV file with `locationName` and `Habitat`. An example template can be downloaded [here](https://drive.google.com/file/d/1lo_CwpLQmuxOVB5193tIAsEq7WF9v0t-/view?usp=sharing).

``` r
habitat <- read.csv("C:/Users/ebrah010/Data/habitat.csv")
head(habitat)
#   locationName      Habitat
# 1       VEL-01     Sandhill
# 2       VEL-02       Forest
# 3       VEL-03 Dry_heathland
```

### **Study area polygon**

A polygon shapefile of the site boundary can be provided to improve maps and add spatial context to the report.

``` r
study_area <- vect("C:/Users/ebrah010/Data/studyarea_nl.shp")
```

When optional input data are available, they can be supplied directly to `camData()`:

``` r
cm <- camData(
  "C:/Users/ebrah010/Data/EOW-Veluwe.zip",
  habitat = habitat,
  study_area = study_area
)
```

## Data status report

To generate a data status report and review the quality and completeness of the input data, use:

``` r
cm$generateStatusReport()
```

## Ecological report

Once the input data have been prepared, a full ecological report can be generated using `report()`:

``` r
report(cm, view = TRUE)  
# Open the report automatically after creation with view = TRUE
```

## Conclusion

This page introduced the basic workflow for setting up input data, reviewing data quality, and generating reports with `camtrapReport`. To explore the available options for customising the ecological report, see the [Package overview vignette](). For more detail on the package’s modular and extensible design, including how to add new report sections, see the [Module vignette]().

## Contribute

Questions, suggestions, and ideas for improvement are always welcome. You can contribute to the development of the [camtrapReport R package](https://github.com/spatialecology/camtrapReport) by [opening an issue](https://github.com/spatialecology/camtrapReport/issues) or joining the conversation in [GitHub Discussions](https://github.com/spatialecology/camtrapReport/discussions).

## Citation

As the official scientific publication for `camtrapReport` is still in preparation, please cite the package as:

[Ebrahimi, E., & Jansen, P. A. (2026). <em>CamtrapReport: An R Package for Automating Camera-Trap Data Reporting for Wildlife Monitoring</em>. p. 65. Abstract from <em>The International Biogeography Society – 12th Biennial Conference</em>, Aarhus, Denmark. <https://zenodo.org/records/18405441>]{.citation-text}

For package citation details, please refer to the citation information available in the [GitHub repository](https://github.com/spatialecology/camtrapReport).

## Acknowledgment

The development of `camtrapReport` was supported by [Biodiversa+](https://www.biodiversa.eu/2022/10/07/2022-2023-joint-call/) through the [Big Picture project](https://wildlifecamera.eu/).
