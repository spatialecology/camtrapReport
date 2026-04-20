---
title: "Get started"
---

# Get started


## Package aims

[**`camtrapReport`**](https://github.com/spatialecology/camtrapReport) helps turn camera-trap data into standardised, reproducible ecological outputs through a workflow designed to automate as much of the reporting process as possible. The package combines data quality checks, summarisation, visualisation, and ecological analysis in a modular framework, helping users move efficiently from raw records to interpretable results. Its automated workflow improves comparability, scalability, and reproducibility across sites and over time, while reducing technical barriers and supporting transparent, efficient, and timely ecological reporting. The main functions of the package include:

-   **Data status report**, which performs automated diagnostics of data quality and completeness across key components of camera-trap datasets, including spatial and temporal information, key column availability, annotations, validation status, and observation types by capture method. These checks help users identify missing information, inconsistencies, and unusual values that may require correction or completion.

-   **Ecological report**, which produces standardised outputs through modular components. Reports can include descriptive information, summaries of key data, visualisations of important patterns, and ecological assessments and analyses.


## Data standard format

`camtrapReport` is designed for datasets provided in, or converted to, the [Camtrap-DP standard format](https://camtrap-dp.tdwg.org/), which is a community-developed data exchange format for camera-trap data. Several camera-trap data management systems support Camtrap-DP as an export format, including [Agouti](https://agouti.eu/) and [TRAPPER](https://os-conservation.org/trapper/). It is also possible to convert data from other camera-trap sources, including manually managed datasets and Wildlife Insights exports, into Camtrap-DP format for use in `camtrapReport`.

### Example input datasets

Three example datasets are provided below. These open-access camera-trap datasets, available through [GBIF](https://www.gbif.org/composition/4fZGV2vrXjo3rNxySz41sj/exploring-camera-trap-data), follow the Camtrap-DP standard and can be used to test `camtrapReport`. 

| Dataset | Habitat data | Study area |
|:---:|:---:|:---:|
| [Leuven dataset](https://album.wildlabs.net/dataset/c9cbc586-660e-4d89-ba14-0000c5770de1/download) | [Leuven habitat](https://drive.google.com/file/d/1kVO3SztP4aeW53KIMJNQi5DDcGK3Wgsk/view?usp=sharing) | [Leuven study area boundary](https://drive.google.com/file/d/1frZsAFzxHtrXU98_5XFsBhSlbyf7quAe/view?usp=sharing) |
| [Antwerp dataset](https://album.wildlabs.net/dataset/a209cef2-cfad-460b-8ed4-0ccf211a8240/download) | [Antwerp habitat](https://drive.google.com/file/d/1ByUVZXc4w6JNFnMbgXEUu9ihJreIp7UJ/view?usp=sharing) | [Antwerp study area boundary](https://drive.google.com/file/d/1Avb-SRqYsL59mrBrcmNdIkS8f582UVkR/view?usp=sharing) |
| [MICA dataset](https://album.wildlabs.net/dataset/8a5cbaec-2839-4471-9e1d-98df301095dd/download) | [MICA habitat](https://drive.google.com/file/d/1-1i8Kw8AUPYpedme8e8t6GKUgqatR8ji/view?usp=sharing) | [MICA study area boundary](https://drive.google.com/file/d/1xskwg3H1vZw4gu-VDaiCHgXPXeoktDvH/view?usp=sharing) |

<p style="color: grey; font-size: 0.9em;">
  <em><strong>Note:</strong> The <a href="https://doi.org/10.1038/s41597-025-06235-7" style="color: grey; font-style: italic;">habitat data</a> and study area files were prepared exclusively for testing <code>camtrapReport</code> and do not represent official data products published by the original dataset owners.</em>
</p>


## First use: installation

Install `camtrapReport` with the standard `remotes` workflow:

``` r
# install using the "remotes" package
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("spatialecology/camtrapReport")
```
<p style="color: grey; font-size: 0.9em;">
  <em><strong>Note:</strong> <code>camtrapReport</code> requires R ≥ 4.1.0. If you are unsure of your current R version, run <code>R.version.string</code> in the R console.</em>
</p>

### Load the package

In addition to loading `camtrapReport` package, depending on which report sections and methods you use, additional packages may be required. To ensure full functionality, run the helper function, `install_All`, that installs all package dependencies:

``` r
library(camtrapReport)
install_All()
```
<p style="color: grey; font-size: 0.9em;">
  <em><strong>Note:</strong> You may be prompted to approve package installations or compile packages from source. Make sure you have an active internet connection.</em>
</p>

### Troubleshooting installation

If `remotes::install_github()` fails, a common cause is a missing 
GitHub personal access token (PAT). To set one up, run:

```r
install.packages(c("remotes", "usethis", "gitcreds"))
usethis::create_github_token()  # Opens GitHub in your browser — create and copy the token
gitcreds::gitcreds_set()        # Paste the token when prompted in R
remotes::install_github("spatialecology/camtrapReport")
```

If installation still fails, please [open an issue](https://github.com/spatialecology/camtrapReport/issues) and include the full error message, the output of `sessionInfo()`, and information on whether the error occurred during `install_github()` or `install_All()`.

## Create the camReport object
At the core of `camtrapReport` is a mutable Reference Class object called `camReport`, which acts as the central workflow container and module registry. It stores harmonised input data, user-defined settings, and intermediate results throughout the reporting process.

Use `camData()` to create the `camReport` object by reading and pre-processing your camera-trap dataset.

## Required input

To keep the workflow simple and reproducible, the only required input is a single `.zip` file containing the dataset in Camtrap-DP format.

```r
cm <- camData("path/to/your/dataset.zip")
```
## Optional input

Additional input data can be provided to improve maps, add spatial context, and support more informative summaries and analyses.

### **Habitat data**

Habitat information can be provided as a two-column CSV file with `locationName` and `Habitat`. An example template of habitat.csv can be downloaded [here](https://drive.google.com/file/d/1lo_CwpLQmuxOVB5193tIAsEq7WF9v0t-/view?usp=sharing).

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
  "path/to/your/dataset.zip",
  habitat = habitat,
  study_area = study_area
)
```

## Data status report

To generate a data status report and review the quality and completeness of the input data, use:

``` r
status (cm, view = TRUE)  # With view = TRUE, the generated report opens automatically
```
<p style="color: grey; font-size: 0.9em;">
  <em><strong>Example:</strong> <a href="https://drive.google.com/file/d/1frZsAFzxHtrXU98_5XFsBhSlbyf7quAe/view" style="color: grey; font-style: italic;">Data Status Report output</a></em>
</p>

## Ecological report

Once the input data have been prepared, a full ecological report can be generated using `report()`:

``` r
report(cm, view = TRUE)  
```
<p style="color: grey; font-size: 0.9em;">
  <em><strong>Example:</strong> <a href="https://drive.google.com/file/d/1frZsAFzxHtrXU98_5XFsBhSlbyf7quAe/view" style="color: grey; font-style: italic;">Ecological Report output</a></em>
</p>

## Conclusion

This page introduced the basic workflow for setting up input data, 
reviewing data quality, and generating reports with `camtrapReport`.
To understand how the package is organised, see the [Package Overview](https://spatialecology.github.io/camtrapReport/articles/Package-Overview.html). 
To explore data quality checks in detail, see the [Data Status Report](https://spatialecology.github.io/camtrapReport/articles/data-status-report.html). 
To customise the ecological report, see the [Ecological Report](https://spatialecology.github.io/camtrapReport/articles/ecological-report.html). 
For more detail on the package's modular and extensible design, including how to add new report sections, see [Module Management](https://spatialecology.github.io/camtrapReport/articles/modules.html).

## Contribute

Questions, suggestions, and ideas for improvement are always welcome. You can contribute to the development of the [camtrapReport R package](https://github.com/spatialecology/camtrapReport) by [opening an issue](https://github.com/spatialecology/camtrapReport/issues) or joining the conversation in [GitHub Discussions](https://github.com/spatialecology/camtrapReport/discussions).

## Citation

As the official scientific publication for `camtrapReport` is still in preparation, please cite the package as:

[Ebrahimi, E., & Jansen, P. A. (2026). <em>CamtrapReport: An R Package for Automating Camera-Trap Data Reporting for Wildlife Monitoring</em>. p. 65. Abstract from <em>The International Biogeography Society – 12th Biennial Conference</em>, Aarhus, Denmark. <https://zenodo.org/records/18405441>]{.citation-text}

For package citation details, please refer to the citation information available in the [GitHub repository](https://github.com/spatialecology/camtrapReport).

## Acknowledgment

The development of `camtrapReport` was supported by [Biodiversa+](https://www.biodiversa.eu/2022/10/07/2022-2023-joint-call/) through the [Big Picture project](https://wildlifecamera.eu/).
