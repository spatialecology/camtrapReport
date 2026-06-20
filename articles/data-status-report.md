# Data Status Report

## Data Status Report

The Data Status Report provides an overview of the quality,
completeness, and readiness of a camera-trap dataset before generating
the final Ecological Report. It helps users check whether the main input
files, metadata fields, deployment information, observation records,
taxonomy, timestamps, annotation information, and spatial data are
complete and internally consistent.

This report is especially useful as a first quality-control step. It
allows users to identify missing values, duplicated records, invalid
timestamps, incomplete metadata, spatial issues, and inconsistencies
between deployments and observations. Based on these checks, users can
decide whether the dataset is ready for reporting or whether further
correction is needed before ecological analyses are performed.

![Figure 1. Workflow for generating a Data Status Report from
camera-trap data using camtrapReport.](figures/data-status/WF_DS.png)

Figure 1. Workflow for generating a Data Status Report from camera-trap
data using camtrapReport.

Figure 1 summarises how `camtrapReport` checks the readiness of a
camera-trap dataset before ecological reporting. After the data are
loaded with
[`camData()`](https://spatialecology.github.io/camtrapReport/reference/camData.md),
the
[`status()`](https://spatialecology.github.io/camtrapReport/reference/report.html)
function runs automated checks on the main input files, metadata,
deployments, observations, timestamps, taxonomy, spatial information,
and annotation records. The output helps users identify potential issues
and decide whether the dataset is ready for generating the final
Ecological Report.

``` r

library(camtrapReport)

# Load camera-trap data
cm <- camData("cameratrap.zip")

# Generate and view the Data Status Report
status(cm, view = TRUE)
```

When `view = TRUE`, the generated report is opened in the browser. This
provides a quick, human-readable overview of dataset quality,
completeness, and consistency.

------------------------------------------------------------------------

## Example Data Status Reports

Below are example Data Status Reports generated with `camtrapReport` for
different camera-trap monitoring projects. These examples show how the
report can be used to assess dataset completeness, detect possible
issues, and evaluate whether the data are ready for ecological
reporting. Click on an image to open the full HTML report.

[![Leuven Data Status
Report](figures/data-status/ct.png)](https://spatialecology.github.io/camtrapReport/reports/data_status_Leuven.md)

Leuven

Data Status Report

Open report

[![Amsterdam Data Status
Report](figures/data-status/dq.png)](https://spatialecology.github.io/camtrapReport/reports/data_status_Amsterdam.md)

Amsterdam Water Supply Dunes

Data Status Report

Open report

[![Luxembourg Data Status
Report](figures/data-status/ct1.png)](https://spatialecology.github.io/camtrapReport/reports/data_status_Lux.md)

Lux National Monitoring

Data Status Report

Open report

[![MICA Data Status
Report](figures/data-status/dq1.png)](https://spatialecology.github.io/camtrapReport/reports/data_status_MICA.md)

MICA Monitoring Project

Data Status Report

Open report

------------------------------------------------------------------------

## Exploring data-status results in R

The results of the data-status checks are also stored inside the
`camReport` object. Users can explore these results directly in R
through the `data_status` slot. This is useful when users want to
inspect the underlying tables, extract summaries, or use the
quality-control outputs in another workflow.

``` r

# Explore all data-status outputs stored in the camReport object
cm$data_status

# Show the available data-status components
names(cm$data_status)

cm$data_status$Spatial
cm$data_status$Temporal
cm$data_status$Essentials
cm$data_status$Annotation
cm$data_status$Validation
cm$data_status$Species
cm$data_status$Visuals
```

For example, the species table can be inspected to check which species
were detected and how many captures were recorded for each species.

``` r

# View species summary table
cm$data_status$Species$Table
```

Together, `status(cm, view = TRUE)` and `cm$data_status` provide both a
human-readable HTML report and direct access to the underlying
data-quality summaries in R. This makes the Data Status Report useful
for documenting data readiness, identifying issues, and improving the
dataset before generating the final Ecological Report.
