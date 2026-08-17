# Leuven example data for camtrapReport

## Purpose

The bundled Leuven data are a compact derived subset of the
**GMU8_LEUVEN** Camtrap DP dataset, prepared for `camtrapReport` examples,
automated tests, and software demonstrations.

The subset is intended to exercise package functionality across multiple
years, repeated camera deployments, multiple species, and REM-related fields.
It should not be treated as a dataset prepared for ecological inference.

## Source dataset

The subset was derived from:

Casaer J, Boone N, Vercammen J, Devisscher S, Pallemaerts L, Rutten A,
Bollen M, Desmet P, Govaert S (2025). *GMU8_LEUVEN - Camera trap
observations in natural habitats south of Leuven (Belgium).* Version 1.
Camtrap DP dataset.

The source metadata specifies:

- data licence: CC0-1.0
- media licence: CC-BY-4.0

These licence declarations are retained in the derived Camtrap DP metadata.

## Subset composition

The bundled subset contains:

- 8 derived example location identifiers
- 87 deployments
- deployment-start years from 2018 through 2023
- 11,092 raw Camtrap DP observation records
- 10,182 unique events
- 15,492 retained media records

All observation records associated with the selected deployments were
retained. Observations were not selected on the basis of species or
observation type.

## Location identifiers

The source dataset uses deployment-specific `locationID` values, while
`locationName` values contain a trailing site code, for example
`2020_7_1473`.

For this derived software fixture, deployments sharing the same trailing
site code were assigned a common location identifier so that repeated
deployments can be handled consistently in package examples and tests.

The identifiers used in the fixture are:

- LEUVEN_416
- LEUVEN_881
- LEUVEN_930
- LEUVEN_1210
- LEUVEN_1304
- LEUVEN_1434
- LEUVEN_1473
- LEUVEN_1713

These identifiers are specific to the derived `camtrapReport` fixture and
should not be interpreted as official location identifiers from the source
dataset.

The original `deploymentID` values are retained unchanged.

Coordinates associated with the same derived location showed small
differences among deployments. For reproducibility within the fixture,
latitude and longitude were standardized to the median coordinate for each
derived location and rounded to 0.001 degrees.

| locationID | latitude | longitude |
|---|---:|---:|
| LEUVEN_416 | 50.769 | 4.638 |
| LEUVEN_881 | 50.803 | 4.635 |
| LEUVEN_930 | 50.805 | 4.634 |
| LEUVEN_1210 | 50.816 | 4.674 |
| LEUVEN_1304 | 50.821 | 4.649 |
| LEUVEN_1434 | 50.828 | 4.638 |
| LEUVEN_1473 | 50.829 | 4.636 |
| LEUVEN_1713 | 50.845 | 4.649 |

## Media reduction

The source dataset contains substantially more media records than are needed
for package testing.

To keep the installed package reasonably small, the derived fixture retains:

- the first media record associated with each selected event;
- the last media record associated with each selected event; and
- media records explicitly referenced by retained observations.

Intermediate media records were omitted.

Consequently, the number of media records associated with an event in this
fixture should not be interpreted as the original number of photographs in
the source dataset.

## Habitat example

A small location-level habitat table is provided in:

`inst/external/habitat/habitat.csv`

The habitat information was derived from habitat information associated with
the selected Leuven deployments.

Because habitat labels can vary among repeated deployments associated with
the same derived location, the bundled habitat table is intended primarily
as an example input for demonstrating habitat-related functionality in
`camtrapReport`. It should not be interpreted as a definitive ecological
classification of each location.

No habitat classes were created solely for the purpose of making package
examples work.

## Study-area example

A study-area shapefile is provided in:

`inst/external/study_area/`

The polygon is a **derived example boundary**, not an official boundary of
the GMU8_LEUVEN study area.

It was constructed as the convex hull of the eight derived example
locations. The calculation was performed in a projected coordinate reference
system and the resulting shapefile is supplied in WGS 84 (EPSG:4326).

Its purpose is to provide a reproducible spatial input for testing and
demonstrating the `study_area` argument of `camData()`.

## REM-related fields

The subset retains REM-related information available for selected
observations and deployments, including fields used by `camtrapReport` for
detection distance, detection angle, movement speed, activity, and
REM-density workflows.

The presence of these fields in the software fixture is intended to support
testing of REM functionality. Density estimates produced from this reduced
fixture should not be interpreted as estimates from the complete original
study dataset.

## Privacy and metadata minimisation

Contact email addresses from the source metadata were omitted from this
derived software fixture because they are not required for package examples
or automated tests.

Contributor names, dataset provenance, licence information, and the source
dataset citation are retained.

## Validation

The derived fixture was checked to ensure that:

- there are exactly 8 derived location identifiers;
- all `deploymentID` values are unique;
- all `observationID` values are unique;
- all `mediaID` values are unique;
- every retained observation refers to a retained deployment;
- every retained media record refers to a retained deployment;
- explicit observation-to-media references resolve to retained media records;
- the dataset can be read successfully by `camData()`;
- the bundled habitat input can be linked to the example locations;
- the bundled study-area shapefile can be read and stored by `camData()`; and
- the package test suite and `R CMD check` complete successfully with the
  bundled fixture.

## Important note

This is a **software testing and demonstration fixture derived from the
source dataset**. Transformations made to reduce size or provide stable
example inputs are documented above. For scientific analyses, users should
refer to the complete source dataset and its accompanying metadata.
