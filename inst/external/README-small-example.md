# Small dataset for executable API examples

`example-small/` is a deterministic subset of the bundled Leuven dataset in
`dataset/`. It illustrates import, metadata access, section selection and text
editing without repeatedly initializing density models on a multiyear dataset.
It is not suitable for ecological inference or density estimation. The larger
dataset, full-report vignettes and analytical regression tests are unchanged.

The source and licences are described in `README-Leuven-dataset.md`: GMU8_LEUVEN,
Casaer et al. (2025), data CC0-1.0 and media CC-BY-4.0. Source attribution,
contributors, project information and licences remain in `datapackage.json`.
No photographs are bundled or downloaded to prepare this subset.

Construction is deterministic:

1. Order deployments by deployment start and deployment ID; retain the earliest
   deployment at each of the eight locations.
2. Within each retained deployment, order distinct events by event start and
   event ID; retain the first 32 events (or all if fewer are available).
3. Keep every observation belonging to those events. Keep every available media
   record within their start/end intervals and any explicitly referenced media
   ID. Preserve the original measurement values and deployment intervals.
4. Retain source taxonomy entries for the observed scientific names; update the
   package title, description and temporal extent to describe this subset.

The resulting data contain 8 deployments, 256 events, 267 observations and
349 media records, with 10 recorded scientific names. Selection is chronological,
not based on species or model-fitting results. These early events have no speed
measurements; no analytical threshold or estimator is changed to bypass fitting.
Source classifications remain incomplete, so the import's missing-class
diagnostic is expected. The examples retain the default offline taxonomy mode,
copy files into a fresh temporary directory and clean up their own outputs.
