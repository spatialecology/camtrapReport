# Reproduce this camtrapReport snapshot

Keep this entire directory together. It may be moved to another computer.
Install camtrapReport at the version recorded in manifest.rds/sessionInfo.txt,
its required packages, and Pandoc (also supplied by RStudio or Quarto).
Nothing is installed or uploaded automatically.

Run `Rscript reproduce.R` from this directory, or pass the script's full path
from any directory. Alternatively run `rmarkdown::render("reproduce.Rmd")`.
The R Markdown file launches the same R script in a fresh process. It does not
contain a second analytical implementation or recursively render itself.

The first run creates reports/ with ecological_report.html/.Rmd and/or
data_status.html/.Rmd, according to the attached modules in the saved object.
Later runs create reports/run-001/, run-002/, etc. Existing reports are retained.
The summary reproduce.html is replaced when you explicitly render reproduce.Rmd
again, as with an ordinary R Markdown document.

## What is reproduced

- Report regeneration uses the saved configured object through public camData()
  cache restoration, then public report() and status(). This reconstructs the
  package's rendering environment; the analytical Rmd files alone do not do so.
- Included modules, their edited narrative, profiles, filters, metadata, spatial
  objects, habitat data, REM parameters/model caches and settings are retained.
  Excluded modules stay excluded. Unsaved edits to excluded GUI sections are not
  part of the configured report. The installed module registry is not edited.
- The existing report functions decide whether stored caches can be reused and
  which calculations must run. Cache reuse is not independent model refitting.
  The per-report RNG state is restored, including the state recorded before the
  latest corresponding GUI generation, when available. Existing REM seeds and
  model settings are preserved. Exact agreement requires the same configuration,
  package/runtime versions and available external resources.
- Original source CSVs are not re-imported. Full raw-input reproduction would
  require the import options and construction history, which arbitrary objects
  passed into the GUI may not retain. This is a potential future enhancement.

## Assets and limitations

Local report logos and cached favourite images required by the appendix are
copied to assets/; spatial boundaries are packed into the saved object. Missing
required local assets prevent export or execution. Export does not download
remote images: supply _camtrap_pictures caches or deselect the appendix.
Only necessary assets are copied; the original source directory is not archived.

Online map backgrounds retain the existing provider settings; their tile imagery
requires internet access and is not archived. Analytical map layers remain in the
report. Custom modules may contain arbitrary code, file paths, online services or
undeclared dependencies: their external requirements cannot be bundled
automatically. Use self-contained trusted modules for portable reproduction, or
supply/document their resources explicitly. Do not claim offline reproducibility
of online basemaps or custom external services. PDF/browser dependencies and
cross-version numerical identity are not guaranteed by this HTML workflow.

The package version, required module packages, RNG states and asset checksums are
recorded in manifest.rds. Missing dependencies/assets produce errors; differing R
or dependency versions produce a warning. sessionInfo.txt records the export
environment, not a complete software lockfile. Reports may differ in timestamps,
widget IDs and other presentation details without changing analytical values.

**Privacy:** saved objects and reports may contain sensitive species locations,
observations and project metadata. Review the entire bundle before sharing.
Only execute bundles and custom modules from trusted sources.
