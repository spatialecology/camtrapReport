# Run with Rscript /path/to/reproduce.R. No GUI or original data folder is needed.
# This replays a trusted camReport snapshot, not an independent raw-data import.
local({
  sources <- vapply(sys.frames(), function(frame) {
    if (is.character(frame$ofile)) frame$ofile[1L] else ""
  }, "")
  sources <- sources[nzchar(sources)]
  cli <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  script <- if (length(sources)) tail(sources, 1L) else if (length(cli)) {
    sub("^--file=", "", cli[1L])
  } else stop("Run this file with Rscript or source('/path/to/reproduce.R').")
  bundle <- dirname(normalizePath(script, winslash = "/", mustWork = TRUE))
  manifest_file <- file.path(bundle, "manifest.rds")
  if (!file.exists(manifest_file)) stop("Missing bundle file: manifest.rds")
  manifest <- readRDS(manifest_file)
  if (!identical(manifest$schema, 1L)) stop("Unsupported reproduction bundle version.")
  files <- file.path(bundle, names(manifest$files))
  absent <- names(manifest$files)[!file.exists(files) | dir.exists(files)]
  if (length(absent)) stop("Missing required bundle file(s): ", paste(absent, collapse = ", "))
  if (!identical(unname(tools::md5sum(files)), unname(manifest$files))) {
    stop("A saved object or required asset has changed. Restore the exported bundle.")
  }
  missing <- names(manifest$packages)[!vapply(names(manifest$packages),
    requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install required package(s): ", paste(missing, collapse = ", "))
  if (as.character(utils::packageVersion("camtrapReport")) != manifest$packages[["camtrapReport"]]) {
    stop("This bundle requires camtrapReport ", manifest$packages[["camtrapReport"]],
         "; install that version before reproducing the reports.")
  }
  versions <- vapply(names(manifest$packages), function(p) as.character(utils::packageVersion(p)), "")
  changed <- names(versions)[versions != manifest$packages]
  if (length(changed) || as.character(getRversion()) != manifest$R) {
    warning("R or dependency versions differ from sessionInfo.txt; results may differ. Packages: ",
            paste(changed, collapse = ", "), call. = FALSE)
  }
  if (!rmarkdown::pandoc_available()) stop("Pandoc is required; install Pandoc or RStudio.")
  # The bundle intentionally contains a processed snapshot, not the source CSVs.
  # Suppress only this expected freshness notice; retain analytical warnings.
  cm <- withCallingHandlers(camtrapReport::camData(bundle,
      cache_file = file.path(bundle, "camReport_object.rds")), warning = function(w) {
    if (startsWith(conditionMessage(w), "Freshness of this camReport cache cannot be verified")) {
      invokeRestart("muffleWarning")
    }
  })
  cm$info$directory <- file.path(bundle, "assets")
  if (nzchar(cm$logoPath)) cm$logoPath <- file.path(bundle, cm$logoPath)
  output <- file.path(bundle, "reports")
  if (file.exists(output)) {
    i <- 1L
    repeat {
      candidate <- file.path(output, sprintf("run-%03d", i))
      if (!file.exists(candidate)) { output <- candidate; break }
      i <- i + 1L
    }
  }
  if (!dir.create(output, recursive = TRUE)) stop("Cannot create report directory: ", output)
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  message("Regenerating reports from stored data/state. This is not a raw-data reanalysis.")
  message("Online basemap imagery and external resources in custom modules are not archived.")
  for (type in manifest$reports) {
    do.call(RNGkind, as.list(manifest$rng[[type]]$kind))
    assign(".Random.seed", manifest$rng[[type]]$seed, envir = .GlobalEnv)
    if (identical(type, "report")) {
      result <- camtrapReport::report(cm, filename = file.path(output, "ecological_report.html"), view = FALSE)
      if (inherits(result, "try-error")) stop(as.character(result), call. = FALSE)
    } else {
      camtrapReport::status(cm, filename = file.path(output, "data_status.html"), view = FALSE)
    }
  }
  message("Reports saved to: ", output)
})
