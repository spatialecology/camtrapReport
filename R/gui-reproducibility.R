# GUI-only output organization and portable snapshot export.
.gui_output_directory <- function(parent, area) {
  .check_output_directory(parent)
  out <- file.path(parent, "GUI", area)
  if (!dir.exists(out) && !dir.create(out, recursive = TRUE)) {
    stop("Could not create GUI output directory: ", out, call. = FALSE)
  }
  .check_output_directory(out)
  normalizePath(out, winslash = "/", mustWork = TRUE)
}

.gui_rng_state <- function() {
  # Initialize an otherwise absent RNG state, without imposing a fixed seed.
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    set.seed(NULL)
  }
  list(kind = RNGkind(), seed = get(".Random.seed", envir = .GlobalEnv))
}

.gui_export_assets <- function(object) {
  assets <- character()
  logo <- object$logoPath
  if (length(logo) && !is.na(logo) && nzchar(logo)) {
    if (!file.exists(logo) || dir.exists(logo)) {
      stop("The report logo is unavailable: ", logo, call. = FALSE)
    }
    assets[paste0("logo.", tools::file_ext(logo))] <- logo
  }
  attached <- .reportSection_catalog(object$reportObjects)$name
  if ("appendix" %in% attached) {
    media <- object$data$media
    media <- media[which(media$favourite %in% TRUE), , drop = FALSE]
    if (nrow(media) && !is.null(object$setting$focus_groups)) {
      species <- object$get_speciesNames(object$setting$focus_groups)
      taxonomy <- object$data$taxonomy
      observations <- object$data$observations
      taxa <- taxonomy$taxonID[taxonomy$scientificName %in% species]
      sequences <- observations$sequenceID[observations$taxonID %in% taxa]
      media <- media[media$sequenceID %in% sequences, , drop = FALSE]
    }
    if (nrow(media)) {
      names <- as.character(media$fileName)
      if (length(names) != nrow(media) || anyNA(names) || !all(nzchar(names)) ||
          any(grepl("[/\\\\:]", names)) || any(names %in% c(".", ".."))) {
        stop("Favourite-image filenames must be simple local filenames for export.",
             call. = FALSE)
      }
      # Archive all candidates: the existing module samples among favourites.
      for (name in unique(names)) {
        cached <- file.path(object$info$directory, "_camtrap_pictures", name)
        if (!file.exists(cached) || dir.exists(cached)) {
          stop("Required favourite-image cache is unavailable: ", name,
               ". Cache the image in _camtrap_pictures or deselect appendix. ",
               "Export does not download remote images.", call. = FALSE)
        }
        assets[paste0("_camtrap_pictures/", name)] <- cached
      }
    }
  }
  assets
}

.export_gui_workflow <- function(object, parent, rng = list()) {
  stopifnot(inherits(object, "camReport"))
  .check_output_directory(parent)
  types <- c(if (length(object$reportObjects)) "report",
             if (length(object$statusReportObjects)) "status")
  if (!length(types)) stop("Select at least one report module before export.")
  packages <- unique(c("camtrapReport", "rmarkdown", "knitr",
    .collect_module_packages(object$reportObjects),
    .collect_module_packages(object$statusReportObjects)))
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Install required package(s) before export: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  assets <- .gui_export_assets(object)
  templates <- system.file("reproducibility", package = "camtrapReport")
  template_files <- c("reproduce.R", "reproduce.Rmd", "README.md")
  if (!all(file.exists(file.path(templates, template_files)))) {
    stop("Reproducibility templates are missing; reinstall camtrapReport.")
  }
  # Never replace an existing bundle, even when ordinary report overwrite is on.
  base <- file.path(.gui_output_directory(parent, ""), "reproducibility")
  destination <- base
  if (file.exists(base)) {
    if (!dir.exists(base)) stop("The reproducibility destination is a file: ", base)
    i <- 1L
    repeat {
      destination <- file.path(base, sprintf("export-%03d", i))
      if (!file.exists(destination)) break
      i <- i + 1L
    }
  }
  # Build beside the destination and publish only the completed bundle.
  stage <- tempfile(".workflow-", tmpdir = dirname(base))
  if (!dir.create(stage)) stop("Could not create the export staging directory.")
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  saved <- object$copy(shallow = FALSE)
  saved$info$directory <- "assets"
  saved$info$source_file_metadata <- NULL
  if (!is.null(saved$study_area)) saved$study_area$path <- NULL
  if (length(assets)) {
    for (name in names(assets)) {
      target <- file.path(stage, "assets", name)
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(assets[[name]], target)) stop("Could not copy asset: ", name)
    }
    if (nzchar(saved$logoPath)) {
      saved$logoPath <- paste0("assets/", names(assets)[1L])
    }
  }
  .save_camreport_cache(saved, file.path(stage, "camReport_object.rds"))
  for (type in types) {
    if (is.null(rng[[type]])) rng[[type]] <- .gui_rng_state()
  }
  versions <- vapply(packages, function(p) as.character(utils::packageVersion(p)), "")
  required <- c("camReport_object.rds", paste0("assets/", names(assets)))
  required <- required[nzchar(required) & required != "assets/"]
  hashes <- tools::md5sum(file.path(stage, required))
  names(hashes) <- required
  manifest <- list(schema = 1L, reports = types, packages = versions,
                   R = as.character(getRversion()), rng = rng[types],
                   files = hashes, created = format(Sys.time(), tz = "UTC"))
  saveRDS(manifest, file.path(stage, "manifest.rds"))
  if (!all(file.copy(file.path(templates, template_files), stage))) {
    stop("Could not copy reproduction scripts.")
  }
  writeLines(utils::capture.output(utils::sessionInfo()), file.path(stage, "sessionInfo.txt"))
  if (file.exists(destination) || !file.rename(stage, destination)) {
    stop("Could not finish export without replacing existing files: ", destination)
  }
  normalizePath(destination, winslash = "/", mustWork = TRUE)
}
