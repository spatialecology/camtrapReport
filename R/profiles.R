# Functions for defining and sharing report profiles
# Licence: MIT
#--------

.empty_profile_entries <- function() {
  data.frame(
    source = character(),
    module = character(),
    parent = character(),
    stringsAsFactors = FALSE
  )
}

.split_module_reference <- function(x, default_source = "report") {
  x <- trimws(as.character(x)[1])
  if (is.na(x) || !nzchar(x)) {
    stop("Module references must be non-empty character strings.")
  }

  parts <- strsplit(x, "::", fixed = TRUE)[[1]]
  if (length(parts) == 1L) {
    source <- default_source
    module <- parts
  } else if (length(parts) == 2L) {
    source <- tolower(trimws(parts[1]))
    module <- trimws(parts[2])
  } else {
    stop("Invalid module reference: ", x)
  }

  if (!source %in% c("report", "status")) {
    stop("Unknown module source in '", x, "'.")
  }
  if (!nzchar(module)) {
    stop("Missing module name in '", x, "'.")
  }

  c(source = source, module = module)
}

.normalize_profile_parent <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) {
    return(NA_character_)
  }

  x <- trimws(as.character(x)[1])
  if (!nzchar(x) || tolower(x) %in% c("null", "na")) {
    return(NA_character_)
  }
  if (grepl("::", x, fixed = TRUE)) {
    x <- .split_module_reference(x)[["module"]]
  }
  x
}

.profile_entry_row <- function(x, default_source) {
  if (is.character(x) && length(x) == 1L) {
    ref <- .split_module_reference(x, default_source)
    return(data.frame(
      source = unname(ref[["source"]]),
      module = unname(ref[["module"]]),
      parent = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  if (!is.list(x)) {
    stop("Each profile entry must be a module reference or a named list.")
  }

  module <- x$module
  if (is.null(module)) {
    module <- x$name
  }
  if (is.null(module) || length(module) == 0L) {
    stop("Each profile entry must define 'module'.")
  }

  source <- x$source
  ref <- .split_module_reference(
    module,
    if (is.null(source)) default_source else source
  )

  data.frame(
    source = unname(ref[["source"]]),
    module = unname(ref[["module"]]),
    parent = .normalize_profile_parent(x$parent),
    stringsAsFactors = FALSE
  )
}

.normalize_profile_entries <- function(x, default_source = "report") {
  default_source <- match.arg(default_source, c("report", "status"))

  if (is.null(x) || length(x) == 0L) {
    return(.empty_profile_entries())
  }

  if (is.data.frame(x)) {
    if (!"module" %in% names(x)) {
      stop("Profile entry data frames must contain a 'module' column.")
    }
    if (!"source" %in% names(x)) {
      x$source <- default_source
    }
    if (!"parent" %in% names(x)) {
      x$parent <- NA_character_
    }

    rows <- lapply(seq_len(nrow(x)), function(i) {
      .profile_entry_row(
        list(
          source = x$source[i],
          module = x$module[i],
          parent = x$parent[i]
        ),
        default_source
      )
    })
  } else if (is.character(x)) {
    rows <- lapply(x, .profile_entry_row, default_source = default_source)
  } else if (is.list(x)) {
    rows <- lapply(x, .profile_entry_row, default_source = default_source)
  } else {
    stop("Profile entries must be character, a list, or a data frame.")
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[, c("source", "module", "parent"), drop = FALSE]
}

#' Define, import, and export report profiles
#'
#' A report profile is an ordered selection of modules used to produce an
#' ecological report and a data-status report. Modules normally come from the
#' corresponding module pool. A module from the other pool can be selected
#' with a qualified reference such as `"status::spatial"`.
#'
#' Profile entries may be character vectors or data frames. Character entries
#' use `"source::module"` notation; unqualified entries default to the report
#' pool for `report` and the status pool for `status`. A data frame can also
#' contain a `parent` column to override the parent declared by a module. This
#' makes it possible, for example, to place status modules below an ecological
#' report's appendix without modifying the shared module YAML files.
#'
#' Bundled profiles are registered in each object created by [camData()]. User
#' profiles can be registered with `add_profile()` and shared as YAML files
#' with `read_profile()` and `write_profile()`.
#'
#' @param name A non-empty profile name.
#' @param report An ordered set of modules for [report()].
#' @param status An ordered set of modules for [status()].
#' @param description Optional descriptive text.
#' @param file Path to a profile YAML file.
#' @param object A [`camReport`][camReport-classes] object.
#' @param profile A `reportProfile` object, a profile name registered in
#'   `object`, or a bundled profile name. `write_profile()` accepts either an
#'   object or a name.
#' @param overwrite A logical value controlling whether an existing profile or
#'   file can be replaced.
#'
#' @return `reportProfile()` and `read_profile()` return a `reportProfile`
#'   object. `profile_names()` returns a character vector. `add_profile()`
#'   invisibly returns the modified `camReport` object, and `write_profile()`
#'   invisibly returns the output path.
#'
#' @seealso [sections()], [section_names()], [report()], [status()]
#' @family report profiles
#'
#' @examples
#' profile <- reportProfile(
#'   name = "brief",
#'   report = c("introduction", "results"),
#'   status = c("abstract", "availability")
#' )
#' profile
#'
#' profile_file <- tempfile(fileext = ".yml")
#' write_profile(profile, profile_file)
#' identical(read_profile(profile_file), profile)
#' unlink(profile_file)
#'
#' @export
reportProfile <- function(
  name,
  report = character(),
  status = character(),
  description = ""
) {
  methods::new(
    "reportProfile",
    name = trimws(as.character(name)[1]),
    description = as.character(description)[1],
    report = .normalize_profile_entries(report, "report"),
    status = .normalize_profile_entries(status, "status")
  )
}

.profile_dir <- function(package = "camtrapReport", dir = NULL) {
  if (!is.null(dir)) {
    return(normalizePath(dir, winslash = "/", mustWork = TRUE))
  }

  out <- system.file("reportProfiles", package = package)
  if (!nzchar(out)) {
    return("")
  }
  out
}

#' @rdname reportProfile
#' @export
read_profile <- function(file) {
  file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  value <- rmarkdown::yaml_front_matter(file)

  if (is.null(value$name)) {
    stop("Profile YAML must define 'name'.")
  }

  reportProfile(
    name = value$name,
    description = value$description %||% "",
    report = value$report %||% character(),
    status = value$status %||% character()
  )
}

.read_profiles <- function(dir = NULL, package = "camtrapReport") {
  dir <- .profile_dir(package = package, dir = dir)
  if (!nzchar(dir) || !dir.exists(dir)) {
    return(list())
  }

  files <- list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)
  out <- lapply(files, read_profile)
  names(out) <- vapply(out, function(x) x@name, character(1))
  out
}

.yaml_string <- function(x) {
  as.character(jsonlite::toJSON(
    as.character(x)[1],
    auto_unbox = TRUE,
    na = "null"
  ))
}

.profile_yaml_entries <- function(entries, indent = "  ") {
  if (nrow(entries) == 0L) {
    return(" []")
  }

  lines <- character()
  for (i in seq_len(nrow(entries))) {
    lines <- c(
      lines,
      paste0(indent, "- source: ", .yaml_string(entries$source[i])),
      paste0(indent, "  module: ", .yaml_string(entries$module[i]))
    )
    if (!is.na(entries$parent[i]) && nzchar(entries$parent[i])) {
      lines <- c(
        lines,
        paste0(indent, "  parent: ", .yaml_string(entries$parent[i]))
      )
    }
  }
  paste0("\n", paste(lines, collapse = "\n"))
}

#' @rdname reportProfile
#' @export
write_profile <- function(profile, file, overwrite = FALSE, object = NULL) {
  if (!inherits(profile, "reportProfile")) {
    profile <- .resolve_profile(profile, object)
  }
  if (!inherits(profile, "reportProfile")) {
    stop("'profile' must be a reportProfile object.")
  }
  file <- path.expand(as.character(file)[1])
  if (file.exists(file) && !isTRUE(overwrite)) {
    stop("Profile file already exists; use overwrite = TRUE to replace it.")
  }
  parent_dir <- dirname(file)
  if (!dir.exists(parent_dir)) {
    stop("The profile output directory does not exist: ", parent_dir)
  }

  lines <- c(
    "---",
    paste0("name: ", .yaml_string(profile@name)),
    paste0("description: ", .yaml_string(profile@description)),
    paste0("report:", .profile_yaml_entries(profile@report)),
    paste0("status:", .profile_yaml_entries(profile@status)),
    "---"
  )
  writeLines(lines, con = file, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

.find_profile_name <- function(name, available) {
  hit <- which(tolower(available) == tolower(as.character(name)[1]))
  if (length(hit) != 1L) {
    stop(
      "Unknown report profile '", name, "'. Available profiles: ",
      toString(available)
    )
  }
  available[hit]
}

.resolve_profile <- function(profile, object = NULL) {
  if (inherits(profile, "reportProfile")) {
    return(profile)
  }
  if (!is.character(profile) || length(profile) != 1L ||
      is.na(profile) || !nzchar(profile)) {
    stop("'profile' must be a profile name or reportProfile object.")
  }

  available <- if (!is.null(object) &&
      inherits(object, "camReport") &&
      !is.null(object$reportObjectElements$Profiles)) {
    object$reportObjectElements$Profiles
  } else {
    .read_profiles()
  }

  if (length(available) == 0L) {
    stop("No report profiles are available.")
  }
  available[[.find_profile_name(profile, names(available))]]
}

#' @rdname reportProfile
#' @export
profile_names <- function(object = NULL) {
  if (!is.null(object) && !inherits(object, "camReport")) {
    stop("'object' must be a camReport object or NULL.")
  }
  profiles <- if (is.null(object)) {
    .read_profiles()
  } else {
    object$reportObjectElements$Profiles %||% list()
  }
  names(profiles)
}

#' @rdname reportProfile
#' @export
add_profile <- function(object, profile, overwrite = FALSE) {
  if (!inherits(object, "camReport")) {
    stop("'object' must be a camReport object.")
  }
  if (is.character(profile) && length(profile) == 1L && file.exists(profile)) {
    profile <- read_profile(profile)
  }
  if (!inherits(profile, "reportProfile")) {
    stop("'profile' must be a reportProfile object or a profile YAML path.")
  }

  profiles <- object$reportObjectElements$Profiles %||% list()
  current <- names(profiles)
  same <- which(tolower(current) == tolower(profile@name))
  if (length(same) > 0L && !isTRUE(overwrite)) {
    stop(
      "A profile named '", profile@name,
      "' is already registered; use overwrite = TRUE to replace it."
    )
  }
  if (length(same) > 0L) {
    profiles[[same[1]]] <- NULL
  }
  profiles[[profile@name]] <- profile
  object$reportObjectElements$Profiles <- profiles
  invisible(object)
}

.profile_reference <- function(entries) {
  paste(entries$source, entries$module, sep = "::")
}

.module_pool <- function(object, source) {
  if (identical(source, "report")) {
    object$reportObjectElements$Modules
  } else {
    object$reportObjectElements$Status_modules
  }
}

.module_pool_info <- function(object, source) {
  if (identical(source, "report")) {
    object$reportObjectElements$Modules_info
  } else {
    object$reportObjectElements$Status_modules_info
  }
}

.available_pool_names <- function(object, source, include_failed = FALSE) {
  info <- .module_pool_info(object, source)
  if (isTRUE(include_failed) || !"tested" %in% names(info)) {
    return(info$name)
  }
  info$name[is.na(info$tested) | info$tested]
}

.copy_profile_module <- function(module, parent = NA_character_) {
  out <- unserialize(serialize(module, NULL))
  if (!is.na(parent)) {
    out@parent <- if (identical(parent, ".root")) NULL else parent
  }
  out
}

.attach_profile_entries <- function(object, entries, report_type = "report") {
  report_type <- match.arg(report_type, c("report", "status"))
  entries <- .normalize_profile_entries(entries, report_type)

  output_names <- entries$module
  if (anyDuplicated(output_names)) {
    stop(
      "A rendered profile cannot contain duplicate module names across pools: ",
      toString(unique(output_names[duplicated(output_names)]))
    )
  }

  modules <- vector("list", nrow(entries))
  effective_parents <- rep(NA_character_, nrow(entries))
  for (i in seq_len(nrow(entries))) {
    source <- entries$source[i]
    pool <- .module_pool(object, source)
    module <- pool[[entries$module[i]]]
    if (is.null(module)) {
      stop(
        "Profile module not found: ", source, "::", entries$module[i]
      )
    }
    available <- .available_pool_names(object, source)
    if (!entries$module[i] %in% available) {
      stop(
        "Profile module failed its module test and cannot be attached: ",
        source, "::", entries$module[i]
      )
    }
    modules[[i]] <- .copy_profile_module(module, entries$parent[i])
    effective_parents[i] <- .norm_parent(modules[[i]]@parent)
  }

  for (i in seq_len(nrow(entries))) {
    parent <- effective_parents[i]
    earlier <- if (i == 1L) character() else output_names[seq_len(i - 1L)]
    if (!identical(parent, ".root") && !parent %in% earlier) {
      stop(
        "Could not attach profile module '",
        entries$source[i], "::", entries$module[i],
        "': parent '", parent,
        "' must appear earlier in the profile.",
        call. = FALSE
      )
    }
  }

  if (identical(report_type, "report")) {
    object$reportObjects <- list()
    add <- function(x) object$addReportObject(x)
  } else {
    object$statusReportObjects <- list()
    add <- function(x) object$addStatusReportObject(x)
  }

  for (i in seq_along(modules)) {
    tryCatch(
      add(modules[[i]]),
      error = function(e) {
        stop(
          "Could not attach profile module '",
          entries$source[i], "::", entries$module[i],
          "'. Check that its parent appears earlier in the profile. ",
          "Original error: ", conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }
  invisible(object)
}

.apply_profile <- function(object, profile, report_type = "report") {
  profile <- .resolve_profile(profile, object)
  report_type <- match.arg(report_type, c("report", "status"))
  entries <- methods::slot(profile, report_type)
  if (nrow(entries) == 0L) {
    stop("Profile '", profile@name, "' has no modules for this report type.")
  }
  .attach_profile_entries(object, entries, report_type)
}

setMethod("show", "reportProfile", function(object) {
  cat("Report profile: ", object@name, "\n", sep = "")
  if (nzchar(object@description)) {
    cat(object@description, "\n")
  }
  cat("  ecological report modules: ", nrow(object@report), "\n", sep = "")
  cat("  data-status modules: ", nrow(object@status), "\n", sep = "")
})
