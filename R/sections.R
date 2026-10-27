# Functions for selecting and updating camtrapReport report sections
# Licence: MIT
#--------

setGeneric(
  "section_names",
  function(keep, exclude, object, profile, report_type) {
    methods::standardGeneric("section_names")
  }
)

.installed_pool_info <- function(source) {
  source <- match.arg(source, c("report", "status"))
  if (identical(source, "report")) {
    dir <- .section_dir("camtrapReport")
    level0 <- c(
      "introduction", "methods", "results", "acknowledgements", "appendix"
    )
  } else {
    dir <- system.file("statusSections", package = "camtrapReport")
    level0 <- c(
      "abstract", "spatial", "temporal", "availability", "validation",
      "annotation", "observation_type", "conclusion", "acknowledge"
    )
  }

  modules <- .read_modules(
    level0 = level0,
    package = "camtrapReport",
    dir = dir,
    write_info = FALSE
  )
  attributes(modules)$info
}

.section_pool_names <- function(object = NULL, source = "report") {
  if (!is.null(object)) {
    return(.available_pool_names(object, source))
  }
  .installed_pool_info(source)$name
}

.canonical_section_matches <- function(x, available) {
  x <- as.character(x)
  out <- rep(NA_character_, length(x))

  for (i in seq_along(x)) {
    if (x[i] %in% available) {
      out[i] <- x[i]
      next
    }
    suffix <- sub("^[^:]+::", "", available)
    hit <- which(!grepl("::", x[i], fixed = TRUE) & suffix == x[i])
    if (length(hit) == 1L) {
      out[i] <- available[hit]
    }
  }
  out
}

.filter_section_names <- function(n, keep = NULL, exclude = NULL) {
  if (is.character(keep) && length(keep) > 0L) {
    matched <- .canonical_section_matches(keep, n)
    valid <- !is.na(matched)
    if (!any(valid)) {
      stop(
        "None of the specified section/module names in 'keep' are ",
        "available; use section_names() to get a list of existing modules."
      )
    }
    if (!all(valid)) {
      warning(
        "Several section/module names specified in 'keep' are not ",
        "available: ", .paste_comma_and(keep[!valid])
      )
    }
    return(matched[valid])
  }

  if (is.character(exclude) && length(exclude) > 0L) {
    matched <- .canonical_section_matches(exclude, n)
    valid <- !is.na(matched)
    if (!any(valid)) {
      stop(
        "None of the specified section/module names in 'exclude' are ",
        "available; use section_names() to get a list of existing modules."
      )
    }
    if (!all(valid)) {
      warning(
        "Several section/module names specified in 'exclude' are not ",
        "available: ", .paste_comma_and(exclude[!valid])
      )
    }
    return(n[!n %in% matched[valid]])
  }

  n
}

.profile_entry_parents <- function(entries, object = NULL) {
  parents <- entries$parent
  missing_parent <- is.na(parents) | !nzchar(parents)

  for (source in c("report", "status")) {
    rows <- which(missing_parent & entries$source == source)
    if (length(rows) == 0L) next

    info <- if (is.null(object)) {
      .installed_pool_info(source)
    } else {
      .module_pool_info(object, source)
    }
    parents[rows] <- info$parent[match(entries$module[rows], info$name)]
  }
  vapply(parents, .norm_parent, character(1), USE.NAMES = FALSE)
}

.prune_profile_selection <- function(entries, object = NULL) {
  if (nrow(entries) == 0L) return(entries)

  repeat {
    parents <- .profile_entry_parents(entries, object)
    valid <- logical(nrow(entries))
    for (i in seq_len(nrow(entries))) {
      earlier <- if (i == 1L) character() else entries$module[seq_len(i - 1L)]
      valid[i] <- identical(parents[i], ".root") || parents[i] %in% earlier
    }
    if (all(valid)) return(entries)
    entries <- entries[valid, , drop = FALSE]
    if (nrow(entries) == 0L) return(entries)
  }
}

#' Select report sections
#'
#' Get module names or update which sections are used by an ecological or
#' data-status report. Profiles provide named, reusable selections without
#' moving or duplicating the YAML module files.
#'
#' `section_names()` lists modules in a pool or in a profile. Set
#' `report_type = "all"` to list both pools. Names from both pools, and names
#' returned for a profile, use the unambiguous `"source::module"` notation.
#'
#' `sections()` updates the transient module tree used for the next render.
#' When `profile` is supplied, it can also inspect or update that registered
#' profile. Set `report_type = "status"` to manage the data-status selection.
#'
#' @param keep An optional character vector of section names to keep.
#' @param exclude An optional character vector of section names to exclude.
#' @param object An optional [`camReport`][camReport-classes] object. It is
#'   needed to inspect user-defined profiles registered in that object.
#' @param profile An optional profile name or `reportProfile` object.
#' @param report_type Which output selection to use: `"report"`, `"status"`,
#'   or, for `section_names()`, `"all"`.
#' @param x A [`camReport`][camReport-classes] object created by [camData()].
#' @param n An optional character vector of modules to include. Qualified names
#'   such as `"status::spatial"` can select from the other module pool when a
#'   profile is being edited.
#'
#' @return `section_names()` and `sections(x)` return character vectors.
#'   `sections(x, n)` updates `x` and returns it invisibly.
#'
#' @seealso [reportProfile()], [report()], [status()], [listReportSections()]
#' @family report sections
#'
#' @usage
#' section_names(keep, exclude, object, profile, report_type)
#'
#' sections(x, n, profile, report_type)
#' @name section_names
#' @aliases section_names sections section_names,ANY-method
#' @aliases sections,camReport-method
#'
#' @examples
#' section_names()
#' section_names(profile = "default")
#' section_names(report_type = "all")
#'
#' \donttest{
#' source_dataset <- system.file(
#'   "external", "dataset", package = "camtrapReport"
#' )
#' example_dataset <- tempfile("camtrapReport-example-")
#' dir.create(example_dataset)
#' invisible(file.copy(
#'   list.files(source_dataset, full.names = TRUE),
#'   example_dataset,
#'   recursive = TRUE
#' ))
#' cm <- camData(example_dataset)
#' sections(cm, profile = "default")
#' cm <- sections(cm, c("introduction", "methods"))
#' unlink(example_dataset, recursive = TRUE, force = TRUE)
#' }
setMethod(
  "section_names",
  signature(keep = "ANY"),
  function(keep, exclude, object, profile, report_type) {
    if (missing(keep)) keep <- NULL
    if (missing(exclude)) exclude <- NULL
    if (missing(object)) object <- NULL
    if (missing(profile)) profile <- NULL
    if (missing(report_type)) report_type <- "report"

    report_type <- match.arg(report_type, c("report", "status", "all"))
    if (!is.null(object) && !inherits(object, "camReport")) {
      stop("'object' must be a camReport object or NULL.")
    }

    if (!is.null(profile)) {
      value <- .resolve_profile(profile, object)
      if (identical(report_type, "all")) {
        entries <- rbind(value@report, value@status)
      } else {
        entries <- methods::slot(value, report_type)
      }
      entries <- entries[
        !duplicated(.profile_reference(entries)),
        ,
        drop = FALSE
      ]
      n <- .profile_reference(entries)
    } else if (identical(report_type, "all")) {
      n <- c(
        paste0("report::", .section_pool_names(object, "report")),
        paste0("status::", .section_pool_names(object, "status"))
      )
    } else {
      n <- .section_pool_names(object, report_type)
    }

    out <- .filter_section_names(n, keep, exclude)

    if (!is.null(profile)) {
      selected <- entries[
        match(out, .profile_reference(entries)),
        ,
        drop = FALSE
      ]
      selected <- .prune_profile_selection(selected, object)
      return(.profile_reference(selected))
    }

    # Preserve the historical safeguard for unqualified ecological selections.
    if (is.null(profile) && identical(report_type, "report")) {
      missing_parent <- .check_parent(out)
      if (!is.null(missing_parent)) {
        out <- out[!out %in% missing_parent]
      }
    }
    out
  }
)

#-------
setGeneric(
  "sections",
  function(x, n, profile, report_type) {
    methods::standardGeneric("sections")
  }
)

.profile_entries_from_selection <- function(n, current, report_type) {
  available_refs <- .profile_reference(current)
  rows <- vector("list", length(n))

  for (i in seq_along(n)) {
    matched <- .canonical_section_matches(n[i], available_refs)
    if (!is.na(matched[1])) {
      rows[[i]] <- current[match(matched[1], available_refs), , drop = FALSE]
    } else {
      rows[[i]] <- .normalize_profile_entries(n[i], report_type)
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

setMethod(
  "sections",
  signature(x = "camReport"),
  function(x, n, profile, report_type) {
    n_missing <- missing(n)
    if (n_missing) {
      n <- NULL
    } else if (!is.character(n)) {
      n <- NULL
      warning("`n` should be character; it is ignored.")
    }
    if (missing(profile)) profile <- NULL
    if (missing(report_type)) report_type <- "report"
    report_type <- match.arg(report_type, c("report", "status"))

    if (!is.null(profile)) {
      value <- .resolve_profile(profile, x)
      current <- methods::slot(value, report_type)
      if (is.null(n)) {
        return(.profile_reference(current))
      }
      if (length(n) == 0L) {
        stop("At least one module must be selected.")
      }

      selected <- .profile_entries_from_selection(n, current, report_type)
      methods::slot(value, report_type) <- selected
      methods::validObject(value)
      add_profile(x, value, overwrite = TRUE)
      .attach_profile_entries(x, selected, report_type)
      message("\nThe '", value@name, "' profile sections are updated.")
      return(invisible(x))
    }

    available <- .available_pool_names(x, report_type)
    if (is.null(n)) {
      return(available)
    }

    if (!all(n %in% available)) {
      all_names <- .module_pool_info(x, report_type)$name
      if (all(n %in% all_names)) {
        message(
          "\nSome specified sections are excluded because their test ",
          "results were problematic."
        )
      } else if (any(n %in% available)) {
        message(
          "\nSome specified section names are unknown and ignored. Use ",
          "section_names() to get the correct names."
        )
      } else {
        stop(
          "None of the specified section names are known. Use ",
          "section_names() to get the correct names of available sections."
        )
      }
    }

    n <- n[n %in% available]
    if (identical(report_type, "report")) {
      .attach_modules(x, n = n)
    } else {
      .attach_status_modules(x, n = n)
    }
    message("\nThe report sections are updated.")
    invisible(x)
  }
)

#-------
