# Functions for generating camtrapReport ecological reports
# Licence: MIT
#--------

setGeneric(
  "report",
  function(object, filename, view, test, profile, format) {
    methods::standardGeneric("report")
  }
)

.generate_report <- function(object, output_file, rmd_file, toc = TRUE) {
  object$generateReport(
    output_file = output_file,
    rmd_file = rmd_file,
    toc = toc
  )
}

.resolve_report_output <- function(
  object,
  filename,
  default_name,
  format,
  format_missing
) {
  if (is.null(filename) || length(filename) == 0L || is.na(filename[1]) ||
      !nzchar(filename[1])) {
    filename <- default_name
  }
  filename <- as.character(filename)[1]

  extension <- if (grepl("\\.[^.]+$", basename(filename))) {
    tolower(sub("^.*\\.", "", basename(filename)))
  } else {
    ""
  }
  extension_format <- if (extension %in% c("html", "htm")) {
    "html"
  } else if (identical(extension, "pdf")) {
    "pdf"
  } else {
    NULL
  }

  if (isTRUE(format_missing)) {
    format <- extension_format %||% "html"
  } else {
    format <- match.arg(tolower(as.character(format)[1]), c("html", "pdf"))
    if (!is.null(extension_format) && !identical(format, extension_format)) {
      stop(
        "The filename extension and 'format' disagree: .", extension,
        " versus format = '", format, "'."
      )
    }
  }

  supplied_dir <- dirname(filename)
  supplied_stem <- basename(filename)
  if (!is.null(extension_format)) {
    supplied_stem <- sub("\\.[^.]+$", "", supplied_stem)
  }

  base_dir <- tryCatch(
    normalizePath(object$info$directory, winslash = "/", mustWork = TRUE),
    error = function(e) getwd()
  )

  if (identical(supplied_dir, ".")) {
    out_dir <- base_dir
  } else {
    out_dir <- tryCatch(
      normalizePath(supplied_dir, winslash = "/", mustWork = TRUE),
      error = function(e) {
        warning(
          'The directory specified in "filename" ("', supplied_dir,
          '") does not exist; the default path is used instead.'
        )
        base_dir
      }
    )
  }

  list(
    stem = file.path(out_dir, supplied_stem),
    format = format,
    output = paste0(file.path(out_dir, supplied_stem), ".", format)
  )
}

.convert_html_to_pdf <- function(html_file, pdf_file) {
  if (!requireNamespace("pagedown", quietly = TRUE)) {
    stop(
      "PDF output requires the optional 'pagedown' package and a supported ",
      "Chrome or Edge browser. Install 'pagedown' and try again.",
      call. = FALSE
    )
  }

  progress <- .report_progress_enabled()
  if (progress) {
    message(
      "[camtrapReport] Starting PDF browser conversion; page-print timeout: ",
      "60 s after browser startup."
    )
  }
  .with_report_stage("PDF conversion/browser resource waits", tryCatch(
    pagedown::chrome_print(
      input = html_file, output = pdf_file, timeout = 60,
      verbose = if (progress) 1 else 0
    ),
    error = function(e) {
      stop(
        "The HTML report was created, but conversion to PDF failed. ",
        "Check Chrome or Edge and page resources (printing timeout: 60 s). ",
        "Original error: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  ))
  normalizePath(pdf_file, winslash = "/", mustWork = TRUE)
}

.open_generated_report <- function(path, format) {
  .with_report_stage("Opening completed report", {
    viewer <- if (identical(format, "html")) getOption("viewer") else NULL
    if (!is.null(viewer)) {
      viewer(path)
    } else {
      utils::browseURL(path)
    }
  })
  invisible(path)
}

.test_profile_report_modules <- function(object, profile, path) {
  entries <- profile@report
  attached_names <- vapply(
    .flatten_attached_sections(object$reportObjects),
    function(x) x@name,
    character(1)
  )
  entries <- entries[entries$module %in% attached_names, , drop = FALSE]
  passed <- logical(nrow(entries))

  for (i in seq_len(nrow(entries))) {
    pool <- .module_pool(object, entries$source[i])
    module <- pool[[entries$module[i]]]
    passed[i] <- isTRUE(.QuickTestReportSection(module, object, path = path))

    info_name <- if (identical(entries$source[i], "report")) {
      "Modules_info"
    } else {
      "Status_modules_info"
    }
    info <- object$reportObjectElements[[info_name]]
    info$tested[info$name == entries$module[i]] <- passed[i]
    object$reportObjectElements[[info_name]] <- info
  }

  entries[passed, , drop = FALSE]
}

#' Generate ecological and data-status reports
#'
#' Generate reports from a [`camReport`][camReport-classes] object. `report()`
#' creates an ecological report, while [status()] creates a data-status report.
#' A named `profile` can select an ordered set of modules immediately before
#' rendering.
#'
#' HTML is the default output. PDF output first renders the existing HTML report
#' and then prints it through a headless Chrome or Edge browser using the
#' optional `pagedown` package. This route preserves modules that contain HTML
#' widgets or HTML-formatted tables. Interactive controls are naturally static
#' in the resulting PDF. PDF output omits the table of contents and excludes
#' modules whose registry entry does not support PDF. The `formats` column
#' shown by [list_Modules()] records module compatibility.
#'
#' @details
#' Output includes the rendered `.html` or `.pdf` file and an `.Rmd` source
#' file with the same stem. If no path is given, the object's dataset directory
#' is used when available, otherwise the current working directory is used.
#' Existing files with the chosen stem can be overwritten. Supply a new,
#' explicit `filename` for each retained report; use [tempfile()] for a preview.
#'
#' `view = TRUE` opens the completed file after rendering; it does not select
#' different modules or dependencies. To inspect a file that was already
#' generated, call `utils::browseURL(report_file)` instead of rendering it again.
#' A failure in the R Markdown `setup` chunk commonly reports a missing package
#' required by the selected modules. Read that original error before using
#' `test = TRUE`: recovery tests only the attached selection, preserves its
#' order, and keeps passing modules whose parent sections also pass. Descendants
#' of failing parents are excluded. A shorter report is an expected diagnostic
#' outcome, not an equivalent successful full report.
#'
#' Use `profile = "default"` or `profile = "EOW"` to select a known report
#' structure explicitly. `section_names()` without a profile lists the complete
#' available module pool, including alternative modules; it is not the same as
#' a profile's selection. Reread corrected source files with
#' `camData(..., update = TRUE)` before rerunning `status()` or `report()`.
#'
#' Rendering is quiet by default. Set `options(camtrapReport.progress = TRUE)`
#' to show console progress for module construction, individual chunks, HTML
#' rendering, PDF conversion, and opening the completed file. Timings include
#' elapsed time and CPU time in the current R process; Pandoc and browser CPU
#' time is not included in the latter. The last started chunk identifies where
#' a long-running computation is executing. PDF page loading and printing use
#' a 60-second timeout after browser startup; analytical computations are not
#' interrupted by that timeout.
#'
#' @param object A [`camReport`][camReport-classes] object created by
#'   [camData()].
#' @param filename An optional output filename or path. A `.html` or `.pdf`
#'   extension selects the format when `format` is omitted. Without an
#'   extension, the defaults are `"report"` and `"data_status"`.
#' @param view A logical value (default `FALSE`) specifying whether to open the
#'   generated report.
#' @param test A logical value (default `FALSE`). If `TRUE`, ecological report
#'   modules are tested when HTML rendering fails.
#' @param profile An optional profile name or `reportProfile` object. When
#'   omitted, the sections currently attached to `object` are rendered.
#' @param format Output format, either `"html"` or `"pdf"`. The default is
#'   inferred from `filename`, falling back to `"html"`.
#'
#' @return Invisibly returns the generated report path.
#'
#' @seealso [reportProfile()], [sections()], [section_names()], [camData()]
#' @family report generation
#'
#' @usage
#' report(object, filename, view, test, profile, format)
#'
#' status(object, filename, view, profile, format)
#' @name report
#' @aliases report status report,camReport-method status,camReport-method
#'
#' @examples
#' \donttest{
#' if (rmarkdown::pandoc_available()) {
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
#' cm <- sections(cm, "introduction")
#' report_file <- report(
#'   cm,
#'   filename = tempfile("camtrapReport-report-"),
#'   view = FALSE
#' )
#' file.exists(report_file)
#' unlink(c(report_file, sub("\\.html$", ".Rmd", report_file)), force = TRUE)
#' unlink(example_dataset, recursive = TRUE, force = TRUE)
#' }
#' }
setMethod(
  "report",
  signature(object = "camReport"),
  function(
    object,
    filename = "report",
    view = FALSE,
    test = FALSE,
    profile = NULL,
    format = c("html", "pdf")
  ) {
    if (missing(filename)) filename <- "report"
    if (missing(view)) view <- FALSE
    if (missing(test)) test <- FALSE
    if (missing(profile)) profile <- NULL
    format_missing <- missing(format)
    destination <- .resolve_report_output(
      object = object,
      filename = filename,
      default_name = "report",
      format = format,
      format_missing = format_missing
    )

    profile_value <- NULL
    if (!is.null(profile)) {
      profile_value <- .resolve_profile(profile, object)
      .apply_profile(object, profile_value, report_type = "report")
    }

    selected_objects <- object$reportObjects

    restore_report_objects <- function() {
      object$reportObjects <- selected_objects
    }

    on.exit(restore_report_objects(), add = TRUE)
    .prepare_attached_modules_for_format(
      object,
      report_type = "report",
      format = destination$format
    )

    html_file <- if (identical(destination$format, "html")) {
      destination$output
    } else {
      tempfile("camtrapReport-pdf-source-", fileext = ".html")
    }
    if (identical(destination$format, "pdf")) {
      on.exit(unlink(html_file, force = TRUE), add = TRUE)
    }

    rendered <- try(
      .generate_report(
        object = object,
        output_file = html_file,
        rmd_file = paste0(destination$stem, ".Rmd"),
        toc = identical(destination$format, "html")
      ),
      silent = TRUE
    )

    if (inherits(rendered, "try-error")) {
      if (!isTRUE(test)) {
        message(
          "Report generation is stopped because of an error; add ",
          "`test = TRUE` ",
          "to identify and exclude modules that cause an error."
        )
        return(rendered)
      }

      message("\nTesting of modules is started....")
      temp_path <- tempfile("camtrapReport-module-test-")
      if (!dir.create(temp_path, recursive = TRUE, showWarnings = FALSE)) {
        stop("Could not create a temporary directory for module testing.")
      }
      on.exit(unlink(temp_path, recursive = TRUE, force = TRUE), add = TRUE)

      if (!is.null(profile_value)) {
        passed <- .test_profile_report_modules(
          object,
          profile_value,
          path = temp_path
        )
        passed <- .prune_profile_selection(passed, object)
        if (nrow(passed) == 0L) {
          stop("No modules in the selected profile passed module testing.")
        }
        .attach_profile_entries(object, passed, report_type = "report")
      } else {
        selected <- .flatten_attached_sections(object$reportObjects)
        passing <- list()
        for (module in selected) {
          parent <- .norm_parent(module@parent)
          if (!identical(parent, ".root") &&
              !parent %in% names(passing)) {
            next
          }

          # Use cached tests only for an unchanged, unambiguous pool module.
          sources <- c("report", "status")
          same <- vapply(sources, function(source) {
            original <- .module_pool(object, source)[[module@name]]
            !is.null(original) &&
              identical(original@title, module@title) &&
              identical(original@txt, module@txt) &&
              identical(original@Rchunk, module@Rchunk)
          }, logical(1))
          info_name <- if (sum(same) == 1L) {
            if (identical(sources[same], "report")) {
              "Modules_info"
            } else {
              "Status_modules_info"
            }
          } else {
            NULL
          }
          info <- if (!is.null(info_name)) {
            object$reportObjectElements[[info_name]]
          } else {
            NULL
          }
          row <- match(module@name, info$name)
          result <- if (length(row) == 1L && !is.na(row) &&
                        "tested" %in% names(info)) {
            info$tested[row]
          } else {
            NA
          }
          if (length(result) != 1L || is.na(result)) {
            result <- .QuickTestReportSection(
              module, object, path = temp_path
            )
            if (length(row) == 1L && !is.na(row)) {
              info$tested[row] <- isTRUE(result)
              object$reportObjectElements[[info_name]] <- info
            }
          }
          if (isTRUE(result)) passing[[module@name]] <- module
        }
        if (length(passing) == 0L) {
          stop("No ecological report modules passed module testing.")
        }
        object$reportObjects <- list()
        for (module in passing) object$addReportObject(module)
      }

      message(
        "\nTesting is done; passing modules are attached and report ",
        "generation is restarted."
      )
      return(report(
        object,
        filename = destination$stem,
        view = view,
        test = FALSE,
        profile = NULL,
        format = destination$format
      ))
    }

    if (identical(destination$format, "pdf")) {
      .convert_html_to_pdf(html_file, destination$output)
    }

    if (isTRUE(view)) {
      if (.report_progress_enabled()) {
        message(
          "Report generated at: ",
          normalizePath(destination$output, winslash = "/", mustWork = FALSE)
        )
      }
      .open_generated_report(destination$output, destination$format)
    }

    invisible(destination$output)
  }
)
