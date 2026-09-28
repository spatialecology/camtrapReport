# Functions for generating camtrapReport data-status reports
# Licence: MIT
#--------

setGeneric(
  "status",
  function(object, filename, view, profile, format) {
    methods::standardGeneric("status")
  }
)

setMethod(
  "status",
  signature(object = "camReport"),
  function(
    object,
    filename = "data_status",
    view = FALSE,
    profile = NULL,
    format = c("html", "pdf")
  ) {
    if (missing(filename)) filename <- "data_status"
    if (missing(view)) view <- FALSE
    if (missing(profile)) profile <- NULL
    format_missing <- missing(format)
    destination <- .resolve_report_output(
      object = object,
      filename = filename,
      default_name = "data_status",
      format = format,
      format_missing = format_missing
    )

    if (!is.null(profile)) {
      .apply_profile(object, profile, report_type = "status")
    }

    selected_objects <- object$statusReportObjects
    
    restore_status_objects <- function() {
      object$statusReportObjects <- selected_objects
    }
    
    on.exit(restore_status_objects(), add = TRUE)
    .prepare_attached_modules_for_format(
      object,
      report_type = "status",
      format = destination$format
    )

    html_file <- if (identical(destination$format, "html")) {
      destination$output
    } else {
      tempfile("camtrapStatus-pdf-source-", fileext = ".html")
    }
    if (identical(destination$format, "pdf")) {
      on.exit(unlink(html_file, force = TRUE), add = TRUE)
    }

    object$generateStatusReport(
      output_file = html_file,
      rmd_file = paste0(destination$stem, ".Rmd"),
      toc = identical(destination$format, "html")
    )

    if (identical(destination$format, "pdf")) {
      .convert_html_to_pdf(html_file, destination$output)
    }

    if (isTRUE(view)) {
      message(
        "Report generated at: ",
        normalizePath(destination$output, winslash = "/", mustWork = FALSE)
      )
      .open_generated_report(destination$output, destination$format)
    }

    invisible(destination$output)
  }
)

#--------
