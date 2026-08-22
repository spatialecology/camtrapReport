.extract_chunk_name <- function(code, fallback = "module") {
  fallback <- as.character(fallback)[1]
  
  if (is.na(fallback) || !nzchar(fallback)) {
    fallback <- "module"
  }
  
  fallback <- gsub("[^A-Za-z0-9_]+", "_", fallback)
  
  if (is.null(code) || length(code) == 0 || is.na(code[1])) {
    return(fallback)
  }
  
  code <- paste(as.character(code), collapse = "\n")
  code_lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  
  hit <- grep("^\\s*#\\|\\s*name\\s*:", code_lines, value = TRUE)
  
  if (length(hit) > 0) {
    out <- sub("^\\s*#\\|\\s*name\\s*:\\s*", "", hit[1])
    out <- trimws(out)
    out <- gsub("[^A-Za-z0-9_]+", "_", out)
    
    if (!is.na(out) && nzchar(out)) {
      return(out)
    }
  }
  
  fallback
}

#--------

.html_escape_base <- function(x) {
  if (missing(x) || is.null(x) || length(x) == 0) {
    return("")
  }
  
  x <- as.character(x[1])
  
  if (is.na(x)) {
    x <- ""
  }
  
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  
  x
}

# Keep old helper name used elsewhere in the package.
.html_attr_escape <- .html_escape_base

#--------

.make_safe_module_code <- function(
  code,
  module_name = NULL,
  show_note_in_report = TRUE
) {
  if (is.null(code) || length(code) == 0 || is.na(code[1])) {
    return("")
  }
  
  paste(as.character(code), collapse = "\n")
}

#--------
#' Create the environment used to render report modules
#'
#' Creates the evaluation environment used when report-module code is
#' executed during report generation. The environment provides access to
#' the current `camReport` object, selected object fields, and internal
#' helper functions required by report modules.
#'
#' The current report object is available as `object`, `cm`, and `.self`.
#' The environment also provides wrappers for `getFigureNumber()` and
#' `getTableNumber()` used during report generation.
#'
#' This function is intended for internal use. Contributors developing
#' report modules should normally use the objects and helpers exposed by
#' this environment rather than modifying the rendering environment itself.
#'
#' @param object A `camReport` object being used to generate a report.
#'
#' @return An environment used to evaluate report-module code.
#'
#' @keywords internal
#' @noRd
.make_render_env <- function(object) {
  env <- new.env(parent = parent.frame())
  
  env$object <- object
  env$cm <- object
  env$.self <- object
  
  helpers <- list(
    .paste_comma_and = .paste_comma_and,
    .trim = .trim,
    .trim_chr = .trim_chr,
    .pretty_label = .pretty_label,
    .firstUpper = .firstUpper,
    .format_duration = .format_duration,
    .format_file_size = .format_file_size,
    .getYear = .getYear,
    .get_Time_length = .get_Time_length,
    .get_hour = .get_hour,
    .html_escape_base = .html_escape_base,
    .html_attr_escape = .html_attr_escape,
    .plot_effort = .plot_effort,
    .basic_corrplot = .basic_corrplot
  )
  
  for (nm in names(helpers)) {
    assign(
      nm,
      helpers[[nm]],
      envir = env
    )
  }
  
  field_names <- character()
  
  field_names <- tryCatch(
    names(object$getRefClass()$fields()),
    error = function(e) character()
  )
  
  if (length(field_names) == 0 && exists("camR", inherits = TRUE)) {
    field_names <- tryCatch(
      names(camR$fields()),
      error = function(e) character()
    )
  }
  
  if (length(field_names) > 0) {
    for (nm in field_names) {
      val <- tryCatch(object[[nm]], error = function(e) NULL)
      assign(nm, val, envir = env)
    }
  }
  
  env$getFigureNumber <- function(...) object$getFigureNumber(...)
  env$getTableNumber <- function(...) object$getTableNumber(...)
  
  env
}

#--------

