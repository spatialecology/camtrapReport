# Internal report-module evaluation and rendering utilities
# Licence: MIT
#--------

# Evaluate code stored in report-module YAML files. Module code is intentionally
# represented as text so optional packages can remain module-specific; callers
# must supply the environment containing the camReport data and settings. Only
# modules from trusted sources should be evaluated.
.eval <- function(x, env) {
  if (missing(x) || is.null(x) || length(x) == 0) {
    return(NULL)
  }
  
  if (missing(env) || is.null(env)) {
    env <- parent.frame()
  }
  
  eval(parse(text = x), envir = env)
}

#--------
# Safe module rendering helpers
#--------

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

.make_safe_module_code <- function(code, module_name = NULL, show_note_in_report = TRUE) {
  if (is.null(code) || length(code) == 0 || is.na(code[1])) {
    return("")
  }
  
  paste(as.character(code), collapse = "\n")
}

#--------

# Build the environment used when rendering module code. The central object is
# exposed under its historical aliases, selected internal formatting helpers and
# object fields are copied in, and assignments made by a module remain outside
# the user's global environment.
.make_render_env <- function(object) {
  env <- new.env(parent = parent.frame())
  
  env$object <- object
  env$cm <- object
  env$.self <- object
  
  helper_names <- c(
    ".paste_comma_and",
    ".trim",
    ".trim_chr",
    ".pretty_label",
    ".firstUpper",
    ".format_duration",
    ".format_file_size",
    ".getYear",
    ".get_Time_length",
    ".get_hour",
    ".html_escape_base",
    ".html_attr_escape"
  )
  
  for (nm in helper_names) {
    if (exists(nm, mode = "function", inherits = TRUE)) {
      assign(nm, get(nm, mode = "function", inherits = TRUE), envir = env)
    }
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

