# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  April 2026
# Version 1.0
# Licence GPL v3
#--------

.reportSection_catalog <- function(node, path = character()) {
  out <- list()
  
  walk <- function(x, path) {
    if (inherits(x, ".textSection")) {
      out[[length(out) + 1L]] <<- data.frame(
        name   = x@name,
        title  = x@title %||% "",
        parent = .norm_parent(x@parent),
        path   = paste(c(path, x@name), collapse = " / "),
        stringsAsFactors = FALSE
      )
      return(invisible(NULL))
    }
    
    if (is.list(x)) {
      nms <- names(x)
      for (i in seq_along(x)) {
        nm <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else as.character(i)
        walk(x[[i]], c(path, nm))
      }
    }
    
    invisible(NULL)
  }
  
  walk(node, path)
  
  if (length(out) == 0L) {
    return(data.frame(
      name = character(),
      title = character(),
      parent = character(),
      path = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  do.call(rbind, out)
}

.matchReportSection <- function(catalog,
                                section,
                                by = c("auto", "name", "title"),
                                ignore.case = TRUE) {
  by <- match.arg(by)
  
  if (!nzchar(section)) stop("'section' is empty.")
  
  cmp <- function(x, y) {
    if (ignore.case) tolower(x) == tolower(y) else x == y
  }
  
  contains <- function(x, y) {
    if (ignore.case) grepl(y, x, ignore.case = TRUE, fixed = TRUE)
    else grepl(y, x, fixed = TRUE)
  }
  
  if (by == "name") {
    w <- which(cmp(catalog$name, section))
    if (length(w) == 0) w <- which(contains(catalog$name, section))
  } else if (by == "title") {
    w <- which(cmp(catalog$title, section))
    if (length(w) == 0) w <- which(contains(catalog$title, section))
  } else {
    w <- which(cmp(catalog$name, section))
    if (length(w) == 0) w <- which(cmp(catalog$title, section))
    if (length(w) == 0) w <- which(contains(catalog$name, section))
    if (length(w) == 0) w <- which(contains(catalog$title, section))
  }
  
  if (length(w) == 0L) {
    stop("No report section matched '", section, "'.")
  }
  
  if (length(w) > 1L) {
    msg <- paste0(
      "More than one section matched '", section, "'.\n",
      paste0(" - ", catalog$path[w], collapse = "\n"),
      "\nUse 'by = \"name\"' or give a more specific section name/title."
    )
    stop(msg)
  }
  
  catalog[w, , drop = FALSE]
}

.capture_code_text <- function(arg, env = parent.frame()) {
  expr <- substitute(arg)
  
  # code passed as { ... }
  if (is.call(expr) && identical(expr[[1]], as.name("{"))) {
    lines <- vapply(as.list(expr)[-1], function(e) {
      paste(deparse(e), collapse = "\n")
    }, character(1))
    return(paste(lines, collapse = "\n"))
  }
  
  # character value passed directly
  val <- try(eval(expr, envir = env), silent = TRUE)
  if (!inherits(val, "try-error") && is.character(val)) {
    return(paste(val, collapse = "\n"))
  }
  
  # fallback: deparse expression
  paste(deparse(expr), collapse = "\n")
}

.capture_setting_text <- function(arg, env = parent.frame()) {
  expr <- substitute(arg)
  
  if (is.null(expr)) return(NULL)
  
  # setting passed as { c(...) }
  if (is.call(expr) && identical(expr[[1]], as.name("{")) && length(expr) == 2L) {
    expr <- expr[[2]]
  }
  
  val <- try(eval(expr, envir = env), silent = TRUE)
  
  if (!inherits(val, "try-error")) {
    if (is.null(val)) return(NULL)
    
    if (is.character(val) && length(val) == 1L) {
      return(val)
    }
    
    if (is.atomic(val)) {
      if (!is.null(names(val)) && any(nzchar(names(val)))) {
        parts <- mapply(
          function(nm, vv) paste0(nm, " = ", deparse(vv)),
          names(val), as.list(val),
          SIMPLIFY = TRUE, USE.NAMES = FALSE
        )
        return(paste(parts, collapse = ", "))
      } else {
        return(paste(as.character(val), collapse = ", "))
      }
    }
    
    if (is.list(val) && !is.null(names(val))) {
      parts <- mapply(
        function(nm, vv) paste0(nm, " = ", deparse(vv)),
        names(val), val,
        SIMPLIFY = TRUE, USE.NAMES = FALSE
      )
      return(paste(parts, collapse = ", "))
    }
  }
  
  paste(deparse(expr), collapse = "")
}
#------------

.update_section_chunk <- function(sec,
                                  code_missing,
                                  code = NULL,
                                  code_name = NULL,
                                  code_setting_missing,
                                  code_setting = NULL,
                                  packages_missing,
                                  packages = NULL,
                                  append_code = FALSE) {
  wants_chunk_update <- (!code_missing) || (!code_setting_missing) || (!packages_missing)
  
  if (!wants_chunk_update) {
    return(sec)
  }
  
  # helper to update one existing chunk
  patch_chunk <- function(ch) {
    if (!code_missing) {
      ch@code <- if (append_code && nzchar(ch@code)) {
        paste(ch@code, code, sep = "\n")
      } else {
        code
      }
    }
    if (!code_setting_missing) ch@setting <- code_setting
    if (!packages_missing) ch@packages <- if (is.null(packages)) NULL else as.character(packages)
    ch
  }
  
  # no chunk exists yet
  if (is.null(sec@Rchunk)) {
    chunk_name <- code_name %||% paste0(sec@name, "__code")
    sec@Rchunk <- .getRchunk(
      parent = sec@name,
      name = chunk_name,
      code = if (code_missing) "" else code,
      setting = if (code_setting_missing) NULL else code_setting,
      packages = if (packages_missing) NULL else packages
    )
    return(sec)
  }
  
  # single chunk
  if (inherits(sec@Rchunk, ".Rchunk")) {
    if (!is.null(code_name) && !identical(sec@Rchunk@name, code_name)) {
      # user wants a second/new chunk
      new_chunk <- .getRchunk(
        parent = sec@name,
        name = code_name,
        code = if (code_missing) "" else code,
        setting = if (code_setting_missing) NULL else code_setting,
        packages = if (packages_missing) NULL else packages
      )
      sec@Rchunk <- list(sec@Rchunk, new_chunk)
      names(sec@Rchunk) <- c(sec@Rchunk[[1]]@name, new_chunk@name)
      return(sec)
    }
    
    sec@Rchunk <- patch_chunk(sec@Rchunk)
    return(sec)
  }
  
  # multiple chunks
  if (is.list(sec@Rchunk)) {
    if (is.null(code_name)) {
      if (length(sec@Rchunk) != 1L) {
        stop(
          "Section '", sec@name,
          "' has multiple code chunks; please supply 'code_name'."
        )
      }
      idx <- 1L
    } else {
      idx <- which(vapply(sec@Rchunk, function(ch) inherits(ch, ".Rchunk") && identical(ch@name, code_name), logical(1)))
      if (length(idx) == 0L) {
        new_chunk <- .getRchunk(
          parent = sec@name,
          name = code_name,
          code = if (code_missing) "" else code,
          setting = if (code_setting_missing) NULL else code_setting,
          packages = if (packages_missing) NULL else packages
        )
        sec@Rchunk[[code_name]] <- new_chunk
        return(sec)
      }
      idx <- idx[1]
    }
    
    sec@Rchunk[[idx]] <- patch_chunk(sec@Rchunk[[idx]])
    return(sec)
  }
  
  sec
}
#---------
.updateReportSection_tree <- function(node, target_name, updater) {
  if (inherits(node, ".textSection")) {
    if (identical(node@name, target_name)) {
      return(updater(node))
    }
    return(node)
  }
  
  if (is.list(node)) {
    for (i in seq_along(node)) {
      node[[i]] <- .updateReportSection_tree(node[[i]], target_name, updater)
    }
  }
  
  node
}
.norm_parent <- camtrapReport:::.norm_parent
#-------




if (!isGeneric("updateReportSection")) {
  setGeneric("updateReportSection", function(x,section,text,title,code,code_name,
                                             code_setting,packages,append_text,
                                             append_code)
    standardGeneric("updateReportSection"))
}


setMethod('updateReportSection', signature(x='camReport'), 
          function(x,section,text,title,code,code_name,
                   code_setting,packages,append_text,
                   append_code) {
            
            if (missing(text)) text <- NULL
            if (missing(title)) title <- NULL
            if (missing(code)) code <- NULL
            if (missing(code_name)) code_name <- NULL
            if (missing(code_setting)) code_setting <- NULL
            if (missing(packages)) packages <- NULL
            if (missing(packages)) packages <- FALSE
            if (missing(append_code)) append_code <- FALSE
            #----------
            if (missing(section) || !is.character(section) || length(section) != 1L) {
              stop("'section' should be a single character string (name or title).")
            }
            
            catalog <- .reportSection_catalog(x$reportObjects)
            
            if (nrow(catalog) == 0L) {
              stop("No report sections were found in x$reportObjects.")
            }
            
            hit <- .matchReportSection(
              catalog = catalog,
              section = section,
              by = 'auto',
              ignore.case = TRUE
            )
            
            target_name <- hit$name[1]
            
            code_missing <- missing(code)
            code_setting_missing <- missing(code_setting)
            packages_missing <- missing(packages)
            
            if (!code_missing) {
              code <- .capture_code_text(code, env = parent.frame())
            }
            
            if (!code_setting_missing) {
              code_setting <- .capture_setting_text(code_setting, env = parent.frame())
            }
            
            updater <- function(sec) {
              if (!is.null(title)) {
                sec@title <- as.character(title)[1]
              }
              
              if (!is.null(text)) {
                if (append_text) {
                  old_txt <- .collapse_section_text(sec@txt)
                  sec@txt <- if (nzchar(old_txt)) {
                    paste(old_txt, as.character(text), sep = "\n\n")
                  } else {
                    as.character(text)
                  }
                } else {
                  sec@txt <- as.character(text)
                }
              }
              
              sec <- .update_section_chunk(
                sec = sec,
                code_missing = code_missing,
                code = code,
                code_name = code_name,
                code_setting_missing = code_setting_missing,
                code_setting = code_setting,
                packages_missing = packages_missing,
                packages = packages,
                append_code = append_code
              )
              
              sec
            }
            
            x$reportObjects <- .updateReportSection_tree(
              node = x$reportObjects,
              target_name = target_name,
              updater = updater
            )
            
            invisible(x)
          }
)
#--------

if (!isGeneric("listReportSections")) {
  setGeneric("listReportSections", function(x)
    standardGeneric("listReportSections"))
}


setMethod('listReportSections', signature(x='camReport'), 
          function(x) {
            .reportSection_catalog(x$reportObjects)
          }
)
