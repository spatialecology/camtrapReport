# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update : June 2026
# Version 1.6
# Licence MIT
#--------


.getTextObj <- function(name = NULL,
                        title = NULL,
                        parent = NULL,
                        headLevel = 1,
                        txt = NULL) {
  new(
    ".textSection",
    name = name,
    title = title,
    parent = parent,
    headLevel = headLevel,
    txt = txt
  )
}

#--------

.getRchunk <- function(parent = NULL,
                       name = NULL,
                       setting = NULL,
                       packages = NULL,
                       code) {
  
  if (!is.null(setting) && as.character(substitute(setting))[1] == "{") {
    setting <- substitute(setting)
    setting <- as.character(setting)[-1]
    setting <- .trim(setting)
    setting <- .rmChar(setting, rm = c(1, 2), rmLast = TRUE)
  }
  
  code <- substitute(code)
  
  if (as.character(code)[1] != "{") {
    stop("code should be placed within { } ")
  }
  
  code <- paste(as.character(code)[-1], collapse = "\n")
  
  new(
    ".Rchunk",
    parent = parent,
    name = name,
    setting = setting,
    packages = packages,
    code = code
  )
}

#--------

.clean_chunk_name <- function(x, fallback = "module") {
  x <- as.character(x)[1]
  
  if (is.na(x) || !nzchar(x)) {
    x <- fallback
  }
  
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  
  if (is.na(x) || !nzchar(x)) {
    x <- "module"
  }
  
  x
}

#--------

.protect_pandoc_attrs_one <- function(z) {
  if (is.na(z) || !nzchar(z)) {
    return(list(text = z, keys = character(), vals = character()))
  }
  
  # Protect Pandoc heading attributes from glue(), for example:
  # {.unnumbered}
  # {.tabset}
  # {.tabset .unnumbered}
  # {#id .class}
  # {#id .tabset .unnumbered}
  m <- gregexpr("\\{[[:space:]]*[#\\.][^\\{\\}\\n]*\\}", z, perl = TRUE)
  vals <- regmatches(z, m)[[1]]
  
  if (length(vals) == 0 || identical(vals, character(0))) {
    return(list(text = z, keys = character(), vals = character()))
  }
  
  keys <- paste0("___CAMTRAP_PANDOC_ATTR_", seq_along(vals), "___")
  
  for (i in seq_along(vals)) {
    z <- sub(vals[i], keys[i], z, fixed = TRUE)
  }
  
  list(text = z, keys = keys, vals = vals)
}

#--------

.restore_pandoc_attrs_one <- function(z, keys, vals) {
  if (length(keys) == 0) {
    return(z)
  }
  
  for (i in seq_along(keys)) {
    z <- gsub(keys[i], vals[i], z, fixed = TRUE)
  }
  
  z
}

#--------

.safe_glue_text <- function(txt, .envir, section_name = "unknown") {
  if (missing(txt) || is.null(txt) || length(txt) == 0) {
    return(character())
  }
  
  if (missing(.envir) || is.null(.envir)) {
    .envir <- parent.frame()
  }
  
  txt <- as.character(txt)
  
  out <- vapply(
    txt,
    FUN = function(z) {
      
      if (is.na(z)) {
        return("")
      }
      
      protected <- .protect_pandoc_attrs_one(z)
      
      glued <- try(
        glue::glue(protected$text, .envir = .envir),
        silent = TRUE
      )
      
      if (inherits(glued, "try-error")) {
        stop(
          "Failed to evaluate text in report section '",
          section_name,
          "'. Original error: ",
          conditionMessage(attr(glued, "condition")),
          call. = FALSE
        )
      }
      
      glued <- as.character(glued)
      .restore_pandoc_attrs_one(glued, protected$keys, protected$vals)
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
  
  out
}

#--------

.glueRchunk <- function(x, .envir = parent.frame()) {
  
  if (is.null(x) || !inherits(x, ".Rchunk")) {
    return("")
  }
  
  chunk_name <- .clean_chunk_name(x@name)
  
  setting <- x@setting
  
  if (is.null(setting) || length(setting) == 0 || all(is.na(setting))) {
    chunk_header <- paste0("```{r ", chunk_name, "}")
  } else {
    setting <- as.character(setting)
    setting <- setting[!is.na(setting)]
    setting <- trimws(setting)
    setting <- setting[nzchar(setting)]
    
    if (length(setting) == 0) {
      chunk_header <- paste0("```{r ", chunk_name, "}")
    } else {
      chunk_header <- paste0(
        "```{r ",
        chunk_name,
        ", ",
        paste(setting, collapse = ", "),
        "}"
      )
    }
  }
  
  code <- x@code
  
  if (is.null(code) || length(code) == 0 || all(is.na(code))) {
    code <- ""
  } else {
    code <- paste(as.character(code), collapse = "\n")
  }
  
  paste0(
    chunk_header,
    "\n",
    code,
    "\n",
    "```"
  )
}

#--------

.glueTextSection <- function(x, .envir) {
  
  if (is.null(x) || !inherits(x, ".textSection")) {
    return("")
  }
  
  if (missing(.envir) || is.null(.envir)) {
    .envir <- parent.frame()
  }
  
  section_name <- x@name
  
  if (is.null(section_name) || length(section_name) == 0 || is.na(section_name[1])) {
    section_name <- "unknown"
  }
  
  out <- character()
  
  # -------------------------
  # Section title
  # -------------------------
  
  title <- x@title
  
  if (!is.null(title) && length(title) > 0 && !is.na(title[1]) && nzchar(title[1])) {
    
    head_level <- x@headLevel
    
    if (is.null(head_level) || length(head_level) == 0 || is.na(head_level[1])) {
      head_level <- 1
    }
    
    head_level <- suppressWarnings(as.integer(head_level[1]))
    
    if (is.na(head_level) || head_level < 1) {
      head_level <- 1
    }
    
    title <- .safe_glue_text(
      txt = title,
      .envir = .envir,
      section_name = section_name
    )
    
    out <- c(
      out,
      paste0(
        paste(rep("#", head_level), collapse = ""),
        " ",
        title
      )
    )
  }
  
  # -------------------------
  # Section text
  # -------------------------
  
  if (!is.null(x@txt) && length(x@txt) > 0) {
    
    txt <- .safe_glue_text(
      txt = x@txt,
      .envir = .envir,
      section_name = section_name
    )
    
    txt <- txt[nzchar(txt)]
    
    if (length(txt) > 0) {
      out <- c(out, txt)
    }
  }
  
  # -------------------------
  # R chunks
  # -------------------------
  
  if (!is.null(x@Rchunk)) {
    
    if (is.list(x@Rchunk)) {
      
      chunk_text <- vapply(
        x@Rchunk,
        FUN = function(z) {
          if (inherits(z, ".Rchunk")) {
            .glueRchunk(z, .envir = .envir)
          } else {
            ""
          }
        },
        FUN.VALUE = character(1),
        USE.NAMES = FALSE
      )
      
      chunk_text <- chunk_text[nzchar(chunk_text)]
      
      if (length(chunk_text) > 0) {
        out <- c(out, chunk_text)
      }
      
    } else if (inherits(x@Rchunk, ".Rchunk")) {
      
      chunk_text <- .glueRchunk(x@Rchunk, .envir = .envir)
      
      if (nzchar(chunk_text)) {
        out <- c(out, chunk_text)
      }
    }
  }
  
  paste(out, collapse = "\n\n")
}

#--------
if (!isGeneric("reportSection")) {
  setGeneric("reportSection",function(name, title, parent, txt, code_setting, packages, code)
    standardGeneric("reportSection")
  )
}

setMethod("reportSection",signature(name = "character"),
  function(name,
           title,
           parent,
           txt,
           code_setting,
           packages,
           code) {
    
    if (missing(title)) title <- ""
    if (missing(parent)) parent <- NULL
    if (missing(txt)) txt <- NULL
    if (missing(code_setting)) code_setting <- NULL
    if (missing(packages)) packages <- NULL
    
    .x <- .getTextObj(
      name = name,
      title = title,
      parent = parent,
      txt = txt
    )
    
    if (!missing(code)) {
      
      code_expr <- substitute(code)
      
      if (!is.null(code_setting) &&
          as.character(substitute(code_setting))[1] == "{") {
        
        code_setting <- substitute(code_setting)
        code_setting <- as.character(code_setting)[-1]
        code_setting <- .trim(code_setting)
        code_setting <- .rmChar(code_setting, rm = c(1, 2), rmLast = TRUE)
      }
      
      if (as.character(code_expr)[1] != "{") {
        stop("code should be placed within { } ")
      }
      
      code_txt <- paste(as.character(code_expr)[-1], collapse = "\n")
      
      .xc <- new(
        ".Rchunk",
        parent = parent,
        name = paste0(name, "_code"),
        setting = code_setting,
        packages = packages,
        code = code_txt
      )
      
      .x@Rchunk <- .xc
    }
    
    .x
  }
)

#--------