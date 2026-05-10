# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  May 2026
# Version 1.3
# Licence GPL v3
#--------


.getTextObj <- function(name = NULL, title = NULL, parent = NULL, headLevel = 1, txt = NULL) {
  new(".textSection", name = name, title = title, parent = parent, headLevel = headLevel, txt = txt)
}
#---


.getRchunk <- function(parent = NULL, name = NULL, setting = NULL, packages = NULL, code) {
  
  if (!is.null(setting) && as.character(substitute(setting))[1] == "{") {
    setting <- substitute(setting)
    setting <- as.character(setting)[-1]
    setting <- .trim(setting)
    setting <- .rmChar(setting, rm = c(1, 2), rmLast = TRUE)
  }
  
  #----
  code <- substitute(code)
  
  if (as.character(code)[1] != "{") {
    stop("code should be placed within { } ")
  }
  
  code <- paste(as.character(code)[-1], collapse = "\n")
  
  new(".Rchunk", parent = parent, name = name, setting = setting, packages = packages, code = code)
}


#----

.glueTextSection <- function(x, .envir) {
  
  # -------------------------
  # Title and text
  # -------------------------
  
  .title <- paste0(
    paste(rep("#", x@headLevel), collapse = ""),
    " ",
    x@title
  )
  
  if (!is.null(x@txt)) {
    .out <- paste0(
      c(
        .title,
        sapply(x@txt, glue::glue, .envir = .envir)
      ),
      collapse = "\n\n"
    )
  } else {
    .out <- paste0(.title, "\n\n")
  }
  
  # -------------------------
  # Helper to build one safe Rmd chunk
  # -------------------------
  
  .build_chunk <- function(.chunk) {
    
    # Name shown in live console messages
    .module_name <- .extract_chunk_name(
      code = .chunk@code,
      fallback = .chunk@name
    )
    
    # Real R Markdown chunk label.
    # Use .chunk@name here because it is usually unique.
    .chunk_header_name <- as.character(.chunk@name)[1]
    .chunk_header_name <- gsub("[^A-Za-z0-9_]+", "_", .chunk_header_name)
    
    if (is.na(.chunk_header_name) || !nzchar(.chunk_header_name)) {
      .chunk_header_name <- .module_name
    }
    
    .chunk_header_name <- gsub("[^A-Za-z0-9_]+", "_", .chunk_header_name)
    
    if (is.na(.chunk_header_name) || !nzchar(.chunk_header_name)) {
      .chunk_header_name <- "module"
    }
    
    # Wrap original code so errors are caught and the report can continue
    .safe_code <- .make_safe_module_code(
      code = .chunk@code,
      module_name = .module_name,
      show_note_in_report = TRUE
    )
    
    # Build chunk header
    if (is.null(.chunk@setting)) {
      
      .p1 <- paste0(
        "```{r ",
        .chunk_header_name,
        "}"
      )
      
    } else {
      
      .p1 <- paste0(
        "```{r ",
        .chunk_header_name,
        ",",
        paste(.chunk@setting, collapse = ","),
        "}"
      )
    }
    
    paste0(
      .p1,
      "\n",
      .safe_code,
      "\n",
      "```"
    )
  }
  
  # -------------------------
  # Add R chunks
  # -------------------------
  
  if (!is.null(x@Rchunk)) {
    
    if (is.list(x@Rchunk)) {
      
      for (i in seq_along(x@Rchunk)) {
        .p1 <- .build_chunk(x@Rchunk[[i]])
        .out <- paste0(.out, "\n\n", .p1, "\n\n")
      }
      
    } else {
      
      .p1 <- .build_chunk(x@Rchunk)
      .out <- paste0(.out, "\n\n", .p1, "\n\n")
    }
  }
  
  .out
}


#--------

if (!isGeneric("reportSection")) {
  setGeneric(
    "reportSection",
    function(name, title, parent, txt, code_setting, packages, code)
      standardGeneric("reportSection")
  )
}


setMethod(
  "reportSection",
  signature(name = "character"),
  function(name, title, parent, txt, code_setting, packages, code) {
    
    if (missing(title)) title <- ""
    if (missing(parent)) parent <- NULL
    if (missing(txt)) txt <- NULL
    if (missing(code_setting)) code_setting <- NULL
    
    if (missing(code)) {
      code <- NULL
    } else {
      code <- substitute(code)
    }
    
    # if (missing(object)) object <- NULL
    if (missing(packages)) packages <- NULL
    
    #------------
    .x <- .getTextObj(name = name, title = title, parent = parent, txt = txt)
    #------------
    
    if (!is.null(code)) {
      
      if (!is.null(code_setting) && as.character(substitute(code_setting))[1] == "{") {
        code_setting <- substitute(code_setting)
        code_setting <- as.character(code_setting)[-1]
        code_setting <- .trim(code_setting)
        code_setting <- .rmChar(code_setting, rm = c(1, 2), rmLast = TRUE)
      }
      
      #----
      if (as.character(code)[1] != "{") {
        stop("code should be placed within { } ")
      }
      
      code <- paste(as.character(code)[-1], collapse = "\n")
      
      .xc <- new(
        ".Rchunk",
        parent = parent,
        name = paste0(name, "_code"),
        setting = code_setting,
        packages = packages,
        code = code
      )
      
      .x@Rchunk <- .xc
    }
    
    .x
  }
)

#===========