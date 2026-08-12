.camtrap_test_cache <- new.env(parent = emptyenv())

camtrap_test_dataset <- function() {
  path <- system.file("external/dataset", package = "camtrapReport")

  if (!nzchar(path) || !dir.exists(path)) {
    stop("The bundled Camtrap DP toy dataset is not available.")
  }

  path
}

copy_camtrap_test_dataset <- function() {
  source <- camtrap_test_dataset()
  root <- tempfile("camtrapReport-test-data-")

  if (!dir.create(root)) {
    stop("Could not create a temporary directory for the test dataset.")
  }

  copied <- file.copy(source, root, recursive = TRUE, copy.date = TRUE)

  if (!all(copied)) {
    stop("Could not copy the bundled test dataset.")
  }

  file.path(root, basename(source))
}

camtrap_test_report <- function() {
  if (!exists(
    "report",
    envir = .camtrap_test_cache,
    inherits = FALSE
  )) {
    dataset <- copy_camtrap_test_dataset()
    object <- NULL
    
    # Routine tests must not contact GBIF.
    original_require <- get(
      ".require",
      envir = asNamespace("camtrapReport")
    )
    
    testthat::local_mocked_bindings(
      .require = function(x) {
        package <- as.character(x)[1]
        
        if (
          !is.na(package) &&
          identical(package, "taxize")
        ) {
          return(FALSE)
        }
        
        original_require(x)
      },
      .package = "camtrapReport"
    )
    
    invisible(
      capture.output(
        object <- withCallingHandlers(
          suppressMessages(
            camData(
              dataset,
              update = TRUE
            )
          ),
          warning = function(w) {
            if (grepl(
              "chi^2 approximation may be inaccurate",
              conditionMessage(w),
              fixed = TRUE
            )) {
              invokeRestart("muffleWarning")
            }
          }
        )
      )
    )
    
    assign(
      "report",
      object,
      envir = .camtrap_test_cache
    )
  }
  
  get(
    "report",
    envir = .camtrap_test_cache,
    inherits = FALSE
  )
}

find_test_report_section <- function(x, name) {
  if (methods::is(x, ".textSection")) {
    if (identical(x@name, name)) {
      return(x)
    }

    return(NULL)
  }

  if (is.list(x)) {
    for (element in x) {
      found <- find_test_report_section(element, name)

      if (!is.null(found)) {
        return(found)
      }
    }
  }

  NULL
}

copy_camtrap_module_library <- function() {
  source <- system.file("reportSections", package = "camtrapReport")

  if (!nzchar(source) || !dir.exists(source)) {
    stop("The bundled report module library is not available.")
  }

  root <- tempfile("camtrapReport-test-modules-")

  if (!dir.create(root)) {
    stop("Could not create a temporary module directory.")
  }

  copied <- file.copy(source, root, recursive = TRUE, copy.date = TRUE)

  if (!all(copied)) {
    stop("Could not copy the report module library.")
  }

  file.path(root, basename(source))
}

write_test_module <- function(path, name, parent = ".root", title = name) {
  parent_value <- if (identical(parent, ".root")) "null" else {
    paste0('"', parent, '"')
  }

  writeLines(
    c(
      "---",
      paste0('name: "', name, '"'),
      paste0('title: "', title, '"'),
      paste0("parent: ", parent_value),
      'text: "A module used by the camtrapReport test suite."',
      "code: null",
      "---"
    ),
    path
  )

  path
}

report_test_loader_packages <- function(rmd) {
  loader_lines <- grep("^pkgs <- c\\(", trimws(rmd), value = TRUE)

  if (length(loader_lines) == 0L) {
    return(character())
  }

  quoted <- regmatches(
    loader_lines,
    gregexpr('"[^"]+"', loader_lines)
  )

  unique(gsub('^"|"$', "", unlist(quoted, use.names = FALSE)))
}
