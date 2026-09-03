.camtrap_test_cache <- new.env(parent = emptyenv())
.camtrap_test_cache$temp_paths <- character()

reg.finalizer(
  .camtrap_test_cache,
  function(cache) {
    unlink(
      cache$temp_paths,
      recursive = TRUE,
      force = TRUE
    )
  },
  onexit = TRUE
)

.register_camtrap_test_path <- function(path) {
  .camtrap_test_cache$temp_paths <- unique(c(
    .camtrap_test_cache$temp_paths,
    path
  ))

  path
}

camtrap_test_dataset <- function() {
  path <- system.file("external/dataset", package = "camtrapReport")

  if (!nzchar(path) || !dir.exists(path)) {
    stop("The bundled Leuven Camtrap DP example dataset is not available.")
  }

  path
}

copy_camtrap_test_dataset <- function() {
  source <- camtrap_test_dataset()
  root <- .register_camtrap_test_path(
    tempfile("camtrapReport-test-data-")
  )

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
    
    # Keep the shared fixture independent of optional module packages. These
    # packages are exercised through mocks in their own unit tests; routine
    # fixture creation must not change with the runner's installed packages or
    # contact external services such as GBIF.
    optional_packages <- c(
      "activity",
      "camtrapDensity",
      "camtraptor",
      "corrplot",
      "Distance",
      "dygraphs",
      "ggplot2",
      "ggrepel",
      "gt",
      "htmltools",
      "htmlwidgets",
      "iNEXT",
      "leaflet",
      "lutz",
      "magick",
      "readr",
      "sbd",
      "scales",
      "sf",
      "spatstat",
      "spatstat.explore",
      "spatstat.geom",
      "suncalc",
      "taxize",
      "tidyr",
      "withr",
      "xts"
    )

    original_require <- get(
      ".require",
      envir = asNamespace("camtrapReport")
    )
    
    testthat::local_mocked_bindings(
      .require = function(x) {
        package <- as.character(x)[1]
        
        if (!is.na(package) && package %in% optional_packages) {
          return(FALSE)
        }
        
        original_require(x)
      },
      .package = "camtrapReport"
    )
    
    object <- withCallingHandlers(
      suppressMessages(
        camData(
          dataset,
          update = TRUE
        )
      ),
      warning = function(w) {
        expected_warning <- grepl(
          "chi^2 approximation may be inaccurate",
          conditionMessage(w),
          fixed = TRUE
        ) || grepl(
          "package is not installed; it is required",
          conditionMessage(w),
          fixed = TRUE
        )

        if (expected_warning) {
          invokeRestart("muffleWarning")
        }
      }
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

  root <- .register_camtrap_test_path(
    tempfile("camtrapReport-test-modules-")
  )

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
