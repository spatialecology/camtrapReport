test_that("offline core import does not need optional analytical packages", {
  optional <- c("taxize", "lutz", "spatstat", "camtrapDensity", "suncalc")
  original_require <- .require
  state <- new.env(parent = emptyenv())
  state$checked <- character()

  local_mocked_bindings(
    .require = function(package) {
      state$checked <- c(state$checked, package)

      if (package %in% optional) {
        return(FALSE)
      }

      original_require(package)
    },
    .getMissingTaxon_GBIF = function(...) {
      stop("Offline import must not query GBIF.")
    },
    .installPak = function(...) {
      stop("Core import must not install optional packages.")
    },
    .package = "camtrapReport"
  )

  expect_true(isNamespace(asNamespace("camtrapReport")))
  expect_false(any(
    optional %in% names(getNamespaceImports("camtrapReport"))
  ))

  value <- .read_camdp(camtrap_test_dataset(), enrich_taxonomy = FALSE)

  expect_named(
    value$data,
    c("observations", "deployments", "media", "locations", "sequences",
      "taxonomy")
  )
  expect_gt(nrow(value$data$observations), 0L)
  expect_gt(nrow(value$data$deployments), 0L)
  expect_true(all(
    value$data$observations$deploymentID %in%
      value$data$deployments$deploymentID
  ))
  expect_true(all(is.na(value$data$taxonomy$class)))
  expect_false(any(state$checked %in% optional))
})


test_that("selected module loader names missing packages without installing", {
  selected <- reportSection(
    name = "optional_module",
    packages = "spatstat",
    code = { 1L }
  )
  packages <- .collect_module_packages(list(selected))
  expect_identical(packages, "spatstat")

  loader <- .make_package_loader_chunk(packages, core = character())
  lines <- strsplit(loader, "\n", fixed = TRUE)[[1L]]
  code <- lines[!grepl("^```", lines)]
  scope <- new.env(parent = baseenv())
  state <- new.env(parent = emptyenv())
  state$checked <- character()
  state$attached <- FALSE
  state$installed <- FALSE

  scope$requireNamespace <- function(package, quietly) {
    state$checked <- c(state$checked, package)
    FALSE
  }
  scope$library <- function(...) {
    state$attached <- TRUE
    stop("Unavailable module packages must not be attached.")
  }
  scope$install.packages <- function(...) {
    state$installed <- TRUE
    stop("Module rendering must not install packages.")
  }

  expect_error(
    eval(parse(text = code), envir = scope),
    "Missing package(s): spatstat",
    fixed = TRUE
  )
  expect_identical(state$checked, "spatstat")
  expect_false(state$attached)
  expect_false(state$installed)
})


test_that("missing REM dependency warns but preserves NULL and cached results", {
  object <- methods::new("camReport")
  state <- new.env(parent = emptyenv())
  state$checked <- character()

  local_mocked_bindings(
    .require = function(package) {
      state$checked <- c(state$checked, package)
      FALSE
    },
    .eval = function(...) {
      stop("Missing dependencies must be reported before model evaluation.")
    },
    .installPak = function(...) {
      stop("REM parameter fitting must not install packages.")
    },
    .package = "camtrapReport"
  )

  cached_before <- object$.rem_params
  missing <- capture_report_conditions(
    object$.get_REM_Param("Toy species")
  )

  expect_null(missing$value)
  expect_identical(
    missing$warnings,
    paste0(
      'The "camtrapDensity" package is not installed; it is required ',
      "for REM parameter estimation. ",
      "Install it from https://camera-traps.r-universe.dev and retry."
    )
  )
  expect_length(missing$messages, 0L)
  expect_identical(object$.rem_params, cached_before)
  expect_identical(state$checked, "camtrapDensity")

  key <- .rem_parameter_key("Toy species")
  stored <- list(radius_model = list(estimate = "cached sentinel"))
  object$.rem_params[[key]] <- stored
  cached <- capture_report_conditions(object$.get_REM_Param("Toy species"))

  expect_identical(cached$value, stored)
  expect_length(cached$warnings, 0L)
  expect_length(cached$messages, 0L)
  expect_identical(state$checked, "camtrapDensity")
})


test_that("missing spatial dependency is diagnosed before point evaluation", {
  object <- methods::new("camReport")
  original_namespace <- base::requireNamespace
  state <- new.env(parent = emptyenv())
  state$evaluated <- FALSE

  local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "spatstat")) {
        return(FALSE)
      }

      original_namespace(package, ...)
    },
    .package = "base"
  )
  local_mocked_bindings(
    .eval = function(...) {
      state$evaluated <- TRUE
      stop("Unqualified point-pattern functions must not be evaluated.")
    },
    .require = function(...) {
      stop("Spatial availability checking must not attach packages.")
    },
    .installPak = function(...) {
      stop("Spatial density must not install packages.")
    },
    .package = "camtrapReport"
  )

  expect_error(
    object$spatial_density(
      x = data.frame(x = c(0, 1), y = c(0, 1), total_observations = c(1, 1))
    ),
    paste0(
      "Package 'spatstat' is required for spatial density. ",
      "Install it with install.packages('spatstat') and retry."
    ),
    fixed = TRUE
  )
  expect_false(state$evaluated)
})


test_that("available spatial dependency keeps existing evaluation unchanged", {
  object <- methods::new("camReport")
  original_namespace <- base::requireNamespace
  state <- new.env(parent = emptyenv())
  state$evaluated <- character()

  local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "spatstat")) {
        return(TRUE)
      }

      original_namespace(package, ...)
    },
    .package = "base"
  )
  local_mocked_bindings(
    .eval = function(code, env) {
      state$evaluated <- c(state$evaluated, code)
      stop("Existing point-pattern evaluation sentinel.")
    },
    .require = function(...) {
      stop("The availability guard must not newly attach spatstat.")
    },
    .package = "camtrapReport"
  )

  expect_error(
    object$spatial_density(
      x = data.frame(x = c(0, 1), y = c(0, 1), total_observations = c(1, 1))
    ),
    "Existing point-pattern evaluation sentinel.",
    fixed = TRUE
  )
  expect_identical(state$evaluated, "owin(xrange = xr, yrange = yr)")
})
