library(testthat)

capture_report_conditions <- function(expr) {
  captured <- new.env(parent = emptyenv())
  captured$messages <- character()
  captured$warnings <- character()
  
  value <- withCallingHandlers(
    expr,
    message = function(condition) {
      captured$messages <- c(
        captured$messages,
        conditionMessage(condition)
      )
      invokeRestart("muffleMessage")
    },
    warning = function(condition) {
      captured$warnings <- c(
        captured$warnings,
        conditionMessage(condition)
      )
      invokeRestart("muffleWarning")
    }
  )
  
  list(
    value = value,
    messages = captured$messages,
    warnings = captured$warnings
  )
}


test_that("report returns rendering errors without crashing", {
  original <- camtrap_test_report()
  report_object <- original$copy(shallow = FALSE)
  
  output_dir <- tempfile("camtrapReport-report-error-")
  dir.create(output_dir)
  
  on.exit(
    unlink(output_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  
  captured <- testthat::with_mocked_bindings(
    capture_report_conditions(
      report(
        report_object,
        filename = file.path(output_dir, "ecological-report"),
        view = FALSE,
        test = FALSE
      )
    ),
    .generate_report = function(...) {
      stop("Synthetic rendering failure", call. = FALSE)
    },
    .package = "camtrapReport"
  )
  
  expect_s3_class(captured$value, "try-error")
  
  expect_true(
    any(grepl(
      "Report generation is stopped because of an error",
      captured$messages,
      fixed = TRUE
    ))
  )
  
  expect_match(
    as.character(captured$value),
    "Synthetic rendering failure",
    fixed = TRUE
  )
})


test_that("report warns and falls back when output directory is missing", {
  original <- camtrap_test_report()
  report_object <- original$copy(shallow = FALSE)
  
  fallback_dir <- tempfile("camtrapReport-fallback-")
  dir.create(fallback_dir)
  
  on.exit(
    unlink(fallback_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  
  report_object$info$directory <- fallback_dir
  
  missing_dir <- file.path(fallback_dir, "missing-directory")
  requested_stem <- file.path(missing_dir, "fallback-report")
  
  captured <- testthat::with_mocked_bindings(
    capture_report_conditions(
      report(
        report_object,
        filename = requested_stem,
        view = FALSE,
        test = FALSE
      )
    ),
    .generate_report = function(...) {
      stop("Synthetic rendering failure", call. = FALSE)
    },
    .package = "camtrapReport"
  )
  
  expect_s3_class(captured$value, "try-error")
  expect_false(dir.exists(missing_dir))
  
  expect_true(
    any(grepl(
      "does not exist",
      captured$warnings,
      fixed = TRUE
    ))
  )
  
  expect_match(
    as.character(captured$value),
    "Synthetic rendering failure",
    fixed = TRUE
  )
})


test_that("report does not invoke viewer after rendering failure", {
  original <- camtrap_test_report()
  report_object <- original$copy(shallow = FALSE)
  
  output_dir <- tempfile("camtrapReport-viewer-")
  dir.create(output_dir)
  
  old_viewer <- getOption("viewer")
  viewer_state <- new.env(parent = emptyenv())
  viewer_state$called <- FALSE
  
  options(
    viewer = function(path) {
      viewer_state$called <- TRUE
    }
  )
  
  on.exit({
    options(viewer = old_viewer)
    unlink(output_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  
  captured <- testthat::with_mocked_bindings(
    capture_report_conditions(
      report(
        report_object,
        filename = file.path(output_dir, "viewer-report"),
        view = TRUE,
        test = FALSE
      )
    ),
    .generate_report = function(...) {
      stop("Synthetic rendering failure", call. = FALSE)
    },
    .package = "camtrapReport"
  )
  
  expect_s3_class(captured$value, "try-error")
  expect_false(viewer_state$called)
})
