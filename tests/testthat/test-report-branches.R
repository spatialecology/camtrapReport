make_test_output_directory <- function(prefix) {
  output_dir <- tempfile(prefix)

  if (!dir.create(output_dir)) {
    stop("Could not create the temporary test directory.")
  }

  output_dir
}


normalise_test_path <- function(path) {
  normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )
}


test_that("report handles NULL and empty filenames", {
  skip_if_not(rmarkdown::pandoc_available())
  local_math_free_html_fixture()

  cm <- camtrap_test_report()$copy(shallow = FALSE)

  output_dir <- make_test_output_directory(
    "camtrapReport-report-default-"
  )

  on.exit(
    unlink(output_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  cm$info$directory <- output_dir

  cm$reportObjects <- list(
    path_test = reportSection(
      name = "path_test",
      title = "Path test",
      txt = "Testing the default ecological-report path."
    )
  )

  output_null <- report(
    cm,
    filename = NULL,
    view = FALSE,
    test = FALSE
  )

  expect_identical(
    basename(output_null),
    "report.html"
  )

  expect_true(
    file.exists(output_null)
  )

  output_empty <- report(
    cm,
    filename = "",
    view = FALSE,
    test = FALSE,
    overwrite = TRUE
  )

  expect_identical(
    basename(output_empty),
    "report.html"
  )

  expect_true(
    file.exists(output_empty)
  )
})


test_that("status handles NULL and empty filenames", {
  skip_if_not(rmarkdown::pandoc_available())
  local_math_free_html_fixture()

  cm <- camtrap_test_report()$copy(shallow = FALSE)

  output_dir <- make_test_output_directory(
    "camtrapReport-status-default-"
  )

  on.exit(
    unlink(output_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  cm$info$directory <- output_dir

  cm$statusReportObjects <- list(
    status_path_test = reportSection(
      name = "status_path_test",
      title = "Status path test",
      txt = "Testing the default data-status-report path."
    )
  )

  output_null <- status(
    cm,
    filename = NULL,
    view = FALSE
  )

  expect_identical(
    basename(output_null),
    "data_status.html"
  )

  expect_true(
    file.exists(output_null)
  )

  output_empty <- status(
    cm,
    filename = "",
    view = FALSE,
    overwrite = TRUE
  )

  expect_identical(
    basename(output_empty),
    "data_status.html"
  )

  expect_true(
    file.exists(output_empty)
  )
})


test_that("report rejects a nonexistent requested directory", {
  skip_if_not(rmarkdown::pandoc_available())

  cm <- camtrap_test_report()$copy(shallow = FALSE)

  output_dir <- make_test_output_directory(
    "camtrapReport-report-fallback-"
  )

  on.exit(
    unlink(output_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  cm$info$directory <- output_dir

  cm$reportObjects <- list(
    fallback_test = reportSection(
      name = "fallback_test",
      title = "Fallback test",
      txt = "Testing fallback from a nonexistent output directory."
    )
  )

  requested_file <- file.path(
    output_dir,
    "directory-that-does-not-exist",
    "custom-report"
  )

  expect_error(
    report(
      cm,
      filename = requested_file,
      view = FALSE,
      test = FALSE
    ),
    "does not exist"
  )

  unexpected_output <- file.path(
    output_dir,
    "custom-report.html"
  )

  expect_false(file.exists(unexpected_output))
  expect_false(dir.exists(dirname(requested_file)))
})


test_that("status rejects a nonexistent requested directory", {
  skip_if_not(rmarkdown::pandoc_available())

  cm <- camtrap_test_report()$copy(shallow = FALSE)

  output_dir <- make_test_output_directory(
    "camtrapReport-status-fallback-"
  )

  on.exit(
    unlink(output_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  cm$info$directory <- output_dir

  cm$statusReportObjects <- list(
    status_fallback_test = reportSection(
      name = "status_fallback_test",
      title = "Status fallback test",
      txt = "Testing status-report output-directory fallback."
    )
  )

  requested_file <- file.path(
    output_dir,
    "directory-that-does-not-exist",
    "custom-status"
  )

  expect_error(
    status(
      cm,
      filename = requested_file,
      view = FALSE
    ),
    "does not exist"
  )

  unexpected_output <- file.path(
    output_dir,
    "custom-status.html"
  )

  expect_false(file.exists(unexpected_output))
  expect_false(dir.exists(dirname(requested_file)))
})


test_that("report sends the generated file to the configured viewer", {
  skip_if_not(rmarkdown::pandoc_available())
  local_math_free_html_fixture()

  cm <- camtrap_test_report()$copy(shallow = FALSE)

  output_dir <- make_test_output_directory(
    "camtrapReport-report-viewer-"
  )

  on.exit(
    unlink(output_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  cm$info$directory <- output_dir

  cm$reportObjects <- list(
    viewer_test = reportSection(
      name = "viewer_test",
      title = "Viewer test",
      txt = "Testing the report viewer."
    )
  )

  viewer_state <- new.env(parent = emptyenv())
  viewer_state$file <- NULL

  old_options <- options(
    camtrapReport.progress = TRUE,
    viewer = function(path) {
      viewer_state$file <- path
    }
  )

  on.exit(
    options(old_options),
    add = TRUE
  )

  output <- capture_expected_message(
    report(
      cm,
      filename = "viewer-report",
      view = TRUE,
      test = FALSE
    ),
    "Report generated at:"
  )

  expect_identical(
    normalise_test_path(viewer_state$file),
    normalise_test_path(output)
  )

  expect_true(
    file.exists(output)
  )
})


test_that("status sends the generated file to the configured viewer", {
  skip_if_not(rmarkdown::pandoc_available())
  local_math_free_html_fixture()

  cm <- camtrap_test_report()$copy(shallow = FALSE)

  output_dir <- make_test_output_directory(
    "camtrapReport-status-viewer-"
  )

  on.exit(
    unlink(output_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  cm$info$directory <- output_dir

  cm$statusReportObjects <- list(
    status_viewer_test = reportSection(
      name = "status_viewer_test",
      title = "Status viewer test",
      txt = "Testing the status-report viewer."
    )
  )

  viewer_state <- new.env(parent = emptyenv())
  viewer_state$file <- NULL

  old_options <- options(
    camtrapReport.progress = TRUE,
    viewer = function(path) {
      viewer_state$file <- path
    }
  )

  on.exit(
    options(old_options),
    add = TRUE
  )

  output <- capture_expected_message(
    status(
      cm,
      filename = "viewer-status",
      view = TRUE
    ),
    "Report generated at:"
  )

  expect_identical(
    normalise_test_path(viewer_state$file),
    normalise_test_path(output)
  )

  expect_true(
    file.exists(output)
  )
})
