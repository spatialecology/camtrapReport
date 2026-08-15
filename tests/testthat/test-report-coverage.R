test_that("report uses defaults when arguments are omitted", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)

  output <- testthat::with_mocked_bindings(
    report(cm),
    .generate_report = function(...) {
      invisible(NULL)
    },
    .package = "camtrapReport"
  )

  expect_identical(
    basename(output),
    "report.html"
  )

  expect_identical(
    normalizePath(
      dirname(output),
      winslash = "/",
      mustWork = FALSE
    ),
    normalizePath(
      tempdir(),
      winslash = "/",
      mustWork = FALSE
    )
  )
})


test_that("report sends successful output to configured viewer", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)

  viewer_state <- new.env(parent = emptyenv())
  viewer_state$called <- FALSE
  viewer_state$path <- NULL

  old_viewer <- getOption("viewer")

  options(
    viewer = function(path) {
      viewer_state$called <- TRUE
      viewer_state$path <- path
    }
  )

  on.exit(
    options(viewer = old_viewer),
    add = TRUE
  )

  output_stem <- tempfile(
    "camtrapReport-view-success-"
  )

  output <- suppressMessages(
    testthat::with_mocked_bindings(
      report(
        cm,
        filename = output_stem,
        view = TRUE,
        test = FALSE
      ),
      .generate_report = function(...) {
        invisible(NULL)
      },
      .package = "camtrapReport"
    )
  )

  expect_true(viewer_state$called)

  expect_identical(
    viewer_state$path,
    output
  )

  expect_identical(
    normalizePath(
      output,
      winslash = "/",
      mustWork = FALSE
    ),
    normalizePath(
      paste0(output_stem, ".html"),
      winslash = "/",
      mustWork = FALSE
    )
  )
})


test_that(
  "report test mode stops when all modules were already tested",
  {
    cm <- camtrap_test_report()$copy(shallow = FALSE)

    modules_info <-
      cm$reportObjectElements$Modules_info

    expect_s3_class(
      modules_info,
      "data.frame"
    )

    expect_true(
      "tested" %in% names(modules_info)
    )

    cm$reportObjectElements$Modules_info$tested[] <-
      TRUE

    expect_error(
      testthat::with_mocked_bindings(
        report(
          cm,
          filename = tempfile(
            "camtrapReport-all-tested-"
          ),
          view = FALSE,
          test = TRUE
        ),
        .generate_report = function(...) {
          stop(
            "Synthetic rendering failure",
            call. = FALSE
          )
        },
        .package = "camtrapReport"
      ),
      "Although all sections are tested",
      fixed = TRUE
    )
  }
)
