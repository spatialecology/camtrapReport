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


test_that("report refuses a missing output directory before rendering", {
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
      expect_error(
        report(
          report_object,
          filename = requested_stem,
          view = FALSE,
          test = FALSE
        ),
        "does not exist", fixed = TRUE
      )
    ),
    .generate_report = function(...) {
      stop("Synthetic rendering failure", call. = FALSE)
    },
    .package = "camtrapReport"
  )

  expect_false(dir.exists(missing_dir))
  expect_length(captured$warnings, 0L)
  expect_false(file.exists(file.path(fallback_dir, "fallback-report.Rmd")))
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

  on.exit(
    {
      options(viewer = old_viewer)
      unlink(output_dir, recursive = TRUE, force = TRUE)
    },
    add = TRUE
  )

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

test_that("explicit report paths do not depend on the dataset or working dir", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  root <- tempfile("camtrapReport-explicit-output-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  cm$info$directory <- NULL
  expect_false("getwd" %in% all.names(body(.resolve_report_output)))
  for (format in c("html", "pdf")) {
    destination <- .resolve_report_output(
      cm, file.path(root, paste0("explicit.", format)), "report",
      format = format, format_missing = TRUE
    )
    expect_identical(destination$output,
                     file.path(root, paste0("explicit.", format)))
    expect_identical(destination$format, format)
  }
  expect_error(.resolve_report_output(cm, "report", "report", "html", TRUE),
               "No valid dataset directory", fixed = TRUE)
  cm$info$directory <- file.path(root, "missing-dataset")
  expect_error(.resolve_report_output(cm, "report", "report", "html", TRUE),
               "does not exist", fixed = TRUE)
  cm$info$directory <- root
  destination <- .resolve_report_output(cm, "report", "report", "html", TRUE)
  expect_identical(destination$output, file.path(root, "report.html"))
})

test_that("existing report or Rmd files are protected before any rendering", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  root <- tempfile("camtrapReport-protected-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  state <- new.env(parent = emptyenv())
  state$renders <- 0L
  local_mocked_bindings(
    .generate_report = function(...) {
      state$renders <- state$renders + 1L
      stop("Rendering should not start")
    },
    .render_report_with_progress = function(...) {
      state$renders <- state$renders + 1L
      stop("Rendering should not start")
    },
    .package = "camtrapReport"
  )
  for (type in c("report", "status")) {
    generate <- if (identical(type, "report")) report else status
    for (format in c("html", "pdf")) {
      for (existing in c("output", "source", "both")) {
        stem <- file.path(root, paste(type, format, existing, sep = "-"))
        paths <- c(paste0(stem, ".", format), paste0(stem, ".Rmd"))
        keep <- switch(existing, output = 1L, source = 2L, both = 1:2)
        for (path in paths[keep]) writeLines("Original user content", path)
        captured <- capture_report_conditions(expect_error(
          generate(cm, filename = paths[1], profile = "EOW"),
          "would be overwritten", fixed = TRUE
        ))
        expect_length(captured$messages, 1L)
        expect_match(captured$messages[1], "Report output: ", fixed = TRUE)
        expect_match(captured$messages[1], "R Markdown source: ", fixed = TRUE)
        for (path in paths[keep]) {
          expect_identical(readLines(path), "Original user content")
        }
        expect_false(any(file.exists(paths[-keep])))
      }
    }
  }
  expect_identical(state$renders, 0L)
})

test_that("overwrite must be explicit and covers output and Rmd together", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  section <- reportSection(name = "safety", title = "Output safety")
  cm$reportObjects <- list(safety = section)
  cm$statusReportObjects <- list(safety = section)
  root <- tempfile("camtrapReport-overwrite-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  local_mocked_bindings(
    .render_report_with_progress = function(input, output_file, envir) {
      writeLines("Updated HTML", output_file)
      output_file
    },
    .convert_html_to_pdf = function(html_file, pdf_file) {
      expect_identical(readLines(html_file), "Updated HTML")
      writeLines("Updated PDF", pdf_file)
      pdf_file
    },
    .package = "camtrapReport"
  )
  for (type in c("report", "status")) {
    generate <- if (identical(type, "report")) report else status
    for (format in c("html", "pdf")) {
      stem <- file.path(root, paste(type, format, sep = "-"))
      output <- paste0(stem, ".", format)
      source <- paste0(stem, ".Rmd")
      writeLines("Original report", output)
      writeLines("Original Rmd", source)
      captured <- capture_report_conditions(generate(
        cm, filename = output, overwrite = TRUE
      ))
      expect_identical(normalizePath(captured$value), normalizePath(output))
      expect_identical(readLines(output),
                       if (format == "html") "Updated HTML" else "Updated PDF")
      expect_true(any(grepl("Output safety", readLines(source), fixed = TRUE)))
      expect_length(captured$messages, 1L)
    }
    for (invalid in list(NA, NULL, logical(), 1, c(TRUE, FALSE))) {
      expect_error(generate(cm, filename = tempfile(), overwrite = invalid),
                   "'overwrite' must be TRUE or FALSE", fixed = TRUE)
    }
  }
})

test_that("diagnostic recovery may reuse its own newly written Rmd", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  cm$reportObjects <- list(
    safety = reportSection(name = "safety", title = "Recovery output safety")
  )
  root <- tempfile("camtrapReport-recovery-output-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  output <- file.path(root, "report.html")
  state <- new.env(parent = emptyenv())
  state$renders <- 0L
  local_mocked_bindings(
    .generate_report = function(object, output_file, rmd_file, toc) {
      state$renders <- state$renders + 1L
      writeLines("Rmd written during this rendering attempt", rmd_file)
      if (state$renders == 1L) stop("Synthetic first-attempt error")
      writeLines("Recovered report", output_file)
      output_file
    },
    .QuickTestReportSection = function(...) TRUE,
    .package = "camtrapReport"
  )
  captured <- capture_report_conditions(report(
    cm, filename = output, test = TRUE
  ))
  expect_identical(state$renders, 2L)
  expect_identical(readLines(output), "Recovered report")
  expect_identical(normalizePath(captured$value), normalizePath(output))
})
