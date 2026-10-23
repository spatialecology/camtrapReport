test_that("console stage timers preserve results and report failures", {
  previous <- options(camtrapReport.progress = TRUE)
  on.exit(options(previous), add = TRUE)
  captured <- capture_report_conditions(
    withVisible(.with_report_stage("Stage test", invisible(42L)))
  )
  expect_identical(captured$value$value, 42L)
  expect_false(captured$value$visible)
  expect_true(any(grepl("Stage test started", captured$messages, fixed = TRUE)))
  expect_true(any(grepl("Stage test completed", captured$messages,
                        fixed = TRUE)))

  captured <- capture_report_conditions(expect_error(
    .with_report_stage("Failing stage", stop("original error")),
    "original error",
    fixed = TRUE
  ))
  expect_true(any(grepl("Failing stage stopped", captured$messages,
                        fixed = TRUE)))
})

test_that("render progress stays in the console and restores knitr hooks", {
  previous <- options(camtrapReport.progress = TRUE)
  on.exit(options(previous), add = TRUE)
  skip_if_not(rmarkdown::pandoc_available())
  source_file <- tempfile(fileext = ".Rmd")
  output <- tempfile(fileext = ".html")
  on.exit(unlink(c(source_file, output), force = TRUE), add = TRUE)
  writeLines(c(
    "---", "title: Progress smoke test", "output: html_document", "---",
    "```{r progress_value, echo=FALSE, message=FALSE}",
    "cat('Preserved report value:', sum(1:3))", "```"
  ), source_file)
  saved_chunks <- knitr::opts_chunk$get()
  saved_options <- knitr::opts_hooks$get()
  saved_hooks <- knitr::knit_hooks$get()

  captured <- capture_report_conditions(.render_report_with_progress(
    input = source_file, output_file = output, envir = new.env()
  ))
  expect_true(file.exists(captured$value))
  expect_true(any(grepl("Module/chunk progress_value started",
                        captured$messages, fixed = TRUE)))
  expect_true(any(grepl("Module/chunk progress_value completed",
                        captured$messages, fixed = TRUE)))
  expect_true(any(grepl("HTML rendering/Pandoc completed",
                        captured$messages, fixed = TRUE)))
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "Preserved report value: 6", fixed = TRUE)
  expect_false(grepl("[camtrapReport]", html, fixed = TRUE))
  expect_identical(knitr::opts_chunk$get(), saved_chunks)
  expect_identical(knitr::opts_hooks$get(), saved_options)
  expect_identical(knitr::knit_hooks$get(), saved_hooks)
})

test_that("failed rendering identifies the active chunk and restores hooks", {
  previous <- options(camtrapReport.progress = TRUE)
  on.exit(options(previous), add = TRUE)
  skip_if_not(rmarkdown::pandoc_available())
  source_file <- tempfile(fileext = ".Rmd")
  output <- tempfile(fileext = ".html")
  on.exit(unlink(c(source_file, output), force = TRUE), add = TRUE)
  writeLines(c(
    "---", "output: html_document", "---",
    "```{r failing_progress, error=FALSE}",
    "stop('intentional progress error')", "```"
  ), source_file)
  saved_chunks <- knitr::opts_chunk$get()
  saved_options <- knitr::opts_hooks$get()
  saved_hooks <- knitr::knit_hooks$get()

  captured <- capture_report_conditions(expect_error(
    .render_report_with_progress(source_file, output, new.env()),
    "intentional progress error",
    fixed = TRUE
  ))
  expect_true(any(grepl("Module/chunk failing_progress stopped",
                        captured$messages, fixed = TRUE)))
  expect_false(any(grepl("HTML rendering/Pandoc started",
                         captured$messages, fixed = TRUE)))
  expect_identical(knitr::opts_chunk$get(), saved_chunks)
  expect_identical(knitr::opts_hooks$get(), saved_options)
  expect_identical(knitr::knit_hooks$get(), saved_hooks)
})

test_that("progress does not invalidate existing knitr chunk caches", {
  previous <- options(camtrapReport.progress = TRUE)
  on.exit(options(previous), add = TRUE)
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile("camtrapReport-progress-cache-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  source_file <- file.path(root, "cached.Rmd")
  output <- file.path(root, "cached.html")
  counter <- file.path(root, "evaluations.txt")
  writeLines(c(
    "---", "title: Cache smoke test", "output: html_document", "---",
    "```{r cached_progress, cache=TRUE, echo=FALSE}",
    paste0("cat('x', file=", encodeString(counter, quote = '"'),
           ", append=TRUE)"),
    "```"
  ), source_file)
  rmarkdown::render(source_file, output_file = output, envir = new.env(),
                    quiet = TRUE)
  cached <- list.files(root, recursive = TRUE)
  captured <- capture_report_conditions(.render_report_with_progress(
    source_file, output, new.env()
  ))
  expect_identical(readLines(counter, warn = FALSE), "x")
  expect_identical(list.files(root, recursive = TRUE), cached)
  expect_true(any(grepl("Module/chunk cached_progress completed",
                        captured$messages, fixed = TRUE)))
  expect_false(any(grepl("Module/chunk cached_progress stopped",
                         captured$messages, fixed = TRUE)))
})

test_that("progress composes custom hooks without adding cache option keys", {
  previous <- options(camtrapReport.progress = TRUE)
  on.exit(options(previous), add = TRUE)
  skip_if_not(rmarkdown::pandoc_available())
  root <- tempfile("camtrapReport-progress-custom-cache-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  source_file <- file.path(root, "cached.Rmd")
  output <- file.path(root, "cached.html")
  counter <- file.path(root, "evaluations.txt")
  writeLines(c(
    "---", "title: Custom cache smoke test", "output: html_document", "---",
    "```{r cached_progress, cache=TRUE, echo=FALSE}",
    paste0("cat('x', file=", encodeString(counter, quote = '"'),
           ", append=TRUE)"), "```"
  ), source_file)
  saved_options <- knitr::opts_hooks$get()
  on.exit(knitr::opts_hooks$restore(saved_options), add = TRUE)
  state <- new.env(parent = emptyenv())
  state$labels <- character()
  knitr::opts_hooks$set(
    cache = function(options) {
      options$cache.extra <- sort(names(options))
      options
    },
    label = function(options) {
      state$labels <- c(state$labels, options$label)
      options
    }
  )
  rmarkdown::render(source_file, output_file = output, envir = new.env(),
                    quiet = TRUE)
  capture_report_conditions(.render_report_with_progress(
    source_file, output, new.env()
  ))
  expect_identical(readLines(counter, warn = FALSE), "x")
  expect_identical(state$labels, rep("cached_progress", 2L))
  expect_identical(names(knitr::opts_hooks$get()),
                   names(c(saved_options, list(cache = NULL, label = NULL))))
})

test_that("PDF conversion supplies a finite browser printing timeout", {
  previous <- options(camtrapReport.progress = TRUE)
  on.exit(options(previous), add = TRUE)
  skip_if_not_installed("pagedown")
  output <- tempfile(fileext = ".pdf")
  on.exit(unlink(output, force = TRUE), add = TRUE)
  state <- new.env(parent = emptyenv())
  captured <- testthat::with_mocked_bindings(
    capture_report_conditions(.convert_html_to_pdf("input.html", output)),
    chrome_print = function(input, output, timeout, verbose) {
      state$timeout <- timeout
      state$verbose <- verbose
      writeLines("PDF conversion stub", output)
    },
    .package = "pagedown"
  )
  expect_identical(state$timeout, 60)
  expect_identical(state$verbose, 1)
  expect_true(file.exists(captured$value))
  expect_true(any(grepl("PDF conversion/browser resource waits completed",
                        captured$messages, fixed = TRUE)))
})

test_that("viewer progress surrounds only opening a completed report", {
  state <- new.env(parent = emptyenv())
  state$opened <- NULL
  previous <- options(
    camtrapReport.progress = TRUE,
    viewer = function(path) {
      state$opened <- path
    }
  )
  on.exit(options(previous), add = TRUE)
  captured <- capture_report_conditions(
    .open_generated_report("completed.html", "html")
  )
  expect_identical(state$opened, "completed.html")
  expect_identical(captured$value, "completed.html")
  expect_true(any(grepl("Opening completed report completed",
                        captured$messages, fixed = TRUE)))
})

test_that("routine stage progress is quiet by default without hiding conditions", {
  previous <- options(camtrapReport.progress = NULL)
  on.exit(options(previous), add = TRUE)
  expect_false(.report_progress_enabled())
  expect_null(.report_stage_start("Silent stage"))
  captured <- capture_report_conditions(
    withVisible(.with_report_stage("Silent stage", {
      warning("original stage warning")
      invisible(42L)
    }))
  )
  expect_identical(captured$value$value, 42L)
  expect_false(captured$value$visible)
  expect_length(captured$messages, 0L)
  expect_identical(captured$warnings, "original stage warning")
  expect_error(.with_report_stage("Silent failure", stop("original error")),
               "original error", fixed = TRUE)
})

test_that("quiet HTML rendering preserves content and restores knitr hooks", {
  skip_if_not(rmarkdown::pandoc_available())
  previous <- options(camtrapReport.progress = NULL)
  on.exit(options(previous), add = TRUE)
  source_file <- tempfile(fileext = ".Rmd")
  output <- tempfile(fileext = ".html")
  on.exit(unlink(c(source_file, output), force = TRUE), add = TRUE)
  writeLines(c(
    "---", "title: Quiet smoke test", "output: html_document", "---",
    "```{r quiet_value, echo=FALSE}",
    "cat('Preserved quiet report value:', sum(1:3))",
    "warning('Preserved report warning')", "```"
  ), source_file)
  saved_chunks <- knitr::opts_chunk$get()
  saved_options <- knitr::opts_hooks$get()
  saved_hooks <- knitr::knit_hooks$get()
  captured <- capture_report_conditions(.render_report_with_progress(
    source_file, output, new.env()
  ))
  expect_true(file.exists(captured$value))
  expect_length(captured$messages, 0L)
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "Preserved quiet report value: 6", fixed = TRUE)
  expect_match(html, "Preserved report warning", fixed = TRUE)
  expect_false(grepl("[camtrapReport]", html, fixed = TRUE))
  expect_identical(knitr::opts_chunk$get(), saved_chunks)
  expect_identical(knitr::opts_hooks$get(), saved_options)
  expect_identical(knitr::knit_hooks$get(), saved_hooks)

  writeLines(c(
    "---", "output: html_document", "---",
    "```{r quiet_failure, error=FALSE}",
    "stop('original quiet render error')", "```"
  ), source_file)
  captured <- capture_report_conditions(expect_error(
    .render_report_with_progress(source_file, output, new.env()),
    "original quiet render error", fixed = TRUE
  ))
  expect_false(any(grepl("[camtrapReport]", captured$messages, fixed = TRUE)))
  expect_identical(knitr::opts_chunk$get(), saved_chunks)
  expect_identical(knitr::opts_hooks$get(), saved_options)
  expect_identical(knitr::knit_hooks$get(), saved_hooks)
})

test_that("quiet PDF conversion preserves warnings, errors and its deadline", {
  skip_if_not_installed("pagedown")
  previous <- options(camtrapReport.progress = NULL)
  on.exit(options(previous), add = TRUE)
  output <- tempfile(fileext = ".pdf")
  on.exit(unlink(output, force = TRUE), add = TRUE)
  state <- new.env(parent = emptyenv())
  captured <- testthat::with_mocked_bindings(
    capture_report_conditions(.convert_html_to_pdf("input.html", output)),
    chrome_print = function(input, output, timeout, verbose) {
      state$timeout <- timeout
      state$verbose <- verbose
      warning("original PDF warning")
      writeLines("PDF conversion stub", output)
    },
    .package = "pagedown"
  )
  expect_identical(state$timeout, 60)
  expect_identical(state$verbose, 0)
  expect_true(file.exists(captured$value))
  expect_length(captured$messages, 0L)
  expect_identical(captured$warnings, "original PDF warning")
  testthat::with_mocked_bindings(
    expect_error(.convert_html_to_pdf("input.html", output),
                 "Original error: original PDF error", fixed = TRUE),
    chrome_print = function(...) stop("original PDF error"),
    .package = "pagedown"
  )
})

test_that("opening a completed report is quiet by default", {
  state <- new.env(parent = emptyenv())
  previous <- options(
    camtrapReport.progress = NULL,
    viewer = function(path) {
      state$opened <- path
    }
  )
  on.exit(options(previous), add = TRUE)
  captured <- capture_report_conditions(
    .open_generated_report("completed.html", "html")
  )
  expect_identical(state$opened, "completed.html")
  expect_identical(captured$value, "completed.html")
  expect_length(captured$messages, 0L)
})

test_that("report and status are quiet for both formats and view settings", {
  skip_if_not(rmarkdown::pandoc_available())
  skip_if_not_installed("pagedown")
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  introduction <- cm$reportObjectElements$Modules$introduction
  cm$reportObjects <- list(introduction = introduction)
  cm$statusReportObjects <- list(introduction = introduction)
  state <- new.env(parent = emptyenv())
  state$opened <- character()
  open_file <- function(path) {
    state$opened <- c(state$opened, path)
  }
  previous <- options(camtrapReport.progress = NULL, viewer = open_file)
  on.exit(options(previous), add = TRUE)
  testthat::local_mocked_bindings(
    browseURL = open_file,
    .package = "utils"
  )
  testthat::local_mocked_bindings(
    chrome_print = function(input, output, timeout, verbose) {
      expect_identical(verbose, 0)
      expect_identical(timeout, 60)
      expect_true(file.exists(input))
      writeLines("PDF conversion stub", output)
    },
    .package = "pagedown"
  )
  root <- tempfile("camtrapReport-quiet-")
  dir.create(root)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  for (type in c("report", "status")) {
    generate <- if (identical(type, "report")) report else status
    for (format in c("html", "pdf")) {
      for (view in c(FALSE, TRUE)) {
        output <- file.path(root, paste0(type, "-", view, ".", format))
        console <- capture.output({
          captured <- capture_report_conditions(
            generate(cm, filename = output, view = view, profile = NULL)
          )
        })
        expect_identical(captured$value, output)
        expect_true(file.exists(output))
        expect_length(console, 0L)
        expect_length(captured$messages, 0L)
        expect_length(captured$warnings, 0L)
      }
    }
  }
  expect_length(state$opened, 4L)
})
