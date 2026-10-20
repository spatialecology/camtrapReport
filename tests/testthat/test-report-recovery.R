recovery_module_names <- function(modules) {
  vapply(
    .flatten_attached_sections(modules),
    function(module) module@name,
    character(1),
    USE.NAMES = FALSE
  )
}

recovery_state <- function() {
  state <- new.env(parent = emptyenv())
  state$renders <- 0L
  state$tested <- character()
  state$rendered <- list()
  state$toc <- logical()
  state
}

recovery_renderer <- function(state) {
  function(object, output_file, rmd_file, toc) {
    state$renders <- state$renders + 1L
    state$toc <- c(state$toc, toc)
    if (state$renders == 1L) stop("Simulated module dependency failure")
    state$rendered <- .flatten_attached_sections(object$reportObjects)
    output_file
  }
}

test_that("report recovery keeps only the attached default selection", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  original <- recovery_module_names(cm$reportObjects)
  cm$reportObjectElements$Modules_info$tested <- NA
  state <- recovery_state()

  testthat::with_mocked_bindings(
    suppressMessages(report(cm, filename = tempfile(), test = TRUE)),
    .generate_report = recovery_renderer(state),
    .QuickTestReportSection = function(x, object, path) {
      state$tested <- c(state$tested, x@name)
      TRUE
    },
    .package = "camtrapReport"
  )

  expect_identical(state$tested, original)
  expect_identical(recovery_module_names(state$rendered), original)
  expect_false(any(grepl("_eow$", recovery_module_names(state$rendered))))
  expect_identical(recovery_module_names(cm$reportObjects), original)
})

test_that("report recovery preserves custom attached module ordering", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  cm$reportObjects <- list()
  selected <- c("methods", "study_area", "introduction")
  for (name in selected) {
    cm$addReportObject(cm$reportObjectElements$Modules[[name]])
  }
  cm$reportObjectElements$Modules_info$tested <- NA
  state <- recovery_state()

  testthat::with_mocked_bindings(
    suppressMessages(report(cm, filename = tempfile(), test = TRUE)),
    .generate_report = recovery_renderer(state),
    .QuickTestReportSection = function(x, object, path) {
      state$tested <- c(state$tested, x@name)
      TRUE
    },
    .package = "camtrapReport"
  )

  expect_identical(state$tested, selected)
  expect_identical(recovery_module_names(state$rendered), selected)
  expect_identical(recovery_module_names(cm$reportObjects), selected)
})

test_that("recovery retains attached EOW status modules and skips orphans", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  suppressMessages(sections(
    cm,
    n = sections(cm, profile = "EOW"),
    profile = "EOW"
  ))
  original <- recovery_module_names(cm$reportObjects)
  cm$reportObjectElements$Modules_info$tested <- NA
  cm$reportObjectElements$Status_modules_info$tested <- NA
  state <- recovery_state()

  testthat::with_mocked_bindings(
    suppressMessages(report(cm, filename = tempfile(), test = TRUE)),
    .generate_report = recovery_renderer(state),
    .QuickTestReportSection = function(x, object, path) {
      state$tested <- c(state$tested, x@name)
      !identical(x@name, "results")
    },
    .package = "camtrapReport"
  )

  expected <- c(
    "introduction", "methods", "study_area", "sampling", "location_eow",
    "effort_eow", "image_processing", "data_processing", "acknowledgements",
    "References_eow", "appendix_eow", "abstract", "spatial", "temporal",
    "availability"
  )
  expect_identical(recovery_module_names(state$rendered), expected)
  expect_false(any(c("population_density_annual", "model_parameters_eow") %in%
                     state$tested))
  spatial <- state$rendered[[which(expected == "spatial")]]
  expect_identical(spatial@parent, "appendix_eow")
  info <- cm$reportObjectElements$Status_modules_info
  expect_true(all(info$tested[info$name %in%
                               c("abstract", "spatial", "temporal",
                                 "availability")]))
  expect_identical(recovery_module_names(cm$reportObjects), original)
})

test_that("explicit-profile recovery removes descendants of failed parents", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  state <- recovery_state()

  testthat::with_mocked_bindings(
    suppressMessages(report(
      cm, filename = tempfile(), test = TRUE, profile = "EOW"
    )),
    .generate_report = recovery_renderer(state),
    .QuickTestReportSection = function(x, object, path) {
      !identical(x@name, "results")
    },
    .package = "camtrapReport"
  )

  rendered <- recovery_module_names(state$rendered)
  expect_false(any(c("results", "population_density_annual",
                     "model_parameters_eow", "population_densities_eow") %in%
                     rendered))
  expect_true(all(c("introduction", "methods", "appendix_eow", "spatial") %in%
                    rendered))
})

test_that("PDF recovery tests only modules compatible with PDF", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  original <- recovery_module_names(cm$reportObjects)
  cm$reportObjectElements$Modules_info$tested <- NA
  state <- recovery_state()

  testthat::with_mocked_bindings(
    suppressMessages(report(
      cm, filename = tempfile(fileext = ".pdf"), test = TRUE
    )),
    .generate_report = recovery_renderer(state),
    .QuickTestReportSection = function(x, object, path) {
      state$tested <- c(state$tested, x@name)
      TRUE
    },
    .convert_html_to_pdf = function(html_file, pdf_file) pdf_file,
    .package = "camtrapReport"
  )

  expect_identical(state$tested, setdiff(original, "appendix"))
  expect_identical(recovery_module_names(state$rendered), state$tested)
  expect_identical(state$toc, c(FALSE, FALSE))
  expect_identical(recovery_module_names(cm$reportObjects), original)
})

test_that("recovery reuses tests of unchanged pool modules", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  suppressMessages(sections(cm, c("introduction", "methods", "study_area")))
  original <- recovery_module_names(cm$reportObjects)
  info <- cm$reportObjectElements$Modules_info
  info$tested <- NA
  info$tested[info$name == "introduction"] <- TRUE
  info$tested[info$name == "methods"] <- FALSE
  cm$reportObjectElements$Modules_info <- info
  state <- recovery_state()

  testthat::with_mocked_bindings(
    suppressMessages(report(cm, filename = tempfile(), test = TRUE)),
    .generate_report = recovery_renderer(state),
    .QuickTestReportSection = function(x, object, path) {
      state$tested <- c(state$tested, x@name)
      TRUE
    },
    .package = "camtrapReport"
  )

  expect_identical(state$tested, character())
  expect_identical(recovery_module_names(state$rendered), "introduction")
  expect_identical(recovery_module_names(cm$reportObjects), original)
})

test_that("recovery tests edited and custom modules without altering the pool", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  edited <- cm$reportObjectElements$Modules$introduction
  edited@txt <- "User-edited introduction."
  cm$reportObjects <- list(
    introduction = edited,
    custom = reportSection(name = "custom", title = "Custom")
  )
  info <- cm$reportObjectElements$Modules_info
  info$tested[info$name == "introduction"] <- TRUE
  cm$reportObjectElements$Modules_info <- info
  state <- recovery_state()

  testthat::with_mocked_bindings(
    suppressMessages(report(cm, filename = tempfile(), test = TRUE)),
    .generate_report = recovery_renderer(state),
    .QuickTestReportSection = function(x, object, path) {
      state$tested <- c(state$tested, x@name)
      !identical(x@name, "introduction")
    },
    .package = "camtrapReport"
  )

  expect_identical(state$tested, c("introduction", "custom"))
  expect_identical(recovery_module_names(state$rendered), "custom")
  info <- cm$reportObjectElements$Modules_info
  expect_true(info$tested[info$name == "introduction"])
  expect_identical(cm$reportObjects$introduction@txt,
                   "User-edited introduction.")
  expect_false(identical(cm$reportObjectElements$Modules$introduction@txt,
                         "User-edited introduction."))
})

test_that("recovery stops cleanly when all selected roots fail", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  suppressMessages(sections(cm, c("methods", "study_area")))
  original <- recovery_module_names(cm$reportObjects)
  cm$reportObjectElements$Modules_info$tested <- NA
  state <- recovery_state()

  testthat::with_mocked_bindings(
    expect_error(
      suppressMessages(report(cm, filename = tempfile(), test = TRUE)),
      "No ecological report modules passed module testing.",
      fixed = TRUE
    ),
    .generate_report = recovery_renderer(state),
    .QuickTestReportSection = function(x, object, path) {
      state$tested <- c(state$tested, x@name)
      FALSE
    },
    .package = "camtrapReport"
  )

  expect_identical(state$tested, "methods")
  expect_identical(state$renders, 1L)
  expect_identical(recovery_module_names(cm$reportObjects), original)
})
