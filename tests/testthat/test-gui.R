test_that("the GUI app is built and exposes its read-only server outputs", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  app <- .camtrapReport_gui_app(cm)

  expect_s3_class(app, "shiny.appobj")
  expect_type(app$serverFuncSource(), "closure")

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()

    expect_gt(length(output$object_status), 0L)
    expect_gt(length(output$years_ui), 0L)
    expect_gt(length(output$existing_group_ui), 0L)
    expect_gt(length(output$group_values_ui), 0L)
    expect_gt(length(output$species_table), 0L)
    expect_gt(length(output$sections_ui), 0L)
    expect_gt(length(output$section_edit_select_ui), 0L)
    expect_match(output$section_list, "name")
    expect_match(output$cm_summary, cm$siteName, fixed = TRUE)
  })
})

test_that("the GUI saves edited settings without running report modules", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  app <- .camtrapReport_gui_app(cm)
  out_dir <- tempfile("camtrap-gui-output-")
  dir.create(out_dir)

  on.exit(
    unlink(out_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      meta_title = "GUI test title",
      meta_subtitle = "GUI subtitle",
      meta_authors = "Test author",
      meta_institute = "Test institute",
      meta_siteName = cm$siteName,
      meta_logoPath = "",
      meta_description = "GUI test description",
      meta_acknowledgement = "GUI test acknowledgement",
      years_selected = cm$years,
      filter_count = cm$filterCount,
      sections_keep = sections(cm),
      output_dir = out_dir,
      save_cm = 1
    )
    session$flushReact()
  })

  saved_file <- file.path(out_dir, "camtrapReport_gui_object.rds")
  saved <- readRDS(saved_file)

  expect_true(file.exists(saved_file))
  expect_s4_class(saved, "camReport")
  expect_identical(saved$title, "GUI test title")
  expect_identical(saved$description, "GUI test description")
})

test_that("the GUI can load an existing saved camReport directory", {
  cm <- camtrap_test_report()
  app <- .camtrapReport_gui_app(NULL)

  shiny::testServer(app$serverFuncSource(), {
    expect_true(any(grepl(
      "No object loaded",
      as.character(output$object_status),
      fixed = TRUE
    )))

    session$setInputs(
      data_zip_path = cm$info$directory,
      habitat_csv_path = "",
      load_cm = 1
    )
    session$flushReact()

    expect_true(any(grepl(
      "Object loaded",
      as.character(output$object_status),
      fixed = TRUE
    )))
  })
})

test_that("gui configures and launches Shiny while restoring global options", {
  cm <- camtrap_test_report()
  calls <- new.env(parent = emptyenv())
  old_limit <- getOption("shiny.maxRequestSize")

  local_mocked_bindings(
    .camtrapReport_gui_app = function(object) {
      calls$object <- object
      "mock-shiny-app"
    },
    .package = "camtrapReport"
  )
  local_mocked_bindings(
    runApp = function(app, launch.browser, ...) {
      calls$app <- app
      calls$launch.browser <- launch.browser
      calls$limit <- getOption("shiny.maxRequestSize")
      invisible(NULL)
    },
    .package = "shiny"
  )

  result <- gui(cm, launch.browser = FALSE, max_upload_mb = 1)

  expect_identical(result, cm)
  expect_identical(calls$object, cm)
  expect_identical(calls$app, "mock-shiny-app")
  expect_false(calls$launch.browser)
  expect_identical(calls$limit, 1024^2)
  expect_identical(getOption("shiny.maxRequestSize"), old_limit)
})

test_that("GUI reports use explicit paths and require overwrite opt-in", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  section <- reportSection(name = "gui_safety", title = "GUI output safety")
  cm$reportObjects <- list(gui_safety = section)
  cm$statusReportObjects <- list(gui_safety = section)
  assign("setup", function(...) {
    state$setups <- state$setups + 1L
    invisible(NULL)
  }, envir = cm)
  app <- .camtrapReport_gui_app(cm)
  root <- tempfile("camtrapReport-gui-safety-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  paths <- file.path(root, c("report.html", "report.Rmd",
                             "data_status.html", "data_status.Rmd"))
  for (path in paths) writeLines("Original GUI output", path)
  state <- new.env(parent = emptyenv())
  state$renders <- character()
  state$setups <- 0L
  state$opened <- character()
  state$notifications <- character()
  old_wd <- getwd()
  local_mocked_bindings(
    .render_report_with_progress = function(input, output_file, envir) {
      state$renders <- c(state$renders, output_file)
      writeLines("Updated GUI output", output_file)
      output_file
    },
    .open_generated_report = function(path, format) {
      state$opened <- c(state$opened, path)
    },
    .package = "camtrapReport"
  )
  local_mocked_bindings(
    showNotification = function(ui, ...) {
      state$notifications <- c(state$notifications, ui)
    },
    .package = "shiny"
  )

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(output_dir = root, overwrite_outputs = FALSE,
                       generate_report = 1, generate_status = 1)
    session$flushReact()
    expect_identical(state$renders, character())
    expect_identical(state$setups, 0L)
    expect_true(any(grepl("would be overwritten", state$notifications,
                          fixed = TRUE)))
    for (path in paths) {
      expect_identical(readLines(path), "Original GUI output")
    }
    expect_match(output$output_paths, file.path(root, "report.html"),
                 fixed = TRUE)
    expect_identical(getwd(), old_wd)

    session$setInputs(overwrite_outputs = TRUE,
                       generate_report = 2, generate_status = 2)
    session$flushReact()
    expect_setequal(state$renders, paths[c(1, 3)])
    expect_identical(state$setups, 2L)
    expect_setequal(state$opened, paths[c(1, 3)])
    expect_identical(getwd(), old_wd)
  })
})

test_that("GUI saving rejects missing directories and protects existing RDS", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  app <- .camtrapReport_gui_app(cm)
  root <- tempfile("camtrapReport-gui-rds-safety-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  saved_path <- file.path(root, "camtrapReport_gui_object.rds")
  saveRDS("Original GUI object", saved_path)
  state <- new.env(parent = emptyenv())
  state$notifications <- character()
  local_mocked_bindings(
    showNotification = function(ui, ...) {
      state$notifications <- c(state$notifications, ui)
    },
    .package = "shiny"
  )
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(output_dir = root, save_cm = 1)
    session$flushReact()
    expect_identical(readRDS(saved_path), "Original GUI object")
    expect_true(any(grepl("already exists", state$notifications, fixed = TRUE)))

    session$setInputs(output_dir = file.path(root, "missing"), save_cm = 2)
    session$flushReact()
    expect_false(dir.exists(file.path(root, "missing")))
    expect_true(any(grepl("existing output directory", state$notifications,
                          fixed = TRUE)))

    session$setInputs(output_dir = root, overwrite_outputs = TRUE, save_cm = 3)
    session$flushReact()
    expect_s4_class(readRDS(saved_path), "camReport")
  })
})

test_that("GUI output defaults use optional here only at the UI layer", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  root <- tempfile("camtrapReport-gui-project-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  app <- .camtrapReport_gui_app(NULL)
  app_env <- environment(app$serverFuncSource())
  default_output <- get(".default_output_dir", envir = app_env)
  assign("requireNamespace", function(package, quietly) {
    expect_identical(package, "here")
    expect_true(quietly)
    TRUE
  }, envir = app_env)
  assign(".eval", function(code) {
    expect_identical(code, "here::here()")
    root
  }, envir = app_env)
  expect_identical(default_output(cm), root)

  assign("requireNamespace", function(...) FALSE, envir = app_env)
  expect_identical(default_output(cm),
                   normalizePath(cm$info$directory, winslash = "/"))
  cm$info$directory <- NULL
  expect_identical(default_output(cm), "")
  expect_identical(default_output(NULL), "")
})

test_that("GUI metadata refresh preserves an explicit output directory", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  app <- .camtrapReport_gui_app(cm)
  default_output <- get(".default_output_dir",
                         envir = environment(app$serverFuncSource()))
  root <- tempfile("camtrapReport-gui-chosen-output-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  state <- new.env(parent = emptyenv())
  state$directories <- character()

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    session$sendInputMessage <- function(inputId, message) {
      if (identical(inputId, "output_dir")) {
        state$directories <- c(state$directories, message$value)
      }
    }
    session$setInputs(output_dir = root)
    refresh_inputs(cm)
    expect_identical(state$directories, character())

    session$setInputs(output_dir = " ")
    refresh_inputs(cm)
    expect_identical(state$directories, default_output(cm))

    session$setInputs(output_dir = file.path(root, "missing"))
    refresh_inputs(cm)
    expect_identical(state$directories, default_output(cm))
  })
})
