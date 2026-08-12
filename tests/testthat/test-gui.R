test_that("the GUI app is built and exposes its read-only server outputs", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  app <- camtrapReport:::.camtrapReport_gui_app(cm)

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
  app <- camtrapReport:::.camtrapReport_gui_app(cm)
  out_dir <- tempfile("camtrap-gui-output-")

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
  app <- camtrapReport:::.camtrapReport_gui_app(NULL)

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
