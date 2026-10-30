test_that("GUI GBIF enrichment requires an explicitly checked control", {
  app <- .camtrapReport_gui_app(NULL)
  server <- app$serverFuncSource()
  env <- environment(server)
  root <- tempfile("camtrapReport-gui-consent-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  zip_path <- file.path(root, "dataset.zip")
  writeLines("Import placeholder", zip_path)
  state <- new.env(parent = emptyenv())
  state$consent <- logical()
  state$study_area <- logical()
  local_mocked_bindings(
    camData = function(data, habitat, enrich_taxonomy, ...) {
      state$consent <- c(state$consent, enrich_taxonomy)
      state$study_area <- c(state$study_area, "study_area" %in% names(list(...)))
      NULL
    },
    .package = "camtrapReport"
  )
  assign(".copy_study_area_upload", function(upload, ...) {
    if (is.null(upload)) NULL else "mocked boundary"
  }, envir = env)

  shiny::testServer(server, {
    session$setInputs(data_zip_path = zip_path, habitat_csv_path = "",
                      load_cm = 1)
    session$flushReact()
    expect_identical(state$consent, FALSE)
    session$setInputs(enrich_taxonomy = TRUE, load_cm = 2)
    session$flushReact()
    expect_identical(state$consent, c(FALSE, TRUE))
    session$setInputs(enrich_taxonomy = FALSE,
                      study_area_files = "mock upload", load_cm = 3)
    session$flushReact()
    expect_identical(state$consent, c(FALSE, TRUE, FALSE))
    session$setInputs(enrich_taxonomy = TRUE, load_cm = 4)
    session$flushReact()
    expect_identical(state$consent, c(FALSE, TRUE, FALSE, TRUE))
    expect_identical(state$study_area, c(FALSE, FALSE, TRUE, TRUE))
  })

  ui <- paste(as.character(get("ui", envir = env, inherits = FALSE)),
              collapse = "")
  expect_match(ui, 'id="enrich_taxonomy"', fixed = TRUE)
  checkbox <- regmatches(ui, regexpr('<input[^>]*id="enrich_taxonomy"[^>]*>', ui))
  expect_false(grepl("checked", checkbox, fixed = TRUE))
})
