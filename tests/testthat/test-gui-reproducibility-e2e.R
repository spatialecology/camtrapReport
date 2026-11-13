# Actual Shiny handlers and installed-package child processes; no renderer mocks.
test_that("GUI workflows replay after relocation in fresh R and R Markdown processes", {
  skip_on_cran()
  for (package in c("xml2", "gt", "dygraphs", "xts", "htmlwidgets")) {
    skip_if_not_installed(package)
  }
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc is needed for workflow execution")
  artifact_parent <- Sys.getenv("CAMTRAPREPORT_EXPORT_TEST_ARTIFACTS")
  root <- if (nzchar(artifact_parent)) tempfile("gui-e2e-", tmpdir = artifact_parent) else tempfile("gui-e2e-")
  dir.create(root)
  if (!nzchar(artifact_parent)) on.exit(unlink(root, recursive = TRUE), add = TRUE)
  root <- normalizePath(root, winslash = "/")
  message("GUI workflow test artifacts: ", root)
  input <- file.path(root, "original-input")
  dir.create(input)
  stopifnot(all(file.copy(list.files(system.file("external/example-small", package = "camtrapReport"),
                                   full.names = TRUE), input)))
  cm <- suppressMessages(camData(input, enrich_taxonomy = FALSE))
  sections(cm, n = "abstract", report_type = "status")
  add_profile(cm, reportProfile("export_test", report = c("methods", "sampling", "effort"),
                                 status = "abstract"))
  probe <- reportSection("rng_probe", title = "Stochastic replay probe",
    code_setting = "echo=FALSE, results='asis'", code = {
      cat(sprintf("RNG_MARKER %.17g", stats::runif(1L)))
    })
  cm$reportObjects$rng_probe <- probe
  gui_dir <- file.path(root, "original-output")
  dir.create(gui_dir)
  local_mocked_bindings(.open_generated_report = function(...) invisible(NULL),
                        .package = "camtrapReport")
  state <- list2env(list(notifications = character()), parent = emptyenv())
  local_mocked_bindings(showNotification = function(ui, ...) {
    state$notifications <- c(state$notifications, as.character(ui))
  }, .package = "shiny")
  wd <- getwd()
  app <- .camtrapReport_gui_app(cm)
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(output_dir = gui_dir, meta_title = "Portable GUI report",
      meta_siteName = "Portable example", meta_authors = "GUI test author",
      meta_description = "Snapshot test", filter_count = 5,
      years_selected = as.character(cm$years),
      sections_keep = c("methods", "sampling", "effort", "rng_probe"), apply_sections = 1)
    session$setInputs(section_to_edit = "effort", section_editor_name = "effort")
    session$setInputs(section_new_text = "Edited effort narrative for {siteName}.", apply_section_text = 1)
    session$setInputs(generate_report = 1)
    session$setInputs(generate_status = 1)
    session$setInputs(export_workflow = 1)
    session$flushReact()
  })
  expect_false(any(grepl("Could not", state$notifications)), info = paste(state$notifications, collapse = "\n"))
  expect_identical(getwd(), wd)
  bundle <- file.path(gui_dir, "GUI", "reproducibility")
  original <- file.path(gui_dir, "GUI", "reports")
  expect_true(all(file.exists(file.path(bundle, c("reproduce.R", "reproduce.Rmd")))))
  expect_true(all(file.exists(file.path(original, c("ecological_report.html", "data_status.html")))))
  expect_false(file.exists(file.path(gui_dir, "report.html")))
  moved_parent <- file.path(root, "relocated")
  dir.create(moved_parent)
  stopifnot(file.copy(bundle, moved_parent, recursive = TRUE))
  moved <- file.path(moved_parent, "reproducibility")
  stopifnot(file.rename(input, paste0(input, "-unavailable")))
  stopifnot(file.rename(bundle, paste0(bundle, "-unavailable")))
  saved <- .read_camreport_cache(file.path(moved, "camReport_object.rds"))
  expect_identical(saved$title, "Portable GUI report")
  expect_identical(saved$siteName, "Portable example")
  expect_identical(saved$filterCount, 5)
  expect_identical(saved$years, cm$years)
  expect_identical(saved$camera_setup, cm$camera_setup)
  expect_identical(saved$camera_stats, cm$camera_stats)
  expect_identical(saved$reportObjects, cm$reportObjects)
  expect_identical(saved$statusReportObjects, cm$statusReportObjects)
  expect_identical(saved$reportObjectElements$Profiles, cm$reportObjectElements$Profiles)
  expect_identical(saved$group_definition, cm$group_definition)
  expect_identical(saved$setting, cm$setting)
  expect_identical(saved$info$directory, "assets")
  original_libs <- Sys.getenv("R_LIBS", unset = NA_character_)
  on.exit(if (is.na(original_libs)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = original_libs), add = TRUE)
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  run_child <- function(script, args = character(), name, success = TRUE) {
    log <- file.path(root, paste0(name, ".log"))
    code <- system2(file.path(R.home("bin"), "Rscript"),
                    c("--vanilla", shQuote(script), shQuote(args)),
                    stdout = log, stderr = log, timeout = 180)
    contents <- paste(readLines(log, warn = FALSE), collapse = "\n")
    if (success) {
      expect_identical(code, 0L, info = contents)
      if (code != 0L) stop("Fresh-process test failed: ", contents)
    } else expect_true(code != 0L)
    contents
  }
  run_child(file.path(moved, "reproduce.R"), name = "reproduce-r")
  script_output <- file.path(moved, "reports")
  rmd_driver <- file.path(root, "render-driver.R")
  writeLines('rmarkdown::render(commandArgs(TRUE)[1L], quiet = TRUE)', rmd_driver)
  run_child(rmd_driver, file.path(moved, "reproduce.Rmd"), "reproduce-rmd")
  rmd_output <- file.path(script_output, "run-001")
  expect_true(file.exists(file.path(moved, "reproduce.html")))
  html_signature <- function(path) {
    doc <- xml2::read_html(path)
    inner <- xml2::read_html(xml2::xml_attr(xml2::xml_find_first(doc, ".//iframe[@srcdoc]"), "srcdoc"))
    json <- xml2::xml_text(xml2::xml_find_first(inner, './/script[@type="application/json"]'))
    text <- xml2::xml_text(doc)
    list(tables = xml2::xml_text(xml2::xml_find_all(doc, ".//table")),
         figure = jsonlite::fromJSON(json, simplifyVector = FALSE)$x,
         rng = regmatches(text, regexpr("RNG_MARKER [0-9.e+-]+", text)))
  }
  signature <- html_signature(file.path(original, "ecological_report.html"))
  expect_gt(length(signature$tables), 0L)
  expect_true(nzchar(signature$rng))
  for (directory in c(script_output, rmd_output)) {
    expect_identical(html_signature(file.path(directory, "ecological_report.html")), signature)
    for (stem in c("ecological_report", "data_status")) {
      expect_true(file.exists(file.path(directory, paste0(stem, ".html"))))
      expect_identical(readLines(file.path(directory, paste0(stem, ".Rmd"))),
                       readLines(file.path(original, paste0(stem, ".Rmd"))))
    }
  }
  hashes <- tools::md5sum(file.path(script_output, c("ecological_report.html", "data_status.html")))
  run_child(rmd_driver, file.path(moved, "reproduce.Rmd"), "reproduce-rmd-repeat")
  expect_identical(tools::md5sum(names(hashes)), hashes)
  expect_true(file.exists(file.path(script_output, "run-002/ecological_report.html")))
  # Missing-file/dependency failures occur before any report is generated.
  snapshot <- file.path(moved, "camReport_object.rds")
  stopifnot(file.rename(snapshot, paste0(snapshot, ".hidden")))
  missing <- run_child(file.path(moved, "reproduce.R"), name = "missing-object", success = FALSE)
  expect_match(missing, "Missing required bundle file.*camReport_object.rds")
  stopifnot(file.rename(paste0(snapshot, ".hidden"), snapshot))
  manifest_file <- file.path(moved, "manifest.rds")
  manifest <- readRDS(manifest_file)
  modified <- manifest
  modified$packages <- c(modified$packages, camtrapReportMissingTestPackage = "1")
  saveRDS(modified, manifest_file)
  missing <- run_child(file.path(moved, "reproduce.R"), name = "missing-package", success = FALSE)
  expect_match(missing, "Install required package.*camtrapReportMissingTestPackage")
  saveRDS(manifest, manifest_file)
  expect_identical(getwd(), wd)
})
