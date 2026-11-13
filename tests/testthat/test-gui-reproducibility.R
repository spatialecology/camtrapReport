workflow_test_object <- function() {
  cm <- camR$new()
  cm$reportObjects <- list(reportSection("snapshot", txt = "Snapshot test"))
  cm$statusReportObjects <- list()
  cm$info$rem_analysis_signature <- .rem_analysis_signature()
  cm
}

test_that("exports preserve state and never replace existing bundles", {
  root <- tempfile("workflow-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  cm <- workflow_test_object()
  cm$title <- "Configured title"
  cm$filterCount <- 7
  cm$setting$rem_seed <- 123L
  cm$setting$custom_setting <- "preserved"
  cm$reportObjects[[1]]@txt <- "Edited narrative"
  before <- list(data = cm$data, info = cm$info, settings = cm$setting,
                 modules = cm$reportObjects)
  wd <- getwd()
  set.seed(421)
  rng <- .gui_rng_state()
  bundle <- .export_gui_workflow(cm, root, list(report = rng))
  expect_identical(getwd(), wd)
  expect_identical(list(data = cm$data, info = cm$info, settings = cm$setting,
                        modules = cm$reportObjects), before)
  saved <- .read_camreport_cache(file.path(bundle, "camReport_object.rds"))
  expect_identical(saved$reportObjects, cm$reportObjects)
  expect_identical(saved$setting, cm$setting)
  expect_identical(saved$filterCount, 7)
  expect_identical(saved$title, cm$title)
  manifest <- readRDS(file.path(bundle, "manifest.rds"))
  expect_identical(manifest$rng$report, rng)
  expect_identical(manifest$reports, "report")
  expect_true(all(file.exists(file.path(bundle, c("reproduce.R", "reproduce.Rmd",
    "README.md", "sessionInfo.txt", "manifest.rds", "camReport_object.rds")))))
  old <- tools::md5sum(list.files(bundle, full.names = TRUE))
  second <- .export_gui_workflow(cm, root)
  expect_identical(second, paste0(bundle, "/export-001"))
  expect_identical(tools::md5sum(names(old)), old)
  expect_false(dir.exists(file.path(bundle, "reports")))
})

test_that("required assets are copied and missing assets fail before export", {
  root <- tempfile("workflow-assets-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  cm <- workflow_test_object()
  cm$logoPath <- file.path(root, "missing.png")
  expect_error(.export_gui_workflow(cm, root), "logo is unavailable")
  expect_false(dir.exists(file.path(root, "GUI")))
  writeBin(as.raw(1:10), cm$logoPath)
  cm$study_area <- list(object = terra::vect(
    data.frame(x = 5, y = 52), geom = c("x", "y"), crs = "EPSG:4326"),
    path = "original/missing/boundary.gpkg")
  bundle <- .export_gui_workflow(cm, root)
  saved <- .read_camreport_cache(file.path(bundle, "camReport_object.rds"))
  expect_s4_class(saved$study_area$object, "SpatVector")
  expect_null(saved$study_area$path)
  expect_identical(saved$logoPath, "assets/logo.png")
  expect_identical(unname(tools::md5sum(file.path(bundle, saved$logoPath))),
                   unname(tools::md5sum(cm$logoPath)))
  cm$reportObjects <- list(reportSection("appendix"))
  cm$data <- list(media = data.frame(favourite = TRUE, fileName = "fox.jpg"))
  cm$info$directory <- root
  expect_error(.export_gui_workflow(cm, root), "Required favourite-image cache")
  dir.create(file.path(root, "_camtrap_pictures"))
  writeBin(as.raw(1:5), file.path(root, "_camtrap_pictures/fox.jpg"))
  images <- .gui_export_assets(cm)
  expect_true("_camtrap_pictures/fox.jpg" %in% names(images))
})

test_that("REM snapshot caches retain their scope and avoid independent fitting", {
  root <- tempfile("workflow-rem-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  cm <- rem_cache_fixture()
  cm$species_summary <- list(large_mammals = list(
    site_list = data.frame(scientificName = "Vulpes vulpes")))
  state <- new.env()
  local_rem_estimators(state)
  suppressMessages(cm$prepare_REM())
  cm$reportObjects <- list(reportSection("cached_rem"))
  cm$statusReportObjects <- list()
  expected <- cm$get_REM("Vulpes vulpes", "annual", 999L, 42L, fit = FALSE)
  fits <- length(state$fits)
  bundle <- .export_gui_workflow(cm, root)
  saved <- .read_camreport_cache(file.path(bundle, "camReport_object.rds"))
  expect_identical(saved$rem, cm$rem)
  expect_identical(saved$.rem_params, cm$.rem_params)
  expect_identical(saved$.act_models, cm$.act_models)
  expect_identical(saved$get_REM("Vulpes vulpes", "annual", 999L, 42L, fit = FALSE), expected)
  expect_identical(length(state$fits), fits)
  # This verifies cached-result regeneration, not fitting a real ecological model.
})

test_that("missing declared dependencies stop export with an actionable error", {
  root <- tempfile("workflow-packages-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  cm <- workflow_test_object()
  cm$reportObjects[[1]]@Rchunk <- .make_rchunk_from_text("1")
  cm$reportObjects[[1]]@Rchunk@packages <- "camtrapReportMissingTestPackage"
  expect_error(.export_gui_workflow(cm, root), "Install required package.*camtrapReportMissingTestPackage")
  expect_false(dir.exists(file.path(root, "GUI")))
})


test_that("export does not require favourite images excluded by the focus group", {
  cm <- workflow_test_object()
  cm$reportObjects <- list(reportSection("appendix"))
  cm$setting$focus_groups <- "fox"
  cm$species_summary <- list(fox = list(site_list = data.frame(scientificName = "Vulpes vulpes")))
  cm$data <- list(media = data.frame(favourite = TRUE, sequenceID = "s", fileName = "missing.jpg"),
                  observations = data.frame(sequenceID = "s", taxonID = 1L),
                  taxonomy = data.frame(taxonID = 1L, scientificName = "Excluded species"))
  expect_identical(.gui_export_assets(cm), character())
})

test_that("capturing an initialized RNG leaves its sequence unchanged", {
  set.seed(42)
  before <- .Random.seed
  state <- .gui_rng_state()
  expect_identical(state$seed, before)
  expect_identical(.Random.seed, before)
})
