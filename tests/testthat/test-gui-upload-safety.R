gui_upload_test_helpers <- function() {
  app <- .camtrapReport_gui_app(NULL)
  env <- environment(app$serverFuncSource())
  list(app = app, env = env,
       copy_zip = get(".copy_upload", envir = env),
       copy_study = get(".copy_study_area_upload", envir = env),
       read_habitat = get(".read_habitat", envir = env))
}

test_that("uploaded filenames and extensions are checked on the server", {
  source <- tempfile()
  writeLines("Uploaded temporary content", source)
  on.exit(unlink(source), add = TRUE)
  unsafe <- c("../data.zip", "/data.zip", "folder/data.zip",
              "folder\\data.zip", "C:data.zip", "bad\nname.zip", "", ".", "..",
              "CON.zip", "lpt1.zip", "data.zip.", "data.zip ")
  for (name in unsafe) {
    upload <- data.frame(name = name, datapath = source)
    expect_error(.validate_gui_upload(upload, "zip"), "safe basenames",
                 fixed = TRUE)
  }
  upload <- data.frame(name = NA_character_, datapath = source)
  expect_error(.validate_gui_upload(upload, "zip"), "safe basenames",
               fixed = TRUE)
  upload$name <- "data.csv"
  expect_error(.validate_gui_upload(upload, "zip"), "Unsupported upload",
               fixed = TRUE)
  upload$name <- "data.ZIP"
  expect_identical(.validate_gui_upload(upload, "zip"), upload)
  upload$datapath <- tempfile()
  expect_error(.validate_gui_upload(upload, "zip"), "temporary files are missing",
               fixed = TRUE)
  upload$datapath <- tempdir()
  expect_error(.validate_gui_upload(upload, "zip"), "temporary files are missing",
               fixed = TRUE)
  upload <- data.frame(name = c("shape.shp", "SHAPE.SHP"), datapath = source)
  expect_error(.validate_gui_upload(upload, "shp", multiple = TRUE),
               "filenames must be unique", fixed = TRUE)
  expect_error(.validate_gui_upload(upload, "shp"), "expected number",
               fixed = TRUE)
})

test_that("ZIP upload copies are unique and copy failures leave no output", {
  helpers <- gui_upload_test_helpers()
  root <- tempfile("camtrapReport-upload-copy-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  source <- file.path(root, "source")
  writeLines("First upload", source)
  upload <- data.frame(name = "data.zip", datapath = source)
  first <- helpers$copy_zip(upload, root)
  writeLines("Second upload", source)
  second <- helpers$copy_zip(upload, root)
  expect_false(identical(first, second))
  expect_identical(basename(first), "data.zip")
  expect_identical(readLines(first), "First upload")
  expect_identical(readLines(second), "Second upload")
  expect_true(startsWith(
    normalizePath(first, winslash = "/", mustWork = TRUE),
    paste0(normalizePath(root, winslash = "/", mustWork = TRUE), "/")
  ))
  before <- list.files(root, all.files = TRUE)
  assign("file.copy", function(...) FALSE, envir = helpers$env)
  expect_error(helpers$copy_zip(upload, root), "Could not copy", fixed = TRUE)
  expect_identical(list.files(root, all.files = TRUE), before)
})

test_that("habitat uploads require CSV names and preserve valid CSV content", {
  helpers <- gui_upload_test_helpers()
  source <- tempfile()
  on.exit(unlink(source), add = TRUE)
  writeLines(c("locationID,Habitat_Type", "L1,forest"), source)
  upload <- data.frame(name = "habitat.CSV", datapath = source)
  actual <- helpers$read_habitat(upload = upload)
  expect_identical(actual$locationID, "L1")
  expect_identical(actual$Habitat_Type, "forest")
  upload$name <- "habitat.exe"
  expect_error(helpers$read_habitat(upload = upload), "Unsupported upload",
               fixed = TRUE)
})

test_that("study-area uploads reject ambiguous or mismatched shapefile parts", {
  helpers <- gui_upload_test_helpers()
  root <- tempfile("camtrapReport-shape-upload-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  source <- file.path(root, "source")
  writeLines("Temporary shapefile bytes", source)
  for (names in list("shape.prj", c("one.shp", "two.shp"),
                     c("one.shp", "other.dbf"))) {
    upload <- data.frame(name = names, datapath = source)
    expect_error(helpers$copy_study(upload, root), "one .shp file",
                 fixed = TRUE)
  }
  upload <- data.frame(name = c("shape.shp", "shape.shx"), datapath = source)
  before <- list.files(root, all.files = TRUE)
  assign("file.copy", function(...) FALSE, envir = helpers$env)
  expect_error(helpers$copy_study(upload, root), "Could not copy", fixed = TRUE)
  expect_identical(list.files(root, all.files = TRUE), before)
})

test_that("valid study-area uploads preserve their geometry", {
  helpers <- gui_upload_test_helpers()
  root <- tempfile("camtrapReport-valid-shape-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  source <- terra::vect("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
                         crs = "EPSG:4326")
  terra::writeVector(source, file.path(root, "boundary.shp"))
  paths <- list.files(root, full.names = TRUE)
  upload <- data.frame(name = basename(paths), datapath = paths)
  actual <- helpers$copy_study(upload, root)
  expect_s4_class(actual, "SpatVector")
  expect_identical(as.vector(terra::ext(actual)), as.vector(terra::ext(source)))
  expect_identical(nrow(actual), nrow(source))
})

test_that("invalid GUI optional uploads stop import and show a notification", {
  helpers <- gui_upload_test_helpers()
  zip_path <- tempfile(fileext = ".zip")
  source <- tempfile()
  writeLines("ZIP placeholder; import must not start", zip_path)
  writeLines("Upload placeholder", source)
  on.exit(unlink(c(zip_path, source)), add = TRUE)
  state <- new.env(parent = emptyenv())
  state$imports <- 0L
  state$notifications <- character()
  local_mocked_bindings(
    camData = function(...) {
      state$imports <- state$imports + 1L
      stop("Invalid uploads must not reach camData")
    },
    .package = "camtrapReport"
  )
  local_mocked_bindings(
    showNotification = function(ui, ...) {
      state$notifications <- c(state$notifications, ui)
    },
    .package = "shiny"
  )
  shiny::testServer(helpers$app$serverFuncSource(), {
    session$setInputs(
      data_zip_path = zip_path, habitat_csv_path = "",
      study_area_files = data.frame(name = "boundary.exe", datapath = source),
      load_cm = 1
    )
    session$flushReact()
    expect_identical(state$imports, 0L)
    expect_true(any(grepl("Unsupported upload", state$notifications,
                          fixed = TRUE)))
    state$notifications <- character()
    session$setInputs(
      study_area_files = NULL,
      habitat_csv = data.frame(name = "habitat.exe", datapath = source),
      load_cm = 2
    )
    session$flushReact()
    expect_identical(state$imports, 0L)
    expect_true(any(grepl("Unsupported upload", state$notifications,
                          fixed = TRUE)))
  })
})

test_that("GUI ZIP extraction overwrite is explicit and separate from outputs", {
  helpers <- gui_upload_test_helpers()
  root <- tempfile("camtrapReport-gui-zip-routing-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  zip_path <- file.path(root, "dataset.zip")
  writeLines("Import placeholder", zip_path)
  state <- new.env(parent = emptyenv())
  state$overwrites <- logical()
  state$paths <- character()
  local_mocked_bindings(
    camData = function(data, habitat, overwrite, ...) {
      state$overwrites <- c(state$overwrites, overwrite)
      state$paths <- c(state$paths, data)
      NULL
    },
    .package = "camtrapReport"
  )
  shiny::testServer(helpers$app$serverFuncSource(), {
    session$setInputs(data_zip_path = zip_path, habitat_csv_path = "",
                      overwrite_outputs = TRUE, load_cm = 1)
    session$flushReact()
    expect_identical(state$overwrites, FALSE)
    session$setInputs(overwrite_dataset = TRUE, load_cm = 2)
    session$flushReact()
    expect_identical(state$overwrites, c(FALSE, TRUE))
    session$setInputs(data_zip_path = root, load_cm = 3)
    session$flushReact()
    expect_identical(state$overwrites, c(FALSE, TRUE, FALSE))
    session$setInputs(
      data_zip_path = "", overwrite_dataset = FALSE,
      data_zip = data.frame(name = "upload.ZIP", datapath = zip_path),
      load_cm = 4
    )
    session$flushReact()
    expect_identical(state$overwrites, c(FALSE, TRUE, FALSE, FALSE))
    expect_identical(basename(state$paths[4]), "upload.ZIP")
    expect_false(identical(state$paths[4], zip_path))
  })
})
