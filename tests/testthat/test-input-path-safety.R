camdp_test_manifest <- function(extra = character()) {
  paths <- c(.camdp_core_files(), extra)
  data.frame(Name = paths, Length = rep(100, length(paths)),
             stringsAsFactors = FALSE)
}

test_that("Camtrap DP resources use exact case-insensitive basenames", {
  paths <- file.path("nested", .camdp_core_files())
  expect_identical(.camdp_input_files(rev(paths)), paths)
  expect_identical(.camdp_input_files(toupper(paths)), toupper(paths))
  expect_identical(.camdp_input_files(c(
    "observations.csv.bak", "observationsXcsv", "old-observations.csv", paths
  )), paths)
  expect_error(.camdp_input_files(c(paths, "other/observations.csv")),
               "multiple files named observations.csv", fixed = TRUE)
  expect_error(.camdp_input_files(sub("observations.csv", "observationsXcsv",
                                      paths, fixed = TRUE)),
               "observations.csv is not available", fixed = TRUE)
})

test_that("ZIP inventories reject unsafe paths before extracting", {
  unsafe <- c("../outside.txt", "folder/../../outside.txt", "/absolute.txt",
              "C:/outside.txt", "C:outside.txt", "\\\\server\\outside.txt",
              "folder\\outside.txt", "folder/file:stream", "bad\nname.txt",
              "folder./outside.txt", "folder /outside.txt", "NUL",
              "folder/CON.txt", "COM1.log", "LPT9.txt")
  for (path in unsafe) {
    expect_error(.validate_camdp_zip_manifest(camdp_test_manifest(path)),
                 "unsafe file path", fixed = TRUE)
  }
  state <- new.env(parent = emptyenv())
  state$extractions <- 0L
  local_mocked_bindings(
    unzip = function(zipfile, list = FALSE, ...) {
      if (list) return(camdp_test_manifest("../outside.txt"))
      state$extractions <- state$extractions + 1L
    },
    .package = "utils"
  )
  expect_error(.read_camdp(tempfile(fileext = ".zip")),
               "unsafe file path", fixed = TRUE)
  expect_identical(state$extractions, 0L)
})

test_that("ZIP inventories reject duplicates and ambiguous core resources", {
  for (path in c("observations.csv", "./observations.csv", "OBSERVATIONS.CSV")) {
    expect_error(.validate_camdp_zip_manifest(camdp_test_manifest(path)),
                 "duplicate file or directory paths", fixed = TRUE)
  }
  expect_error(.validate_camdp_zip_manifest(
    camdp_test_manifest("nested/observations.csv")
  ), "multiple files named observations.csv", fixed = TRUE)
  expect_error(.validate_camdp_zip_manifest(
    camdp_test_manifest(c("folder", "folder/extra.txt"))
  ), "conflicting file and directory paths", fixed = TRUE)
  expect_error(.validate_camdp_zip_manifest(camdp_test_manifest("")),
               "unsafe file path", fixed = TRUE)
  for (size in c(NA_real_, -1, Inf)) {
    manifest <- camdp_test_manifest()
    manifest$Length[1] <- size
    expect_error(.validate_camdp_zip_manifest(manifest),
                 "invalid file inventory", fixed = TRUE)
  }
  expect_error(.validate_camdp_zip_manifest(data.frame()),
               "invalid file inventory", fixed = TRUE)
})

test_that("valid ZIP inventories retain nested resources and ancillary files", {
  manifest <- data.frame(
    Name = c("./", "data/", "data/observations.csv", "data/deployments.csv",
             "data/media.csv", "datapackage.json", "images/photo.jpg"),
    Length = c(0, 0, 10, 10, 10, 10, 10)
  )
  expect_identical(.validate_camdp_zip_manifest(manifest), manifest)
})

test_that("ZIP resource limits have safe adjustable defaults", {
  old_options <- options(camtrapReport.max_zip_entries = NULL,
                         camtrapReport.max_zip_bytes = NULL)
  on.exit(options(old_options), add = TRUE)
  too_many <- data.frame(Name = rep("unused", 100001L),
                         Length = rep(0, 100001L))
  expect_error(.validate_camdp_zip_manifest(too_many),
               "camtrapReport.max_zip_entries", fixed = TRUE)
  too_large <- camdp_test_manifest()
  too_large$Length <- c(20 * 1024^3 + 1, 0, 0, 0)
  expect_error(.validate_camdp_zip_manifest(too_large),
               "camtrapReport.max_zip_bytes", fixed = TRUE)

  options(camtrapReport.max_zip_entries = 3,
          camtrapReport.max_zip_bytes = 400)
  expect_error(.validate_camdp_zip_manifest(camdp_test_manifest()),
               "entry limit", fixed = TRUE)
  options(camtrapReport.max_zip_entries = 4,
          camtrapReport.max_zip_bytes = 399)
  expect_error(.validate_camdp_zip_manifest(camdp_test_manifest()),
               "uncompressed-size limit", fixed = TRUE)
  options(camtrapReport.max_zip_bytes = 400)
  expect_identical(.validate_camdp_zip_manifest(camdp_test_manifest()),
                    camdp_test_manifest())

  for (name in c("camtrapReport.max_zip_entries", "camtrapReport.max_zip_bytes")) {
    for (value in list(0, -1, Inf, NA_real_, "400", TRUE, c(1, 2))) {
      options(stats::setNames(list(value), name))
      expect_error(.validate_camdp_zip_manifest(camdp_test_manifest()),
                   "must be a positive finite number", fixed = TRUE)
      options(stats::setNames(list(400), name))
    }
  }
})

test_that("ZIP limits and existing-target guards run before extraction", {
  output <- tempfile("camtrapReport-zip-guard-")
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE, force = TRUE), add = TRUE)
  zip_file <- file.path(output, "survey.zip")
  target <- file.path(output, "survey")
  dir.create(target)
  sentinel <- file.path(target, "keep.txt")
  writeLines("Existing output", sentinel)
  state <- new.env(parent = emptyenv())
  state$extractions <- 0L
  local_mocked_bindings(
    unzip = function(zipfile, list = FALSE, ...) {
      if (list) return(camdp_test_manifest())
      state$extractions <- state$extractions + 1L
    },
    .package = "utils"
  )
  expect_error(.read_camdp(zip_file, path = output),
               "already exists", fixed = TRUE)
  expect_identical(readLines(sentinel), "Existing output")
  old_options <- options(camtrapReport.max_zip_entries = 3)
  on.exit(options(old_options), add = TRUE)
  expect_error(.read_camdp(zip_file, path = output, overwrite = TRUE),
               "entry limit", fixed = TRUE)
  expect_identical(state$extractions, 0L)
})

test_that("camData forwards the named-only overwrite flag without shifting API", {
  expect_identical(tail(names(formals(camData)), 2), c("...", "overwrite"))
  state <- new.env(parent = emptyenv())
  state$overwrites <- logical()
  original_read <- .read_camdp
  local_mocked_bindings(
    .read_camdp = function(file, enrich_taxonomy, overwrite) {
      state$overwrites <- c(state$overwrites, overwrite)
      stop("Import reached", call. = FALSE)
    }
  )
  zip_file <- tempfile(fileext = ".zip")
  expect_error(suppressMessages(camData(zip_file)), "Import reached")
  expect_error(suppressMessages(camData(zip_file, overwrite = TRUE)),
               "Import reached")
  expect_error(suppressMessages(camData(zip_file, NULL, NULL, FALSE, FALSE,
                                        "reserved", overwrite = TRUE)),
               "Import reached")
  expect_identical(state$overwrites, c(FALSE, TRUE, TRUE))
  for (value in list(NA, 1, logical(), c(TRUE, FALSE))) {
    expect_error(camData(zip_file, overwrite = value),
                 "'overwrite' must be TRUE or FALSE", fixed = TRUE)
    expect_error(original_read(zip_file, overwrite = value),
                 "'overwrite' must be TRUE or FALSE", fixed = TRUE)
  }
})

test_that("explicit ZIP overwrite cannot follow existing symbolic links", {
  skip_on_os("windows")
  root <- tempfile("camtrapReport-zip-links-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  outside <- file.path(root, "outside")
  target <- file.path(root, "target")
  dir.create(outside)
  skip_if(!file.symlink(outside, target), "Symbolic links are unavailable")
  expect_error(.check_camdp_zip_destination(target, .camdp_core_files(), TRUE),
               "not a file or symbolic link", fixed = TRUE)
  unlink(target)
  dir.create(target)
  expect_true(file.symlink(outside, file.path(target, "nested")))
  expect_error(.check_camdp_zip_destination(
    target, c(.camdp_core_files(), "nested/extra.txt"), TRUE
  ), "contains a symbolic-link path", fixed = TRUE)
  expect_identical(list.files(outside), character())
})

test_that("backup files do not change valid folder data or cache candidates", {
  directory <- copy_camtrap_test_dataset()
  baseline <- .read_camdp(directory, enrich_taxonomy = FALSE)
  backups <- file.path(directory, c(
    "observations.csv.bak", "old-observations.csv", "mediaXcsv",
    "datapackage.json.backup"
  ))
  for (path in backups) writeLines("Not an input resource", path)
  actual <- .read_camdp(directory, enrich_taxonomy = FALSE)
  expect_identical(actual$data, baseline$data)
  expect_identical(actual$source_file_metadata, baseline$source_file_metadata)

  file.rename(file.path(directory, "media.csv"),
              file.path(directory, "media.csv.bak"))
  expect_error(.read_camdp(directory, enrich_taxonomy = FALSE),
               "media.csv is not available", fixed = TRUE)
})

test_that("ZIP stems are anchored and ordinary ZIP data match folder imports", {
  commands <- unique(c(Sys.getenv("R_ZIPCMD"), "zip"))
  commands <- commands[nzchar(commands)]
  commands <- unname(Sys.which(commands))
  commands <- commands[nzchar(commands)]
  skip_if(!length(commands), "A ZIP executable is required for this test")
  directory <- copy_camtrap_test_dataset()
  zip_file <- tempfile("survey.zip.notes-", fileext = ".ZIP")
  output <- tempfile("camtrapReport-zip-output-")
  dir.create(output)
  on.exit(unlink(c(zip_file, output), recursive = TRUE, force = TRUE), add = TRUE)
  writeLines("Retained ancillary file", file.path(directory, "ancillary.txt"))
  invisible(capture.output(utils::zip(
    zip_file, file.path(directory, c(.camdp_core_files(), "ancillary.txt")),
    flags = "-jq", zip = commands[1]
  )))
  baseline <- .read_camdp(directory, enrich_taxonomy = FALSE)
  actual <- .read_camdp(zip_file, path = output, enrich_taxonomy = FALSE)
  stem <- sub("\\.zip$", "", basename(zip_file), ignore.case = TRUE)
  expect_match(actual$directory, stem, fixed = TRUE)
  expect_identical(actual$data, baseline$data)
  expect_true(file.exists(file.path(actual$directory, "ancillary.txt")))
  expect_false(dir.exists(file.path(".", stem)))

  writeLines("Retain unrelated old files", file.path(actual$directory, "old.txt"))
  archived_file <- file.path(actual$directory, "ancillary.txt")
  writeLines("Old archived member", archived_file)
  expect_error(.read_camdp(zip_file, path = output, enrich_taxonomy = FALSE),
               "already exists", fixed = TRUE)
  expect_identical(readLines(archived_file), "Old archived member")
  replaced <- .read_camdp(zip_file, path = output, enrich_taxonomy = FALSE,
                           overwrite = TRUE)
  expect_identical(replaced$data, baseline$data)
  expect_identical(readLines(archived_file), "Retained ancillary file")
  expect_identical(readLines(file.path(replaced$directory, "old.txt")),
                    "Retain unrelated old files")
})
