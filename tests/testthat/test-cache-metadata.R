.camtrap_cache_fixture <- function(snapshot = TRUE, nested = FALSE) {
  directory <- .register_camtrap_test_path(tempfile("camtrap-cache-"))
  dir.create(directory)
  source_directory <- directory
  if (nested) {
    source_directory <- file.path(directory, "source")
    dir.create(source_directory)
  }
  sources <- file.path(source_directory, c(
    "datapackage.json", "deployments.csv", "media.csv", "observations.csv"
  ))
  stopifnot(all(file.create(sources)))
  object <- camR$new()
  object$description <- "Saved cache marker; inputs must not be reread."
  object$info$directory <- directory
  object$info$rem_analysis_signature <- .rem_analysis_signature()
  object$reportObjectElements$Profiles <- list()
  if (snapshot) {
    object$info$source_file_metadata <- .camdata_source_metadata(
      directory, sources
    )
  }
  cache <- file.path(directory, "__camReport_Object.rds")
  saveRDS(object, cache)
  list(directory = directory, sources = sources, object = object, cache = cache)
}

.camtrap_cache_bytes <- function(file) {
  readBin(file, what = raw(), n = file.info(file)$size)
}

test_that("source snapshots contain sorted lightweight relative metadata", {
  fixture <- .camtrap_cache_fixture(nested = TRUE)
  snapshot <- fixture$object$info$source_file_metadata
  expect_named(snapshot, c("version", "files", "candidates"))
  expect_identical(snapshot$version, 1L)
  expect_named(snapshot$files, c("path", "size", "mtime"))
  expect_identical(snapshot$files$path, paste0("source/", c(
    "datapackage.json", "deployments.csv", "media.csv", "observations.csv"
  )))
  expect_type(snapshot$files$size, "double")
  expect_type(snapshot$files$mtime, "double")
  expect_identical(snapshot$files$size, rep(0, 4L))
  expect_true(.valid_camdata_source_metadata(snapshot))
  expect_identical(snapshot$candidates, snapshot$files$path)
})

test_that("unchanged new-style caches are reused without rereading inputs", {
  fixture <- .camtrap_cache_fixture()
  before <- .camtrap_cache_bytes(fixture$cache)
  testthat::local_mocked_bindings(
    .read_camdp = function(...) {
      stop("Inputs must not be reread on cache reuse")
    },
    .package = "camtrapReport"
  )
  result <- capture_report_conditions(camData(fixture$directory))
  expect_length(result$warnings, 0L)
  expect_identical(result$value$description, fixture$object$description)
  expect_identical(
    result$value$info$source_file_metadata,
    fixture$object$info$source_file_metadata
  )
  expect_identical(.camtrap_cache_bytes(fixture$cache), before)
})

test_that("changed source size or mtime warns but preserves cached data", {
  for (change in c("size", "mtime")) {
    fixture <- .camtrap_cache_fixture()
    before <- .camtrap_cache_bytes(fixture$cache)
    if (identical(change, "size")) {
      writeLines("changed source content", fixture$sources[1])
    } else {
      Sys.setFileTime(
        fixture$sources[1], as.POSIXct("2000-01-01", tz = "UTC")
      )
    }
    result <- capture_report_conditions(camData(fixture$directory))
    expect_length(result$warnings, 1L)
    expect_match(
      result$warnings, "Source data have changed since", fixed = TRUE
    )
    expect_match(result$warnings, "update = TRUE", fixed = TRUE)
    expect_identical(result$value$description, fixture$object$description)
    expect_identical(.camtrap_cache_bytes(fixture$cache), before)
  }
})

test_that("added and removed core candidates warn without rebuilding", {
  for (change in c("added", "removed", "removed_directory")) {
    fixture <- .camtrap_cache_fixture(
      nested = change %in% c("added", "removed_directory")
    )
    before <- .camtrap_cache_bytes(fixture$cache)
    if (identical(change, "added")) {
      file.create(file.path(fixture$directory, "observations.csv"))
    } else if (identical(change, "removed")) {
      unlink(fixture$sources[1])
    } else {
      unlink(dirname(fixture$sources[1]), recursive = TRUE)
    }
    result <- capture_report_conditions(camData(fixture$directory))
    expect_length(result$warnings, 1L)
    expect_match(
      result$warnings, "Source data have changed since", fixed = TRUE
    )
    expect_identical(result$value$description, fixture$object$description)
    expect_identical(.camtrap_cache_bytes(fixture$cache), before)
  }
})

test_that("unrelated report exports do not invalidate source caches", {
  fixture <- .camtrap_cache_fixture()
  file.create(file.path(fixture$directory, c(
    "report-results.csv", "report-data.json", "study_area.map", "report.html"
  )))
  result <- capture_report_conditions(camData(fixture$directory))
  expect_length(result$warnings, 0L)
  expect_identical(result$value$description, fixture$object$description)
})

test_that("cache checks use known input directories without a recursive walk", {
  fixture <- .camtrap_cache_fixture(nested = TRUE)
  media_directory <- file.path(fixture$directory, "images", "deep")
  dir.create(media_directory, recursive = TRUE)
  file.create(file.path(media_directory, "observations.csv"))
  result <- capture_report_conditions(camData(fixture$directory))
  expect_length(result$warnings, 0L)
  expect_identical(result$value$description, fixture$object$description)
})

test_that("legacy caches warn once per reuse and are not rewritten", {
  fixture <- .camtrap_cache_fixture(snapshot = FALSE)
  before <- .camtrap_cache_bytes(fixture$cache)
  for (attempt in seq_len(2L)) {
    result <- capture_report_conditions(camData(fixture$directory))
    expect_length(result$warnings, 1L)
    expect_match(result$warnings, "cannot be verified", fixed = TRUE)
    expect_match(result$warnings, "update = TRUE", fixed = TRUE)
    expect_identical(result$value$description, fixture$object$description)
    expect_null(result$value$info$source_file_metadata)
    expect_identical(.camtrap_cache_bytes(fixture$cache), before)
  }
})

test_that("unavailable or unrecognised metadata leaves the cache usable", {
  fixture <- .camtrap_cache_fixture()
  before <- .camtrap_cache_bytes(fixture$cache)
  testthat::local_mocked_bindings(
    .camdata_source_metadata = function(...) stop("metadata unavailable"),
    .package = "camtrapReport"
  )
  result <- capture_report_conditions(camData(fixture$directory))
  expect_length(result$warnings, 1L)
  expect_match(result$warnings, "cannot be verified", fixed = TRUE)
  expect_identical(result$value$description, fixture$object$description)
  expect_identical(.camtrap_cache_bytes(fixture$cache), before)

  fixture$object$info$source_file_metadata <- list(version = 999L)
  saveRDS(fixture$object, fixture$cache)
  result <- capture_report_conditions(camData(fixture$directory))
  expect_length(result$warnings, 1L)
  expect_match(result$warnings, "cannot be verified", fixed = TRUE)
  expect_identical(result$value$description, fixture$object$description)
})

test_that("failed snapshot capture is advisory rather than an import error", {
  fixture <- .camtrap_cache_fixture()
  result <- capture_report_conditions(.capture_camdata_source_metadata(
    fixture$directory, character()
  ))
  expect_null(result$value)
  expect_length(result$warnings, 1L)
  expect_match(result$warnings, "could not be recorded", fixed = TRUE)
})

test_that("same-size edits with restored mtime are a metadata limitation", {
  fixture <- .camtrap_cache_fixture()
  source <- fixture$sources[1]
  timestamp <- as.POSIXct("2000-01-01", tz = "UTC")
  writeLines("original", source)
  Sys.setFileTime(source, timestamp)
  fixture$object$info$source_file_metadata <- .camdata_source_metadata(
    fixture$directory, fixture$sources
  )
  saveRDS(fixture$object, fixture$cache)
  writeLines("modified", source)
  Sys.setFileTime(source, timestamp)
  result <- capture_report_conditions(camData(fixture$directory))
  expect_length(result$warnings, 0L)
  expect_identical(result$value$description, fixture$object$description)
})

test_that("fresh imports persist metadata for the four files actually read", {
  object <- camtrap_test_report()
  snapshot <- object$info$source_file_metadata
  expect_true(.valid_camdata_source_metadata(snapshot))
  expect_identical(snapshot$files$path, c(
    "datapackage.json", "deployments.csv", "media.csv", "observations.csv"
  ))
  saved <- readRDS(file.path(object$info$directory, "__camReport_Object.rds"))
  expect_identical(saved$info$source_file_metadata, snapshot)
})

test_that("changes after parsing do not become a falsely fresh snapshot", {
  directory <- copy_camtrap_test_dataset()
  source <- file.path(directory, "datapackage.json")
  original_size <- unname(file.info(source)$size)
  original_sequences <- .getSequences
  testthat::local_mocked_bindings(
    .getSequences = function(media) {
      writeLines("source changed after JSON parsing", source)
      original_sequences(media)
    },
    .package = "camtrapReport"
  )
  imported <- .read_camdp(directory, enrich_taxonomy = FALSE)
  snapshot <- imported$source_file_metadata
  expect_true(.valid_camdata_source_metadata(snapshot))
  expect_identical(
    snapshot$files$size[snapshot$files$path == "datapackage.json"],
    original_size
  )
  expect_false(identical(unname(file.info(source)$size), original_size))
  expect_true(length(imported$json$taxonomic) > 0L)

  object <- camR$new()
  object$info$source_file_metadata <- snapshot
  result <- capture_report_conditions(.check_camdata_cache_metadata(
    object, directory
  ))
  expect_false(result$value)
  expect_length(result$warnings, 1L)
  expect_match(
    result$warnings, "Source data have changed since", fixed = TRUE
  )
})
