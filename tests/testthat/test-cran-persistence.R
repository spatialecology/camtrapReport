test_that("imports leave installed and read-only source directories unchanged", {
  source <- system.file("external", "example-small", package = "camtrapReport")
  files <- list.files(source, full.names = TRUE, recursive = TRUE, all.files = TRUE)
  before <- tools::md5sum(files)
  area <- terra::vect(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow=TRUE),
                      type="polygons", crs="EPSG:4326")
  original_require <- .require
  local_mocked_bindings(.require = function(x) {
    if (x %in% c("camtrapDensity", "activity", "taxize")) return(FALSE)
    original_require(x)
  })
  imported <- capture_report_conditions(camData(source, study_area = area))$value
  expect_s4_class(imported, "camReport")
  expect_s4_class(imported$study_area$object, "SpatVector")
  expect_identical(tools::md5sum(files), before)
  expect_setequal(list.files(source, full.names=TRUE, recursive=TRUE, all.files=TRUE), files)
  expect_false(file.exists(file.path(source, "__camReport_Object.rds")))
  expect_false(file.exists(file.path(source, "study_area.map")))
})

test_that("explicit caches preserve REM state, study areas and existing files", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  cm$rem <- list(known = data.frame(density=2, lower=1, upper=3))
  cm$study_area$object <- terra::vect(matrix(c(1,2), ncol=2), crs="EPSG:4326")
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add=TRUE)
  .save_camreport_cache(cm, file)
  saved_hash <- tools::md5sum(file)
  restored <- .read_camreport_cache(file)
  expect_identical(restored$rem, cm$rem)
  expect_equal(terra::crds(restored$study_area$object), terra::crds(cm$study_area$object))
  expect_s4_class(cm$study_area$object, "SpatVector")
  expect_error(.save_camreport_cache(cm, file), "already exists")
  expect_identical(tools::md5sum(file), saved_hash)
  cm$title <- "Updated explicitly"
  .save_camreport_cache(cm, file, overwrite=TRUE)
  expect_identical(.read_camreport_cache(file)$title, "Updated explicitly")
  expect_error(.save_camreport_cache(cm, sub("rds$", "csv", file), TRUE), ".rds filename", fixed=TRUE)
  local_mocked_bindings(.read_camdp = function(...) stop("Must reuse cache"))
  result <- capture_report_conditions(camData(cm$info$directory, cache_file=file))$value
  expect_identical(result$rem, cm$rem)
  expect_error(camData(cm$info$directory, cache_file=file, update=TRUE), "already exists")
  expect_error(camData(cm$info$directory, cache_file=file, update=TRUE, overwrite=TRUE), "Must reuse cache")
})

test_that("failed serialization leaves the old cache intact and removes staging files", {
  cm <- camtrap_test_report()
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive=TRUE), add=TRUE)
  file <- file.path(root, "saved.rds")
  .save_camreport_cache(cm, file)
  hash <- tools::md5sum(file)
  writer <- .save_camreport_cache
  environment(writer) <- list2env(list(saveRDS = function(object, file, ...) {
    writeLines("partial serialization", file)
    stop("Serialization failed")
  }), parent = environment(.save_camreport_cache))
  expect_error(writer(cm, file, TRUE), "Serialization failed")
  expect_identical(tools::md5sum(file), hash)
  expect_identical(list.files(root, all.files=TRUE, no..=TRUE), "saved.rds")
})

test_that("omitted report paths are temporary and explicit paths are protected", {
  cm <- camtrap_test_report()
  for (filename in list(NULL, "", character(), NA_character_)) {
    out <- .resolve_report_output(cm, filename, "report", "html", TRUE)
    expect_identical(dirname(out$output), normalizePath(tempdir(), winslash="/"))
  }
  package_dir <- system.file(package="camtrapReport")
  expect_error(.resolve_report_output(cm, file.path(package_dir,"report"), "report", "html", TRUE), "installed package")
  expect_error(.check_cache_file(file.path(package_dir,"new.rds")), "installed package")
  profile <- read_profile(system.file("reportProfiles", "EOW.yml", package="camtrapReport"))
  expect_error(write_profile(profile, file.path(package_dir,"profile.yml")), "installed package")
  checker <- .check_output_directory
  environment(checker) <- list2env(list(file.access = function(...) -1L),
                                  parent = environment(checker))
  expect_error(checker(tempdir()), "not writable")
})

test_that("module inspection never creates trash and mutations require copied registries", {
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  expect_identical(nrow(.read_trash_index(root)), 0L)
  expect_length(list.files(root,all.files=TRUE,no..=TRUE), 0L)
  expect_error(.writable_module_dir(), "Supply 'dir'")
  expect_error(.writable_module_dir(dir=system.file("reportSections",package="camtrapReport")), "installed package")
  expect_identical(.writable_module_dir(dir=root), normalizePath(root,winslash="/"))
})

test_that("ZIP imports require a chosen destination and preserve existing data", {
  skip_if(!nzchar(Sys.which("zip")), "A ZIP executable is required")
  source <- system.file("external", "example-small", package="camtrapReport")
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  zipfile <- file.path(root,"input.zip")
  old <- setwd(source)
  on.exit(setwd(old),add=TRUE)
  utils::zip(zipfile, list.files(source), flags="-q", zip=unname(Sys.which("zip")))
  setwd(root)
  expect_error(.read_camdp(zipfile), "explicit extraction parent directory")
  expect_false(dir.exists(file.path(root,"input")))
  persistent <- .read_camdp(zipfile,path=root)
  expect_identical(persistent$directory,normalizePath(file.path(root,"input"),winslash="/"))
  expect_error(.read_camdp(zipfile,path=root),"already exists")
  expect_silent(.read_camdp(zipfile,path=root,overwrite=TRUE))
  expect_true(file.exists(file.path(persistent$directory,"observations.csv")))
})
