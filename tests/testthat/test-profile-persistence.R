test_that("profiles preserve escaped metadata and parent overrides", {
  directory <- tempfile("camtrapReport-profile-roundtrip-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  profile <- reportProfile(
    name = "roundtrip",
    description = "Caf\u00e9: \"quoted\" text \\ path\nand a second line",
    report = data.frame(
      source = c("report", "status"),
      module = c("appendix", "spatial"),
      parent = c(".root", "appendix")
    ),
    status = character()
  )
  file <- file.path(directory, "profile.yml")

  expect_identical(
    write_profile(profile, file),
    normalizePath(file, winslash = "/", mustWork = TRUE)
  )
  expect_identical(read_profile(file), profile)
  expect_identical(profile@report$parent, c(".root", "appendix"))
  expect_identical(nrow(read_profile(file)@status), 0L)
})

test_that("profile replacement requires explicit consent", {
  directory <- tempfile("camtrapReport-profile-overwrite-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  file <- file.path(directory, "profile.yml")
  first <- reportProfile("first", report = "introduction")
  second <- reportProfile("second", status = "abstract")
  write_profile(first, file)
  original <- readBin(file, "raw", n = file.info(file)$size)

  expect_error(write_profile(second, file), "Profile file already exists",
               fixed = TRUE)
  expect_identical(readBin(file, "raw", n = file.info(file)$size), original)
  expect_identical(read_profile(file), first)
  expect_identical(
    write_profile(second, file, overwrite = TRUE),
    normalizePath(file, winslash = "/", mustWork = TRUE)
  )
  expect_identical(read_profile(file), second)
})

test_that("invalid profile paths fail without creating output", {
  directory <- tempfile("camtrapReport-profile-invalid-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  missing <- file.path(directory, "missing", "profile.yml")
  profile <- reportProfile("brief", report = "introduction")

  expect_error(write_profile(profile, missing),
               "profile output directory does not exist", fixed = TRUE)
  expect_false(dir.exists(dirname(missing)))

  file <- file.path(directory, "no-name.yml")
  writeLines(c("---", "report: []", "status: []", "---"), file)
  original <- readLines(file, warn = FALSE)
  expect_error(read_profile(file), "Profile YAML must define 'name'",
               fixed = TRUE)
  expect_identical(readLines(file, warn = FALSE), original)
})

test_that("profile registration protects case-insensitive names", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  first <- reportProfile("Brief", report = "introduction")
  second <- reportProfile("brief", status = "abstract")
  add_profile(cm, first)
  original <- cm$reportObjectElements$Profiles

  expect_error(add_profile(cm, second), "already registered", fixed = TRUE)
  expect_identical(cm$reportObjectElements$Profiles, original)
  expect_identical(add_profile(cm, second, overwrite = TRUE), cm)
  expect_identical(cm$reportObjectElements$Profiles$brief, second)
  expect_identical(sum(tolower(profile_names(cm)) == "brief"), 1L)
  expect_false("Brief" %in% profile_names(cm))
})
