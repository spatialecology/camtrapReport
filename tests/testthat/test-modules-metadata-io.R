test_that("section_dir uses a supplied existing directory", {
  test_dir <- tempfile("camtrapReport-section-dir-")
  dir.create(test_dir)
  
  on.exit(
    unlink(test_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  
  result <- .section_dir(
    dir = test_dir
  )
  
  expect_identical(
    result,
    normalizePath(
      test_dir,
      winslash = "/",
      mustWork = TRUE
    )
  )
})


test_that("section_dir rejects a supplied missing directory", {
  missing_dir <- tempfile(
    "camtrapReport-missing-section-dir-"
  )
  
  expect_false(
    dir.exists(missing_dir)
  )
  
  expect_error(
    .section_dir(
      dir = missing_dir
    )
  )
})


test_that("modules_info_path finds the metadata CSV", {
  test_dir <- tempfile("camtrapReport-module-info-")
  dir.create(test_dir)
  
  on.exit(
    unlink(test_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  
  info_file <- file.path(
    test_dir,
    "__modulesList.csv"
  )
  
  utils::write.csv(
    data.frame(
      ID = 1L,
      name = "methods",
      parent = ".root",
      stringsAsFactors = FALSE
    ),
    info_file,
    row.names = FALSE
  )
  
  result <- .modules_info_path(
    test_dir
  )
  
  expect_identical(
    normalizePath(
      result,
      winslash = "/",
      mustWork = TRUE
    ),
    normalizePath(
      info_file,
      winslash = "/",
      mustWork = TRUE
    )
  )
})


test_that("modules_info_path accepts a prefixed metadata filename", {
  test_dir <- tempfile(
    "camtrapReport-prefixed-module-info-"
  )
  
  dir.create(test_dir)
  
  on.exit(
    unlink(test_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  
  info_file <- file.path(
    test_dir,
    "package__modulesList.csv"
  )
  
  file.create(info_file)
  
  result <- .modules_info_path(
    test_dir
  )
  
  expect_identical(
    basename(result),
    "package__modulesList.csv"
  )
})


test_that("modules_info_path errors when metadata CSV is absent", {
  test_dir <- tempfile(
    "camtrapReport-no-module-info-"
  )
  
  dir.create(test_dir)
  
  on.exit(
    unlink(test_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  
  expect_error(
    .modules_info_path(
      test_dir
    ),
    "Could not find '__modulesList.csv'",
    fixed = TRUE
  )
})


test_that("read_modules_info errors when metadata is missing", {
  test_dir <- tempfile(
    "camtrapReport-read-missing-info-"
  )
  
  dir.create(test_dir)
  
  on.exit(
    unlink(test_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  
  expect_error(
    .read_modules_info(
      dir = test_dir,
      create_if_missing = FALSE
    ),
    "Could not find '__modulesList.csv'",
    fixed = TRUE
  )
})


test_that("read_modules_info creates metadata from root sections", {
  test_dir <- tempfile(
    "camtrapReport-create-module-info-"
  )
  
  dir.create(test_dir)
  
  on.exit(
    unlink(test_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  
  level0 <- c(
    "introduction",
    "methods",
    "results",
    "appendix"
  )
  
  result <- .read_modules_info(
    dir = test_dir,
    level0 = level0,
    create_if_missing = TRUE
  )
  
  expected <- data.frame(
    ID = seq_along(level0),
    name = level0,
    parent = rep(".root", length(level0)),
    stringsAsFactors = FALSE
  )
  
  rownames(result) <- NULL
  rownames(expected) <- NULL
  
  expect_identical(
    result,
    expected
  )
  
  expect_true(
    file.exists(
      file.path(
        test_dir,
        "__modulesList.csv"
      )
    )
  )
})


test_that("created module metadata can be read again", {
  test_dir <- tempfile(
    "camtrapReport-create-and-read-info-"
  )
  
  dir.create(test_dir)
  
  on.exit(
    unlink(test_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  
  level0 <- c(
    "introduction",
    "methods",
    "results"
  )
  
  created <- .read_modules_info(
    dir = test_dir,
    level0 = level0,
    create_if_missing = TRUE
  )
  
  reread <- .read_modules_info(
    dir = test_dir,
    create_if_missing = FALSE
  )
  
  rownames(created) <- NULL
  rownames(reread) <- NULL
  
  expect_identical(
    reread,
    created
  )
})


test_that("read_modules_info sorts rows by ID", {
  test_dir <- tempfile(
    "camtrapReport-sorted-module-info-"
  )
  
  dir.create(test_dir)
  
  on.exit(
    unlink(test_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  
  input <- data.frame(
    ID = c(30L, 10L, 20L),
    name = c(
      "results",
      "introduction",
      "methods"
    ),
    parent = rep(".root", 3),
    stringsAsFactors = FALSE
  )
  
  utils::write.csv(
    input,
    file.path(
      test_dir,
      "__modulesList.csv"
    ),
    row.names = FALSE
  )
  
  result <- .read_modules_info(
    dir = test_dir
  )
  
  expect_identical(
    result$name,
    c(
      "introduction",
      "methods",
      "results"
    )
  )
  
  expect_identical(
    result$ID,
    1:3
  )
})


test_that("read_modules_info normalizes parent values", {
  test_dir <- tempfile(
    "camtrapReport-normalized-module-info-"
  )
  
  dir.create(test_dir)
  
  on.exit(
    unlink(test_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  
  input <- data.frame(
    ID = 1:4,
    name = c(
      "introduction",
      "methods",
      "sampling",
      "results"
    ),
    parent = c(
      "root",
      "",
      " methods ",
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
  
  utils::write.csv(
    input,
    file.path(
      test_dir,
      "__modulesList.csv"
    ),
    row.names = FALSE,
    na = ""
  )
  
  result <- .read_modules_info(
    dir = test_dir
  )
  
  expect_identical(
    result$parent,
    c(
      ".root",
      ".root",
      "methods",
      ".root"
    )
  )
})


test_that("read_modules_info removes blank and duplicate names", {
  test_dir <- tempfile(
    "camtrapReport-clean-module-info-"
  )
  
  dir.create(test_dir)
  
  on.exit(
    unlink(test_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  
  input <- data.frame(
    ID = 1:5,
    name = c(
      "introduction",
      "",
      "methods",
      "methods",
      "results"
    ),
    parent = c(
      ".root",
      ".root",
      ".root",
      "introduction",
      ".root"
    ),
    stringsAsFactors = FALSE
  )
  
  utils::write.csv(
    input,
    file.path(
      test_dir,
      "__modulesList.csv"
    ),
    row.names = FALSE
  )
  
  result <- .read_modules_info(
    dir = test_dir
  )
  
  expect_identical(
    result$name,
    c(
      "introduction",
      "methods",
      "results"
    )
  )
  
  expect_identical(
    result$ID,
    1:3
  )
  
  expect_identical(
    result$parent,
    c(
      ".root",
      ".root",
      ".root"
    )
  )
})


test_that("read_modules_info handles metadata without an ID column", {
  test_dir <- tempfile(
    "camtrapReport-no-id-module-info-"
  )
  
  dir.create(test_dir)
  
  on.exit(
    unlink(test_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  
  input <- data.frame(
    name = c(
      "methods",
      "sampling",
      "results"
    ),
    parent = c(
      ".root",
      "methods",
      ".root"
    ),
    stringsAsFactors = FALSE
  )
  
  utils::write.csv(
    input,
    file.path(
      test_dir,
      "__modulesList.csv"
    ),
    row.names = FALSE
  )
  
  result <- .read_modules_info(
    dir = test_dir
  )
  
  expect_identical(
    result$ID,
    1:3
  )
  
  expect_identical(
    result$name,
    input$name
  )
  
  expect_identical(
    result$parent,
    input$parent
  )
})