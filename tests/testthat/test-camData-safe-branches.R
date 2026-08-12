test_that("read_camdp reports a missing jsonlite dependency", {
  testthat::local_mocked_bindings(
    .require = function(package) {
      !identical(package, "jsonlite")
    },
    .package = "camtrapReport"
  )
  
  expect_error(
    camtrapReport:::.read_camdp(
      file = tempfile("camdp-input-")
    ),
    "jsonlite package is not installed",
    fixed = TRUE
  )
})


test_that("read_camdp reports a missing data.table dependency", {
  testthat::local_mocked_bindings(
    .require = function(package) {
      !identical(package, "data.table")
    },
    .package = "camtrapReport"
  )
  
  expect_error(
    camtrapReport:::.read_camdp(
      file = tempfile("camdp-input-")
    ),
    "data.table package is not installed",
    fixed = TRUE
  )
})


test_that("read_camdp rejects an input that is neither ZIP nor directory", {
  missing_path <- tempfile(
    "camtrapReport-missing-camdp-"
  )
  
  testthat::local_mocked_bindings(
    .require = function(package) TRUE,
    .isZip = function(file) FALSE,
    .package = "camtrapReport"
  )
  
  expect_false(
    file.exists(missing_path)
  )
  
  expect_error(
    camtrapReport:::.read_camdp(
      file = missing_path
    ),
    "not a zip file or a directory",
    fixed = TRUE
  )
})


test_that("read_camdp rejects an empty directory", {
  test_dir <- tempfile(
    "camtrapReport-empty-camdp-"
  )
  
  dir.create(test_dir)
  
  on.exit(
    unlink(
      test_dir,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )
  
  testthat::local_mocked_bindings(
    .require = function(package) TRUE,
    .isZip = function(file) FALSE,
    .package = "camtrapReport"
  )
  
  expect_error(
    camtrapReport:::.read_camdp(
      file = test_dir
    ),
    "does not have the standard Camtrap DP files",
    fixed = TRUE
  )
})


test_that("read_camdp reports missing standard files", {
  test_dir <- tempfile(
    "camtrapReport-incomplete-camdp-"
  )
  
  dir.create(test_dir)
  
  on.exit(
    unlink(
      test_dir,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )
  
  file.create(
    file.path(
      test_dir,
      "datapackage.json"
    )
  )
  
  testthat::local_mocked_bindings(
    .require = function(package) TRUE,
    .isZip = function(file) FALSE,
    .package = "camtrapReport"
  )
  
  expect_error(
    camtrapReport:::.read_camdp(
      file = test_dir
    ),
    "standard data files",
    fixed = TRUE
  )
  
  expect_error(
    camtrapReport:::.read_camdp(
      file = test_dir
    ),
    "deployments.csv",
    fixed = TRUE
  )
  
  expect_error(
    camtrapReport:::.read_camdp(
      file = test_dir
    ),
    "observations.csv",
    fixed = TRUE
  )
  
  expect_error(
    camtrapReport:::.read_camdp(
      file = test_dir
    ),
    "media.csv",
    fixed = TRUE
  )
})


test_that("read_camdp uses UTC when timezone is empty", {
  test_dir <- tempfile(
    "camtrapReport-timezone-camdp-"
  )
  
  dir.create(test_dir)
  
  on.exit(
    unlink(
      test_dir,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )
  
  testthat::local_mocked_bindings(
    .require = function(package) TRUE,
    .isZip = function(file) FALSE,
    .package = "camtrapReport"
  )
  
  empty_timezone_values <- list(
    NULL,
    character(),
    NA_character_,
    ""
  )
  
  for (timezone in empty_timezone_values) {
    expect_error(
      camtrapReport:::.read_camdp(
        file = test_dir,
        tz = timezone
      ),
      "does not have the standard Camtrap DP files",
      fixed = TRUE
    )
  }
})


test_that("get_Taxonomic_DF handles taxa without vernacular names", {
  taxa <- list(
    list(
      taxonID = "https://www.gbif.org/species/5219404",
      scientificName = "Vulpes vulpes",
      family = "Canidae",
      order = "Carnivora",
      taxonRank = "species",
      vernacularNames = list()
    ),
    list(
      taxonID = "https://www.gbif.org/species/2435099",
      scientificName = "Meles meles",
      family = "Mustelidae",
      order = "Carnivora",
      taxonRank = "species",
      vernacularNames = list()
    )
  )
  
  result <- camtrapReport:::.get_Taxonomic_DF(
    taxa
  )
  
  expect_s3_class(
    result,
    "data.frame"
  )
  
  expect_identical(
    nrow(result),
    2L
  )
  
  expect_identical(
    result$taxonID,
    c("5219404", "2435099")
  )
  
  expect_identical(
    result$scientificName,
    c("Vulpes vulpes", "Meles meles")
  )
  
  expect_identical(
    result$family,
    c("Canidae", "Mustelidae")
  )
  
  expect_identical(
    result$order,
    c("Carnivora", "Carnivora")
  )
  
  expect_true(
    all(is.na(result$vernacularNames))
  )
})


test_that("get_Taxonomic_DF handles one named vernacular language", {
  taxa <- list(
    list(
      taxonID = "https://www.gbif.org/species/5219404",
      scientificName = "Vulpes vulpes",
      family = "Canidae",
      order = "Carnivora",
      taxonRank = "species",
      vernacularNames = c(
        eng = "Red fox"
      )
    ),
    list(
      taxonID = "https://www.gbif.org/species/2435099",
      scientificName = "Meles meles",
      family = "Mustelidae",
      order = "Carnivora",
      taxonRank = "species",
      vernacularNames = c(
        eng = "European badger"
      )
    )
  )
  
  result <- camtrapReport:::.get_Taxonomic_DF(
    taxa
  )
  
  expect_s3_class(
    result,
    "data.frame"
  )
  
  expect_identical(
    nrow(result),
    2L
  )
  
  expect_true(
    "vernacularNames.eng" %in% names(result)
  )
  
  expect_identical(
    result$vernacularNames.eng,
    c(
      "Red fox",
      "European badger"
    )
  )
})


test_that("get_Taxonomic_DF combines different vernacular languages", {
  taxa <- list(
    list(
      taxonID = "https://www.gbif.org/species/5219404",
      scientificName = "Vulpes vulpes",
      family = "Canidae",
      order = "Carnivora",
      taxonRank = "species",
      vernacularNames = c(
        eng = "Red fox",
        nld = "Vos"
      )
    ),
    list(
      taxonID = "https://www.gbif.org/species/2435099",
      scientificName = "Meles meles",
      family = "Mustelidae",
      order = "Carnivora",
      taxonRank = "species",
      vernacularNames = c(
        eng = "European badger"
      )
    )
  )
  
  result <- camtrapReport:::.get_Taxonomic_DF(
    taxa
  )
  
  expect_identical(
    nrow(result),
    2L
  )
  
  expect_true(
    all(
      c(
        "vernacularNames.eng",
        "vernacularNames.nld"
      ) %in% names(result)
    )
  )
  
  expect_identical(
    result$vernacularNames.eng,
    c(
      "Red fox",
      "European badger"
    )
  )
  
  expect_identical(
    result$vernacularNames.nld[1],
    "Vos"
  )
})


test_that("get_Taxonomic_DF handles unnamed vernacular values safely", {
  taxa <- list(
    list(
      taxonID = "taxon/1",
      scientificName = "Species one",
      family = "Family one",
      order = "Order one",
      taxonRank = "species",
      vernacularNames = "Common one"
    ),
    list(
      taxonID = "taxon/2",
      scientificName = "Species two",
      family = "Family two",
      order = "Order two",
      taxonRank = "species",
      vernacularNames = "Common two"
    )
  )
  
  result <- camtrapReport:::.get_Taxonomic_DF(
    taxa
  )
  
  expect_s3_class(
    result,
    "data.frame"
  )
  
  expect_identical(
    nrow(result),
    2L
  )
  
  expect_identical(
    result$taxonID,
    c("1", "2")
  )
  
  expect_identical(
    result$scientificName,
    c(
      "Species one",
      "Species two"
    )
  )
})


test_that("get_Taxonomic_DF preserves taxonomic ranks", {
  taxa <- list(
    list(
      taxonID = "taxon/100",
      scientificName = "Testus species",
      family = "Testidae",
      order = "Testiformes",
      taxonRank = "species",
      vernacularNames = c(
        eng = "Test species"
      )
    )
  )
  
  result <- camtrapReport:::.get_Taxonomic_DF(
    taxa
  )
  
  expect_identical(
    result$taxonID,
    "100"
  )
  
  expect_identical(
    result$scientificName,
    "Testus species"
  )
  
  expect_identical(
    result$family,
    "Testidae"
  )
  
  expect_identical(
    result$order,
    "Testiformes"
  )
  
  expect_identical(
    result$taxonRank,
    "species"
  )
  
  expect_true(
    is.na(result$class)
  )
})
