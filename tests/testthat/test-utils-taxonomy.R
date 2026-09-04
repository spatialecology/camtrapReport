test_that("taxonomy lookup helpers fail clearly without taxize", {
  local_mocked_bindings(
    .require = function(...) FALSE,
    .package = "camtrapReport"
  )

  expect_error(
    .getMissingTaxon_GBIF("Vulpes vulpes"),
    "taxize package is required",
    fixed = TRUE
  )
  expect_error(
    .getMissingTaxon_NCBI("Vulpes vulpes"),
    "taxize package is required",
    fixed = TRUE
  )
})


test_that("taxonomy lookup helpers handle identifier lookup failures", {
  local_mocked_bindings(
    .require = function(...) TRUE,
    .eval = function(...) stop("synthetic lookup failure"),
    .package = "camtrapReport"
  )

  gbif <- .getMissingTaxon_GBIF("Vulpes vulpes")
  ncbi <- .getMissingTaxon_NCBI("Vulpes vulpes")

  expect_named(gbif, c("scientificName", "class", "order"))
  expect_named(ncbi, c("scientificName", "class", "order"))
  expect_identical(gbif$scientificName, "Vulpes vulpes")
  expect_identical(ncbi$scientificName, "Vulpes vulpes")
  expect_true(all(is.na(gbif[c("class", "order")])))
  expect_true(all(is.na(ncbi[c("class", "order")])))
})

test_that("taxonomy ranks are extracted as character scalars", {
  classification <- data.frame(
    name = c(
      "Animalia",
      "Chordata",
      "Mammalia",
      "Carnivora"
    ),
    rank = c(
      "kingdom",
      "phylum",
      "class",
      "order"
    ),
    stringsAsFactors = FALSE
  )
  
  expect_identical(
    .taxonomy_rank_value(classification, "class"),
    "Mammalia"
  )
  
  expect_identical(
    .taxonomy_rank_value(classification, "order"),
    "Carnivora"
  )
  
  expect_identical(
    .taxonomy_rank_value(classification, "missing"),
    NA_character_
  )
  
  expect_identical(
    .taxonomy_rank_value(
      classification["name"],
      "class",
      fallback_row = 3L
    ),
    "Mammalia"
  )
  
  factor_classification <- classification
  factor_classification$name <- factor(factor_classification$name)
  
  expect_identical(
    .taxonomy_rank_value(factor_classification, "class"),
    "Mammalia"
  )
  
  expect_identical(
    .taxonomy_rank_value(NULL, "class"),
    NA_character_
  )
})
