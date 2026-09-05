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
