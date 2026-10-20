test_that("filter preserves available English vernacular names", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  taxonomy <- cm$data$taxonomy

  expect_true("vernacularNames.eng" %in% names(taxonomy))
  expect_no_error(cm$filter())
  expect_gt(nrow(cm$frequent_species), 0L)
  selected <- match(
    cm$frequent_species$scientificName,
    taxonomy$scientificName
  )
  expect_identical(
    cm$frequent_species$vernacularNames.eng,
    taxonomy$vernacularNames.eng[selected]
  )
})

test_that("filter works when optional English vernacular names are absent", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  cm$filter()
  expected_names <- cm$frequent_species$scientificName
  expect_gt(length(expected_names), 0L)

  cm$data$taxonomy$vernacularNames.eng <- NULL
  expect_no_error(cm$filter())
  expect_identical(cm$frequent_species$scientificName, expected_names)
  expect_type(cm$frequent_species$vernacularNames.eng, "character")
  expect_length(
    cm$frequent_species$vernacularNames.eng,
    length(expected_names)
  )
  expect_true(all(is.na(cm$frequent_species$vernacularNames.eng)))
})

test_that("filter returns empty columns when no species meet the threshold", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  cm$filterCount <- Inf

  expect_no_error(cm$filter())
  expected_species <- cm$frequent_species
  expect_identical(nrow(expected_species), 0L)

  cm$data$taxonomy$vernacularNames.eng <- NULL
  expect_no_error(cm$filter())
  expect_identical(cm$frequent_species, expected_species)
  expect_identical(cm$frequent_species$vernacularNames.eng, character())
})
