test_that("camData reads bundled habitat and study area inputs", {
  dataset_path <- camtrap_test_dataset()
  
  habitat_path <- system.file(
    "external",
    "habitat",
    "habitat.csv",
    package = "camtrapReport"
  )
  
  study_area_path <- system.file(
    "external",
    "study_area",
    "study_area.shp",
    package = "camtrapReport"
  )
  
  expect_true(file.exists(habitat_path))
  expect_true(file.exists(study_area_path))
  
  habitat <- utils::read.csv(
    habitat_path,
    stringsAsFactors = FALSE
  )
  
  cm <- camData(
    dataset_path,
    habitat = habitat,
    study_area = study_area_path,
    update = TRUE
  )
  
  expect_s4_class(
    cm,
    "camReport"
  )
  
  expect_identical(
    nrow(cm$habitat),
    8L
  )
  
  expect_setequal(
    cm$habitat$locationName,
    c(
      "LEUVEN_416",
      "LEUVEN_881",
      "LEUVEN_930",
      "LEUVEN_1210",
      "LEUVEN_1304",
      "LEUVEN_1434",
      "LEUVEN_1473",
      "LEUVEN_1713"
    )
  )
  
  expect_true(
    inherits(
      cm$study_area$object,
      "SpatVector"
    )
  )
  
  expect_true(
    file.exists(
      cm$study_area$path
    )
  )
})
