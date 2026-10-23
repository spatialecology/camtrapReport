test_that("status narrative retains the qualitative guide without false grading", {
  root <- system.file("statusSections", package = "camtrapReport")
  abstract <- .read_section_module(file.path(root, "abstract.yml"))
  conclusion <- .read_section_module(file.path(root, "conclusion.yml"))

  expect_identical(abstract$name, "abstract")
  expect_identical(conclusion$name, "conclusion")
  expect_null(abstract$parent)
  expect_null(conclusion$parent)
  expect_null(conclusion$code)
  for (label in c("Perfect", "Acceptable", "Needs Improvement")) {
    expect_match(abstract$text, label, fixed = TRUE)
    expect_match(conclusion$text, label, fixed = TRUE)
  }
  expect_match(abstract$text, "five chapters", fixed = TRUE)
  expect_match(abstract$text, "user-assessed guide", fixed = TRUE)
  expect_match(conclusion$text, "No overall classification is calculated",
    fixed = TRUE)
  expect_false(grepl("dataset is classified as", conclusion$text, fixed = TRUE))
  expect_false(grepl("report generation is not possible", abstract$text,
    fixed = TRUE))
  expect_match(abstract$text, "camData(..., update = TRUE)", fixed = TRUE)
  expect_match(conclusion$text, "camData(..., update = TRUE)", fixed = TRUE)
})

test_that("status abstract keeps dynamic setup fields and chapter heading", {
  root <- system.file("statusSections", package = "camtrapReport")
  abstract <- .read_section_module(file.path(root, "abstract.yml"))
  expect_match(abstract$code, "Chapter 1: Setup", fixed = TRUE)
  for (field in c("country", "TimeZone", "coordinate_range", "status_MCArea")) {
    expect_match(abstract$code, paste0("object$data_status$Spatial$", field),
      fixed = TRUE)
  }
  expect_match(abstract$code, "object$siteName", fixed = TRUE)
})
