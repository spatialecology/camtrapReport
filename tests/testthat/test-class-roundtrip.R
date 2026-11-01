test_that(
  "report classes retain dispatch and independent copies after RDS reuse", {
  original <- camtrap_test_report()$copy(shallow = FALSE)
  original$siteName <- "Original study"
  profile <- reportProfile("roundtrip", report = c("introduction", "methods"))
  add_profile(original, profile)
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  saveRDS(original, file)
  restored <- readRDS(file)

  expect_s4_class(restored, "camReport")
  expect_true(methods::validObject(restored))
  expect_identical(restored$data, original$data)
  expect_identical(restored$extractYears(), original$extractYears())
  expect_true("roundtrip" %in% profile_names(restored))
  expect_identical(
    sections(restored, profile = "roundtrip"),
    c("report::introduction", "report::methods")
  )
  info(restored, "title") <- "Restored report"
  expect_identical(info(restored, "title")$title, "Restored report")
  expect_false(identical(info(original, "title")$title, "Restored report"))
  copy <- restored$copy(shallow = FALSE)
  copy$siteName <- "Independent copy"
  expect_identical(restored$siteName, "Original study")
})
