test_that("report metadata can be read and updated", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)

  metadata <- info(cm, c("title", "siteName"))

  expect_s3_class(metadata, "camInfo")
  expect_named(metadata, c("title", "siteName"))

  info(cm, "title") <- "Updated camera-trap report"

  expect_identical(cm$title, "Updated camera-trap report")
})

test_that("attached report sections can be listed and updated", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  catalog <- listReportSections(cm)

  expect_s3_class(catalog, "data.frame")
  expect_named(catalog, c("name", "title", "parent", "path"))
  expect_identical(nrow(catalog), 23L)
  expect_false(anyDuplicated(catalog$name) > 0L)

  target <- catalog$name[[1]]
  updated_text <- "Text added by the report-section test."

  updated <- updateReportSection(
    cm,
    section = target,
    text = updated_text
  )
  section <- find_test_report_section(updated$reportObjects, target)

  expect_s4_class(updated, "camReport")
  expect_s4_class(section, ".textSection")
  expect_identical(section@txt, updated_text)
})

test_that("section selectors expose and update valid modules", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  available <- section_names()
  attached <- sections(cm)

  expect_type(available, "character")
  expect_type(attached, "character")
  expect_gt(length(available), 0L)
  expect_true(all(attached %in% available))

  selected <- attached[[1]]

  expect_message(
    result <- sections(cm, selected),
    "report sections are updated",
    fixed = TRUE
  )
  expect_s4_class(result, "camReport")
})

test_that("a minimal ecological report renders without optional packages", {
  skip_if_not(rmarkdown::pandoc_available())

  cm <- camtrap_test_report()$copy(shallow = FALSE)
  minimal_section <- reportSection(
    name = "test_report",
    title = "Ecological-report rendering test",
    txt = "A minimal report section used by the package test suite.",
    code_setting = {c(echo = FALSE, results = "asis")},
    code = {
      cat("Observations in the test dataset:", nrow(object$data$observations))
    }
  )
  cm$reportObjects <- list(test_report = minimal_section)

  output_stem <- tempfile("camtrapReport-report-")
  output <- report(cm, filename = output_stem, view = FALSE, test = FALSE)
  rmd <- readLines(paste0(output_stem, ".Rmd"), warn = FALSE)

  expect_true(file.exists(output))
  expect_true(file.exists(paste0(output_stem, ".Rmd")))
  expect_match(output, "\\.html$")
  expect_identical(report_test_loader_packages(rmd), "knitr")
  expect_true(any(grepl("Observations in the test dataset", rmd, fixed = TRUE)))
})

test_that("a minimal data-status report renders from the bundled toy dataset", {
  skip_if_not(rmarkdown::pandoc_available())

  cm <- camtrap_test_report()$copy(shallow = FALSE)
  minimal_section <- reportSection(
    name = "test_status",
    title = "Data-status rendering test",
    txt = "A minimal report section used by the package test suite.",
    code_setting = {c(echo = FALSE, results = "asis")},
    code = {
      cat("Deployments in the test dataset:", nrow(object$data$deployments))
    }
  )
  cm$statusReportObjects <- list(test_status = minimal_section)

  output_stem <- tempfile("camtrapReport-status-")
  output <- status(cm, filename = output_stem, view = FALSE)
  rmd <- readLines(paste0(output_stem, ".Rmd"), warn = FALSE)

  expect_true(file.exists(output))
  expect_true(file.exists(paste0(output_stem, ".Rmd")))
  expect_match(output, "\\.html$")
  expect_identical(report_test_loader_packages(rmd), "knitr")
  expect_true(any(grepl("Deployments in the test dataset", rmd, fixed = TRUE)))
})
