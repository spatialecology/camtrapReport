test_that("bundled profiles select ordered modules from both pools", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)

  expect_setequal(profile_names(cm), c("default", "EOW"))

  eow <- sections(cm, profile = "EOW")
  expect_length(eow, 22L)
  expect_true("report::location_eow" %in% eow)
  expect_true("status::spatial" %in% eow)

  report_pool_before <- names(cm$reportObjectElements$Modules)
  status_pool_before <- names(cm$reportObjectElements$Status_modules)

  selected <- sections(
    cm,
    n = eow,
    profile = "EOW",
    report_type = "report"
  )

  expect_s4_class(selected, "camReport")
  expect_named(cm$reportObjectElements$Modules, report_pool_before)
  expect_named(
    cm$reportObjectElements$Status_modules,
    status_pool_before
  )
  expect_s4_class(find_test_report_section(cm$reportObjects, "spatial"),
                  ".textSection")
  expect_identical(
    find_test_report_section(cm$reportObjects, "spatial")@parent,
    "appendix"
  )
})

test_that("profiles can be written, read, registered, and edited", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  brief <- reportProfile(
    name = "brief",
    description = "A test profile",
    report = c("introduction", "methods"),
    status = c("abstract", "availability")
  )

  file <- tempfile(fileext = ".yml")
  on.exit(unlink(file, force = TRUE), add = TRUE)
  expect_identical(write_profile(brief, file), normalizePath(file))
  expect_identical(read_profile(file), brief)

  add_profile(cm, file)
  expect_true("brief" %in% profile_names(cm))
  expect_identical(
    sections(cm, profile = "brief"),
    c("report::introduction", "report::methods")
  )

  sections(
    cm,
    n = "report::introduction",
    profile = "brief",
    report_type = "report"
  )
  expect_identical(
    sections(cm, profile = "brief"),
    "report::introduction"
  )

  sections(
    cm,
    n = c("status::abstract", "status::availability"),
    profile = "brief",
    report_type = "status"
  )
  expect_identical(
    sections(cm, profile = "brief", report_type = "status"),
    c("status::abstract", "status::availability")
  )
})

test_that("section_names distinguishes pools and profile selections", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  all_names <- section_names(object = cm, report_type = "all")
  profile_modules <- section_names(
    object = cm,
    profile = "EOW",
    report_type = "report"
  )

  expect_true(any(startsWith(all_names, "report::")))
  expect_true(any(startsWith(all_names, "status::")))
  expect_true(all(grepl("::", profile_modules, fixed = TRUE)))
  expect_true("status::availability" %in% profile_modules)

  without_methods <- section_names(
    object = cm,
    profile = "EOW",
    exclude = "methods"
  )
  expect_false("report::methods" %in% without_methods)
  expect_false("report::study_area" %in% without_methods)
  expect_false("report::location_eow" %in% without_methods)
})

test_that("invalid profile hierarchies do not replace the attached tree", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  before <- names(cm$reportObjects)
  invalid <- reportProfile(name = "orphan", report = "study_area")
  add_profile(cm, invalid)

  expect_error(
    sections(cm, n = "study_area", profile = "orphan"),
    "parent 'methods' must appear earlier"
  )
  expect_named(cm$reportObjects, before)
})

test_that("PDF output is inferred and uses the HTML conversion route", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  output <- tempfile(fileext = ".pdf")
  rmd <- sub("\\.pdf$", ".Rmd", output)
  on.exit(unlink(c(output, rmd), force = TRUE), add = TRUE)

  result <- testthat::with_mocked_bindings(
    report(cm, filename = output, view = FALSE, test = FALSE),
    .generate_report = function(object, output_file, rmd_file) {
      writeLines("<html><body>test</body></html>", output_file)
      writeLines("---", rmd_file)
      output_file
    },
    .convert_html_to_pdf = function(html_file, pdf_file) {
      expect_true(file.exists(html_file))
      writeLines("PDF test", pdf_file)
      pdf_file
    },
    .package = "camtrapReport"
  )

  expect_identical(normalizePath(result), normalizePath(output))
  expect_true(file.exists(output))
  expect_true(file.exists(rmd))
  expect_error(
    report(cm, filename = output, format = "html"),
    "disagree"
  )
})
