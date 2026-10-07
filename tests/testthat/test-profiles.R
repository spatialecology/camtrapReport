test_that("bundled profiles select ordered modules from both pools", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)

  expect_setequal(profile_names(cm), c("default", "EOW"))

  eow <- sections(cm, profile = "EOW")
  expect_length(eow, 22L)
  expect_true("report::location_eow" %in% eow)
  expect_true("report::population_density_annual" %in% eow)
  expect_false("report::population_density" %in% eow)
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
    find_test_report_section(
      cm$reportObjects,
      "model_parameters_eow"
    )@parent,
    "population_density_annual"
  )
  expect_identical(
    find_test_report_section(cm$reportObjects, "spatial")@parent,
    "appendix_eow"
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
    .generate_report = function(object, output_file, rmd_file, toc) {
      expect_false(toc)
      expect_null(find_test_report_section(object$reportObjects, "appendix"))
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
  expect_s4_class(
    find_test_report_section(cm$reportObjects, "appendix"),
    ".textSection"
  )
  expect_error(
    report(cm, filename = output, format = "html"),
    "disagree"
  )
})

test_that("module formats filter render trees without losing user state", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  cm$reportObjectElements$Modules_info <- data.frame(
    ID = 1:3,
    name = c("portable", "html_only", "html_child"),
    parent = c(".root", ".root", "html_only"),
    formats = c("both", "html", "both"),
    tested = NA,
    stringsAsFactors = FALSE
  )
  cm$reportObjects <- list()
  cm$addReportObject(reportSection(name = "portable", title = "Portable"))
  cm$addReportObject(reportSection(name = "html_only", title = "HTML"))
  cm$addReportObject(reportSection(
    name = "html_child",
    title = "HTML child",
    parent = "html_only"
  ))

  excluded <- suppressMessages(
    .prepare_attached_modules_for_format(cm, format = "pdf")
  )

  expect_setequal(excluded, c("html_only", "html_child"))
  expect_s4_class(
    find_test_report_section(cm$reportObjects, "portable"),
    ".textSection"
  )
  expect_null(find_test_report_section(cm$reportObjects, "html_only"))
})

test_that("bundled EOW appendix and print CSS are PDF-safe", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  sections(cm, n = sections(cm, profile = "EOW"), profile = "EOW")

  appendix <- find_test_report_section(cm$reportObjects, "appendix_eow")
  spatial <- find_test_report_section(cm$reportObjects, "spatial")
  css <- .report_css_block()

  expect_s4_class(appendix, ".textSection")
  expect_false(grepl("tabset", appendix@title, fixed = TRUE))
  expect_identical(spatial@parent, "appendix_eow")
  expect_match(css, "a[href]::after", fixed = TRUE)
  expect_match(css, ".tab-content > .tab-pane", fixed = TRUE)
})


test_that("bundled report maps default to self-contained backgrounds", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)

  expect_identical(cm$setting$map_basemap, "offline")

  report_modules <- system.file(
    "reportSections",
    package = "camtrapReport"
  )
  map_modules <- file.path(
    report_modules,
    c("location.yml", "location_EOW.yml", "richness.yml", "spatial_density.yml")
  )
  module_text <- unlist(lapply(map_modules, readLines, warn = FALSE))

  expect_false(any(grepl("addTiles", module_text, fixed = TRUE)))
  expect_true(any(grepl("add_report_basemap", module_text, fixed = TRUE)))
})
