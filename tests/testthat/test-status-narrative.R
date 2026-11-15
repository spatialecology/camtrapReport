test_that("status narrative has the exact diagnostic abstract without rankings", {
  root <- system.file("statusSections", package = "camtrapReport")
  abstract <- .read_section_module(file.path(root, "abstract.yml"))
  expected <- paste(
    "The Data Status Check provides an overview of the camera-trap dataset and summarises automated checks for data completeness, internal consistency and potential quality issues. The default report comprises five sections: Setup (including spatial and temporal checks), Data Availability, Validation, Annotation, and Observation Types by Capture Method.",
    "The report provides a structured summary of data quality, highlighting potential issues and limitations that may affect subsequent ecological analyses. Users can review and correct flagged issues in their original data management platforms, such as Agouti or TRAPPER, save the corrected data and rerun the checks before generating the Ecological Report.", sep = "\n\n")
  expect_identical(trimws(abstract$text), expected)
  expect_null(abstract$parent)
  expect_false(file.exists(file.path(root, "conclusion.yml")))
  for (label in c("Perfect", "Acceptable", "Needs Improvement")) {
    expect_false(any(vapply(list.files(root, pattern = "[.]yml$", full.names = TRUE),
      function(file) any(grepl(label, readLines(file, warn = FALSE), fixed = TRUE)), logical(1))))
  }
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  expect_false("conclusion" %in% names(cm$reportObjectElements$Status_modules))
  for (profile in c("default", "EOW")) {
    entries <- .resolve_profile(profile, cm)@status$module
    expect_false("conclusion" %in% entries)
    expect_true(all(c("abstract", "spatial", "temporal", "availability", "validation",
                      "annotation", "observation_type") %in% entries))
  }
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


test_that("default and custom status profiles render diagnostics with valid anchors", {
  skip_if_not(rmarkdown::pandoc_available())
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("xml2")
  local_math_free_html_fixture()
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  add_profile(cm, reportProfile("diagnostic_subset", status = c("abstract", "availability")))
  directory <- tempfile("status-diagnostics-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  checks <- cm$data_status
  for (profile in c("default", "EOW", "diagnostic_subset")) {
    output <- status(cm, profile = profile, filename = file.path(directory, profile), view = FALSE)
    expect_true(file.exists(output))
    doc <- xml2::read_html(output)
    text <- gsub("[[:space:]]+", " ", xml2::xml_text(doc))
    expect_match(text, "The Data Status Check provides an overview", fixed = TRUE)
    expect_match(text, "before generating the Ecological Report.", fixed = TRUE)
    headings <- xml2::xml_text(xml2::xml_find_all(doc, ".//h1|.//h2|.//h3"))
    headings <- gsub("[[:space:]]+", " ", headings)
    expect_false(any(grepl("Conclusion", headings, fixed = TRUE)))
    expect_false(any(grepl("Perfect|Acceptable|Needs Improvement", text)))
    expect_true(any(grepl("Chapter 1: Setup", headings, fixed = TRUE)))
    expect_true(any(grepl("Chapter 2: Data Availability", headings, fixed = TRUE)))
    if (profile != "diagnostic_subset") {
      for (chapter in c("Chapter 3: Validation", "Chapter 4: Annotation",
                        "Chapter 5: Observation Types by Capture Method")) {
        expect_true(any(grepl(chapter, headings, fixed = TRUE)))
      }
    } else expect_false(any(grepl("Chapter 3:", headings, fixed = TRUE)))
    anchors <- xml2::xml_attr(xml2::xml_find_all(doc, './/a[starts-with(@href,"#")]'), "href")
    ids <- xml2::xml_attr(xml2::xml_find_all(doc, './/*[@id]'), "id")
    targets <- sub("^#", "", anchors)
    expect_true(all(targets[nzchar(targets)] %in% ids))
    expect_identical(cm$data_status, checks)
  }
})
