test_that("report-section matching supports names, titles, and informative errors", {
  catalog <- data.frame(
    name = c("intro", "sampling", "sampling_effort"),
    title = c("Introduction", "Sampling", "Sampling effort"),
    path = c("intro", "methods / sampling", "methods / sampling_effort"),
    stringsAsFactors = FALSE
  )
  match_section <- .matchReportSection

  expect_identical(match_section(catalog, "INTRO")$name, "intro")
  expect_identical(match_section(catalog, "Introduction", by = "title")$name, "intro")
  expect_identical(
    suppressWarnings(match_section(catalog, "effort", by = "name"))$name,
    "sampling_effort"
  )
  expect_identical(
    match_section(catalog, "intro", by = "name", ignore.case = FALSE)$name,
    "intro"
  )
  expect_error(match_section(catalog, ""), "empty")
  expect_error(suppressWarnings(match_section(catalog, "unknown")), "No report section")
  expect_error(suppressWarnings(match_section(catalog, "amp", by = "title")), "More than one")
})

test_that("code and chunk settings are captured without evaluation side effects", {
  capture_code <- .capture_code_text
  capture_setting <- .capture_setting_text
  env <- list2env(list(code_value = c("x <- 1", "x + 1")), parent = baseenv())

  expect_match(capture_code(quote({x <- 1; x + 1})), "x <- 1", fixed = TRUE)
  expect_identical(capture_code(quote(code_value), env), "x <- 1\nx + 1")
  expect_identical(capture_code(quote(not_defined(1)), env), "not_defined(1)")

  expect_null(capture_setting(NULL))
  expect_identical(
    capture_setting(quote({c(echo = FALSE, warning = TRUE)})),
    "echo = FALSE, warning = TRUE"
  )
  expect_identical(capture_setting(quote(c("echo=FALSE", "results=asis"))),
                   "echo=FALSE, results=asis")
  expect_identical(capture_setting(quote(list(echo = FALSE, fig.width = 6))),
                   "echo = FALSE, fig.width = 6")
  expect_identical(capture_setting(quote("echo=FALSE")), "echo=FALSE")
})

test_that("section chunks can be created, patched, appended, and selected", {
  patch_section <- .update_section_chunk
  section <- reportSection("analysis", txt = "Analysis text")

  unchanged <- patch_section(
    section,
    code_missing = TRUE,
    code_setting_missing = TRUE,
    packages_missing = TRUE
  )
  expect_null(unchanged@Rchunk)

  created <- patch_section(
    section,
    code_missing = FALSE,
    code = "mean(1:3)",
    code_name = "first",
    code_setting_missing = FALSE,
    code_setting = "echo=FALSE",
    packages_missing = FALSE,
    packages = "stats"
  )
  expect_s4_class(created@Rchunk, ".Rchunk")
  expect_identical(created@Rchunk@name, "first")

  appended <- patch_section(
    created,
    code_missing = FALSE,
    code = "sum(1:3)",
    code_setting_missing = TRUE,
    packages_missing = TRUE,
    append_code = TRUE
  )
  expect_match(appended@Rchunk@code, "mean(1:3)\nsum(1:3)", fixed = TRUE)

  multiple <- patch_section(
    appended,
    code_missing = FALSE,
    code = "sd(1:3)",
    code_name = "second",
    code_setting_missing = TRUE,
    packages_missing = TRUE
  )
  expect_type(multiple@Rchunk, "list")
  expect_named(multiple@Rchunk, c("first", "second"))

  expect_error(
    patch_section(
      multiple,
      code_missing = FALSE,
      code = "var(1:3)",
      code_setting_missing = TRUE,
      packages_missing = TRUE
    ),
    "multiple code chunks"
  )

  patched <- patch_section(
    multiple,
    code_missing = TRUE,
    code_name = "second",
    code_setting_missing = FALSE,
    code_setting = "warning=FALSE",
    packages_missing = FALSE,
    packages = character()
  )
  expect_identical(patched@Rchunk$second@setting, "warning=FALSE")
  expect_identical(patched@Rchunk$second@packages, character())

  third <- patch_section(
    patched,
    code_missing = FALSE,
    code = "range(1:3)",
    code_name = "third",
    code_setting_missing = TRUE,
    packages_missing = TRUE
  )
  expect_named(third@Rchunk, c("first", "second", "third"))
})

test_that("updateReportSection updates title, text, code, and packages", {
  cm <- camR$new()
  cm$reportObjects <- list(custom = reportSection(
    "custom",
    title = "Original title",
    txt = list("First paragraph", "Second paragraph")
  ))

  updated <- updateReportSection(
    cm,
    section = "Original title",
    title = "Updated title",
    text = "Third paragraph",
    append_text = TRUE,
    code_name = "custom_code",
    code_setting = {c(echo = FALSE, results = "asis")},
    packages = "stats",
    code = {
      mean(1:3)
    }
  )
  section <- find_test_report_section(updated$reportObjects, "custom")

  expect_identical(section@title, "Updated title")
  expect_match(section@txt, "Second paragraph\n\nThird paragraph", fixed = TRUE)
  expect_s4_class(section@Rchunk, ".Rchunk")
  expect_match(section@Rchunk@setting, "echo = .*FALSE")
  expect_identical(section@Rchunk@packages, "stats")

  updated <- updateReportSection(
    updated,
    section = "custom",
    append_code = TRUE,
    code = {
      sum(1:3)
    }
  )
  section <- find_test_report_section(updated$reportObjects, "custom")
  expect_match(section@Rchunk@code, "sum(1:3)", fixed = TRUE)

  expect_error(updateReportSection(updated, section = 1), "single character")
  empty <- camR$new()
  empty$reportObjects <- list()
  expect_error(updateReportSection(empty, "missing", text = "x"), "No report sections")
})
