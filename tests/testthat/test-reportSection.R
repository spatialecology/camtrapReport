test_that("reportSection creates text-only sections", {
  section <- reportSection(
    name = "introduction",
    title = "Introduction",
    txt = "Camera-trap monitoring results."
  )

  expect_s4_class(section, ".textSection")
  expect_identical(section@name, "introduction")
  expect_identical(section@title, "Introduction")
  expect_identical(section@txt, "Camera-trap monitoring results.")
  expect_null(section@parent)
  expect_null(section@Rchunk)
})

test_that("reportSection captures code, settings, and package requirements", {
  section <- reportSection(
    name = "summary",
    title = "Summary",
    code_setting = {c(echo = FALSE, results = "asis")},
    packages = "stats",
    code = {
      mean(1:3)
    }
  )

  expect_s4_class(section@Rchunk, ".Rchunk")
  expect_identical(section@Rchunk@name, "summary_code")
  expect_identical(section@Rchunk@packages, "stats")
  expect_match(section@Rchunk@setting, "echo = FALSE", fixed = TRUE)
  expect_match(section@Rchunk@code, "mean(1:3)", fixed = TRUE)
})

test_that("testSection renders a self-contained section", {
  skip_if_not(rmarkdown::pandoc_available())

  section <- reportSection(
    name = "render_test",
    title = "Render test",
    txt = "A minimal section.",
    code = {
      summary(1:3)
    }
  )

  output <- testSection(section, view = FALSE)

  expect_true(file.exists(output))
  expect_match(output, "\\.html$")
})

test_that("low-level report chunks and Pandoc attributes are preserved", {
  chunk <- camtrapReport:::.getRchunk(
    parent = "methods",
    name = "low_level",
    setting = {c(echo = FALSE)},
    packages = "stats",
    code = {
      mean(1:3)
    }
  )
  env <- list2env(list(value = "camera traps"), parent = baseenv())
  glued <- camtrapReport:::.safe_glue_text(
    "Results for {value} {.unnumbered}",
    env,
    "pandoc_test"
  )

  expect_s4_class(chunk, ".Rchunk")
  expect_match(chunk@setting, "echo = FALSE", fixed = TRUE)
  expect_identical(glued, "Results for camera traps {.unnumbered}")
  expect_identical(camtrapReport:::.clean_chunk_name("bad & name"), "bad_name")
  expect_identical(camtrapReport:::.clean_chunk_name("", ""), "module")
  expect_identical(camtrapReport:::.glueRchunk(NULL), "")
  expect_error(
    camtrapReport:::.safe_glue_text("{missing_value}", env, "bad_section"),
    "bad_section"
  )
})

test_that("quick section checks report rendering success and failure", {
  skip_if_not(rmarkdown::pandoc_available())

  valid <- reportSection("quick_valid", txt = "Valid quick test")
  expect_true(camtrapReport:::.QuickTestReportSection(valid))

  testthat::local_mocked_bindings(
    render = function(...) stop("intentional render failure"),
    .package = "rmarkdown"
  )
  expect_false(camtrapReport:::.QuickTestReportSection(valid))
})
