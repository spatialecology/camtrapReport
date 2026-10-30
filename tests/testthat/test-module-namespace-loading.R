test_that("the default module loader checks namespaces without attachment", {
  loader <- .make_package_loader_chunk(
    c("knitr", "dplyr, knitr", "dplyr"), core = "knitr"
  )
  expect_identical(report_test_loader_packages(strsplit(loader, "\n")[[1L]]),
                   c("knitr", "dplyr"))
  expect_no_match(loader, "library\\(|require\\(|attach\\(")

  lines <- strsplit(loader, "\n", fixed = TRUE)[[1L]]
  code <- paste(lines[-c(1L, length(lines))], collapse = "\n")
  before <- search()
  env <- new.env(parent = baseenv())
  expect_no_error(eval(parse(text = code), envir = env))
  expect_identical(env$missing_pkgs, character())
  expect_identical(search(), before)

  missing <- .make_package_loader_chunk(
    "camtrapReportThisModuleDependencyDoesNotExist", core = character()
  )
  lines <- strsplit(missing, "\n", fixed = TRUE)[[1L]]
  code <- paste(lines[-c(1L, length(lines))], collapse = "\n")
  expect_error(eval(parse(text = code), envir = new.env(parent = baseenv())),
    "Missing package\\(s\\): camtrapReportThisModuleDependencyDoesNotExist")
  expect_identical(search(), before)
})

test_that("report and module render paths use namespace checks consistently", {
  skip_if_not(rmarkdown::pandoc_available())
  local_math_free_html_fixture()
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  expected <- dplyr::count(cm$data$observations, scientificName, name = "records")
  section <- reportSection(
    name = "namespaced_counts", title = "Namespaced species counts",
    packages = c("dplyr", "knitr", "dplyr"),
    code_setting = c(echo = FALSE, results = "asis"),
    code = {
      counts <- dplyr::count(object$data$observations,
                             scientificName, name = "records")
      stopifnot(sum(counts$records) == nrow(object$data$observations))
      cat("Verified observation count:", sum(counts$records), "\n\n")
      knitr::kable(counts)
    }
  )
  directory <- tempfile("camtrapReport-namespaced-render-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  before <- search()

  cm$reportObjects <- list(namespaced_counts = section)
  output <- report(cm, filename = file.path(directory, "ecological"),
                   test = FALSE, view = FALSE)
  rmd <- readLines(file.path(directory, "ecological.Rmd"), warn = FALSE)
  expect_no_match(paste(rmd, collapse = "\n"),
                  "library\\(|require\\(|attach\\(")
  expect_identical(report_test_loader_packages(rmd), c("knitr", "dplyr"))
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  html <- gsub("[[:space:]]+", " ", html)
  expect_match(html, paste("Verified observation count:", sum(expected$records)),
               fixed = TRUE)
  expect_true(all(vapply(expected$scientificName, function(name) {
    grepl(name, html, fixed = TRUE)
  }, logical(1))))

  # This is the actual bundled spatial-summary module, not a text-only stand-in.
  spatial <- cm$reportObjectElements$Status_modules$spatial
  expect_s4_class(spatial, ".textSection")
  cm$statusReportObjects <- list(spatial = spatial, namespaced_counts = section)
  output <- status(cm, filename = file.path(directory, "status"), view = FALSE)
  rmd <- readLines(file.path(directory, "status.Rmd"), warn = FALSE)
  expect_no_match(paste(rmd, collapse = "\n"),
                  "library\\(|require\\(|attach\\(")
  expect_identical(report_test_loader_packages(rmd), c("knitr", "base", "dplyr"))
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  html <- gsub("[[:space:]]+", " ", html)
  expect_match(html, "Spatial summary of camera-trap locations", fixed = TRUE)
  expect_match(html, "Verified observation count:", fixed = TRUE)
  expect_lt(regexpr("Spatial summary of camera-trap locations", html,
                    fixed = TRUE)[[1L]],
            regexpr("Verified observation count:", html, fixed = TRUE)[[1L]])

  output <- testSection(section, object = cm, view = FALSE)
  on.exit(unlink(output, force = TRUE), add = TRUE)
  expect_true(file.exists(output))
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "Verified observation count:", fixed = TRUE)
  expect_true(.QuickTestReportSection(section, object = cm, path = directory))
  quick <- readLines(file.path(directory, "test.Rmd"), warn = FALSE)
  expect_no_match(paste(quick, collapse = "\n"),
                  "library\\(|require\\(|attach\\(")
  expect_identical(report_test_loader_packages(quick), c("knitr", "dplyr"))
  expect_identical(search(), before)
  expect_identical(dplyr::count(cm$data$observations, scientificName,
                              name = "records"), expected)
})
