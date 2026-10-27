test_that("math-free fixture rendering preserves arguments and conditions", {
  renderer <- function(input, output_format = NULL, output_file = NULL,
                       output_options = NULL, envir = parent.frame(),
                       quiet = FALSE) {
    if (identical(input, "warning")) warning("original fixture warning")
    if (identical(input, "error")) stop("original fixture error")
    invisible(list(
      input = input, output_format = output_format, output_file = output_file,
      output_options = output_options, envir = envir, quiet = quiet
    ))
  }
  render <- math_free_html_fixture_renderer(renderer)
  original_options <- list(toc = TRUE, self_contained = TRUE, mathjax = "default")
  env <- new.env(parent = baseenv())
  value <- withVisible(render(
    "fixture.Rmd", "html_document", "fixture.html",
    output_options = original_options, envir = env, quiet = TRUE
  ))
  expect_false(value$visible)
  expect_identical(value$value$input, "fixture.Rmd")
  expect_identical(value$value$output_format, "html_document")
  expect_identical(value$value$output_file, "fixture.html")
  expect_identical(value$value$envir, env)
  expect_true(value$value$quiet)
  expect_identical(value$value$output_options,
                   list(toc = TRUE, self_contained = TRUE, mathjax = NULL))
  expect_identical(original_options$mathjax, "default")
  expect_identical(render("fixture.Rmd")$envir, environment())
  expect_warning(render("warning"), "original fixture warning", fixed = TRUE)
  expect_error(render("error"), "original fixture error", fixed = TRUE)
})

test_that("math-free fixture bindings are local and leave math defaults intact", {
  original_render <- rmarkdown::render
  original_default <- formals(rmarkdown::html_document)$mathjax
  local({
    local_math_free_html_fixture()
    expect_false(identical(rmarkdown::render, original_render))
    expect_identical(environment(rmarkdown::render), environment(original_render))
    expect_identical(formals(rmarkdown::html_document)$mathjax, original_default)
  })
  expect_identical(rmarkdown::render, original_render)
  expect_identical(formals(rmarkdown::html_document)$mathjax, original_default)
})
