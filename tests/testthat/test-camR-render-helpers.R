test_that("package normalization handles vectors and comma-separated entries", {
  normalize_packages <- .normalize_packages
  
  expect_identical(
    normalize_packages(NULL),
    character()
  )
  
  expect_identical(
    normalize_packages(character()),
    character()
  )
  
  expect_identical(
    normalize_packages(
      c(
        "stats, methods",
        " knitr ",
        NA_character_,
        "",
        "stats"
      )
    ),
    c("stats", "methods", "knitr")
  )
})


test_that("module package collection handles single report chunks", {
  collect_packages <- .collect_module_packages
  
  section <- reportSection(
    name = "summary",
    title = "Summary",
    packages = "stats, methods",
    code = {
      mean(1:3)
    }
  )
  
  result <- collect_packages(section)
  
  expect_identical(
    result,
    c("stats", "methods")
  )
})


test_that("module package collection handles nested sections", {
  collect_packages <- .collect_module_packages
  
  first <- reportSection(
    name = "first",
    packages = c("stats", "methods"),
    code = {
      mean(1:3)
    }
  )
  
  second <- reportSection(
    name = "second",
    packages = "knitr, stats",
    code = {
      summary(1:3)
    }
  )
  
  nested_modules <- list(
    root = list(
      first = first,
      deeper = list(
        second = second
      )
    ),
    ignored = "not a report section"
  )
  
  expect_identical(
    collect_packages(nested_modules),
    c("stats", "methods", "knitr")
  )
})


test_that("module package collection handles lists of chunks", {
  collect_packages <- .collect_module_packages
  
  section <- reportSection(
    name = "multi_chunk",
    title = "Multiple chunks",
    txt = "A section containing multiple chunks."
  )
  
  first_chunk <- methods::new(
    ".Rchunk",
    parent = "multi_chunk",
    name = "first_chunk",
    setting = "echo=FALSE",
    packages = "stats",
    code = "mean(1:3)"
  )
  
  second_chunk <- methods::new(
    ".Rchunk",
    parent = "multi_chunk",
    name = "second_chunk",
    setting = "echo=FALSE",
    packages = "methods, knitr",
    code = "summary(1:3)"
  )
  
  section@Rchunk <- list(
    first_chunk,
    "ignored element",
    second_chunk
  )
  
  expect_identical(
    collect_packages(section),
    c("stats", "methods", "knitr")
  )
})


test_that("module package collection handles empty structures", {
  collect_packages <- .collect_module_packages
  
  expect_identical(
    collect_packages(NULL),
    character()
  )
  
  expect_identical(
    collect_packages(list()),
    character()
  )
  
  expect_identical(
    collect_packages(
      list(
        text = "plain text",
        number = 1
      )
    ),
    character()
  )
})


test_that("package loader creates an empty setup chunk", {
  make_loader <- .make_package_loader_chunk
  
  loader <- make_loader(
    pkgs = NULL,
    core = character(),
    attach = TRUE
  )
  
  expect_identical(
    loader,
    paste0(
      "```{r setup, include=FALSE}\n",
      "# no extra packages\n",
      "```\n"
    )
  )
})


test_that("package loader normalizes and deduplicates packages", {
  make_loader <- .make_package_loader_chunk
  
  loader <- make_loader(
    pkgs = c(
      "stats, methods",
      "knitr",
      "stats"
    ),
    core = c("knitr", " methods "),
    attach = TRUE
  )
  
  expect_match(
    loader,
    'pkgs <- c\\("knitr", "methods", "stats"\\)'
  )
  
  expect_match(
    loader,
    "requireNamespace",
    fixed = TRUE
  )
  
  expect_match(
    loader,
    "Missing package(s)",
    fixed = TRUE
  )
  
  expect_match(
    loader,
    "library(p, character.only = TRUE)",
    fixed = TRUE
  )
  
  expect_identical(lengths(
      regmatches(
        loader,
        gregexpr(
          '"stats"',
          loader,
          fixed = TRUE
        )
      )
    ), 1L)
})


test_that("package loader can check packages without attaching them", {
  make_loader <- .make_package_loader_chunk
  
  loader <- make_loader(
    pkgs = c("stats", "methods"),
    core = character(),
    attach = FALSE
  )
  
  expect_match(
    loader,
    'pkgs <- c\\("stats", "methods"\\)'
  )
  
  expect_match(
    loader,
    "requireNamespace",
    fixed = TRUE
  )
  
  expect_match(
    loader,
    "Missing package(s)",
    fixed = TRUE
  )
  
  expect_no_match(
    loader,
    "library\\("
  )
  
  expect_no_match(
    loader,
    "lapply\\("
  )
})


test_that("HTML attribute escaping protects report markup", {
  escape_attribute <- .html_attr_escape
  
  result <- escape_attribute(
    '<image title="A & B">'
  )
  
  expect_identical(
    result,
    "&lt;image title=&quot;A &amp; B&quot;&gt;"
  )
})

test_that("report logo block embeds a user-provided PNG", {
  logo_file <- tempfile(
    "camtrapReport-test-logo-",
    fileext = ".png"
  )
  
  grDevices::png(
    filename = logo_file,
    width = 300,
    height = 300
  )
  
  on.exit(
    {
      if (grDevices::dev.cur() > 1L) {
        grDevices::dev.off()
      }
      
      unlink(
        logo_file,
        force = TRUE
      )
    },
    add = TRUE
  )
  
  graphics::par(
    mar = c(0, 0, 0, 0)
  )
  
  graphics::plot.new()
  
  grDevices::dev.off()
  
  expect_true(
    file.exists(logo_file)
  )
  
  logo_block <- .report_logo_block(
    logo_file
  )
  
  expect_match(
    logo_block,
    "report-logo-placeholder",
    fixed = TRUE
  )
  
  expect_match(
    logo_block,
    '<img src="data:image/png;base64,',
    fixed = TRUE
  )
  
  expect_match(
    logo_block,
    'alt="Report logo"',
    fixed = TRUE
  )
  
  expect_no_match(
    logo_block,
    "PNG logo placeholder",
    fixed = TRUE
  )
})


test_that("report logo block handles absent user logos safely", {
  logo_block <- .report_logo_block(
    "a-logo-file-that-does-not-exist.png"
  )
  
  expect_type(
    logo_block,
    "character"
  )
  
  expect_length(
    logo_block,
    1L
  )
  
  expect_match(
    logo_block,
    "report-logo-placeholder",
    fixed = TRUE
  )
  
  # Depending on package installation, this uses either the bundled
  # default logo or the HTML placeholder.
  expect_true(
    grepl(
      "<img src=",
      logo_block,
      fixed = TRUE
    ) ||
      grepl(
        "PNG logo placeholder",
        logo_block,
        fixed = TRUE
      )
  )
})


test_that("report CSS block contains essential report styles", {
  css <- .report_css_block()
  
  expect_type(
    css,
    "character"
  )
  
  expect_length(
    css,
    1L
  )
  
  expect_match(
    css,
    "<style>",
    fixed = TRUE
  )
  
  expect_match(
    css,
    ".main-container",
    fixed = TRUE
  )
  
  expect_match(
    css,
    "report-logo-placeholder",
    fixed = TRUE
  )
  
  expect_match(
    css,
    "</style>",
    fixed = TRUE
  )
})
