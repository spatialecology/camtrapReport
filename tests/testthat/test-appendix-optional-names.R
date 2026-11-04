appendix_test_expressions <- function() {
  module <- rmarkdown::yaml_front_matter(system.file(
    "reportSections", "appendix.yml", package = "camtrapReport"
  ))
  parse(text = module$code)
}

appendix_test_fixture <- function(english = TRUE, favourite = FALSE) {
  taxonomy <- data.frame(
    taxonID = c("tax-1", "tax-2"),
    vernacularNames.eng = c("Red fox", "Roe deer"),
    scientificName = c("Vulpes vulpes", "Capreolus capreolus"),
    stringsAsFactors = FALSE
  )
  if (!english) taxonomy$vernacularNames.eng <- NULL
  list(data = list(
    media = data.frame(
      sequenceID = c("seq-1", "seq-2"),
      favourite = c(favourite, FALSE),
      fileName = c("one.jpg", "two.jpg"),
      filePath = c("https://example.invalid/one.jpg",
                   "https://example.invalid/two.jpg"),
      stringsAsFactors = FALSE
    ),
    observations = data.frame(
      sequenceID = c("seq-1", "seq-2"),
      taxonID = c("tax-1", "tax-2"), stringsAsFactors = FALSE
    ),
    taxonomy = taxonomy
  ))
}

appendix_test_environment <- function(object) {
  env <- new.env(parent = asNamespace("camtrapReport"))
  env$object <- object
  for (name in c("download.file", "dir.create", "image_read", "image_scale",
                 "image_write", "HTML")) {
    env[[name]] <- function(...) {
      stop("No image, network or file operation is allowed in this fixture.")
    }
  }
  env
}

test_that("appendix downloads use temporary storage without changing source data", {
  object <- appendix_test_fixture(favourite = TRUE)

  root <- tempfile("camtrapReport-appendix-source-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  object$info <- list(directory = root)

  env <- new.env(parent = asNamespace("camtrapReport"))
  env$object <- object
  env$destinations <- character()

  env$download.file <- function(url, destfile, ...) {
    env$destinations <- c(env$destinations, destfile)
    stop("Offline fixture")
  }

  invisible(capture.output(
    eval(appendix_test_expressions(), envir = env)
  ))

  on.exit(
    unlink(env$.picture_dir, recursive = TRUE),
    add = TRUE
  )

  expect_length(env$destinations, 1L)

  expect_true(startsWith(
    normalizePath(dirname(env$destinations), winslash = "/"),
    paste0(normalizePath(tempdir(), winslash = "/"), "/")
  ))

  expect_length(
    list.files(root, all.files = TRUE, no.. = TRUE),
    0L
  )
})

test_that("the actual appendix module accepts missing optional English names", {
  code <- appendix_test_expressions()
  for (english in c(TRUE, FALSE)) {
    env <- appendix_test_environment(appendix_test_fixture(english = english))
    output <- capture.output(eval(code, envir = env))
    expect_identical(output, character())
    expect_identical(nrow(env$favoImgs), 0L)
    expect_identical(env$.downloadable, FALSE)
    expect_identical("vernacularNames.eng" %in% names(env$favoImgs), english)
    expect_true("scientificName" %in% names(env$favoImgs))
  }
})

test_that(
  "appendix favourite selection and scientific names remain unchanged", {
  first <- appendix_test_expressions()[[1L]]
  expect_identical(first[[1L]], as.name("<-"))
  expect_identical(first[[2L]], as.name("favoImgs"))
  present <- appendix_test_environment(appendix_test_fixture(favourite = TRUE))
  eval(first, envir = present)
  expected <- dplyr::group_by(data.frame(
    sequenceID = "seq-1", favourite = TRUE, fileName = "one.jpg",
    filePath = "https://example.invalid/one.jpg", taxonID = "tax-1",
    vernacularNames.eng = "Red fox", scientificName = "Vulpes vulpes",
    stringsAsFactors = FALSE
  ), scientificName)
  expect_identical(present$favoImgs, expected)

  absent <- appendix_test_environment(appendix_test_fixture(
    english = FALSE, favourite = TRUE
  ))
  eval(first, envir = absent)
  expect_identical(
    absent$favoImgs,
    dplyr::select(present$favoImgs, -dplyr::all_of("vernacularNames.eng"))
  )
  expect_identical(absent$favoImgs$scientificName, "Vulpes vulpes")
})
