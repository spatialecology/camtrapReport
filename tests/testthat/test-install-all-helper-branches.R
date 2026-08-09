test_that("is.installed preserves input order and names", {
  result <- camtrapReport:::.is.installed(
    c(
      "methods",
      "nonexistent_package_987654",
      "stats"
    )
  )
  
  expect_identical(
    names(result),
    c(
      "methods",
      "nonexistent_package_987654",
      "stats"
    )
  )
  
  expect_identical(
    unname(result),
    c(TRUE, FALSE, TRUE)
  )
})


test_that("is.installed handles duplicate and invalid values", {
  result <- camtrapReport:::.is.installed(
    c(
      "methods",
      "methods",
      "",
      NA_character_
    )
  )
  
  expect_length(result, 4L)
  expect_true(result[[1]])
  expect_true(result[[2]])
  expect_false(result[[3]])
  expect_false(result[[4]])
})


test_that("package loader handles empty and valid package groups", {
  result <- camtrapReport:::.loadLib(
    list(
      character(),
      "methods",
      c("methods", "stats")
    )
  )
  
  expect_identical(
    unname(result),
    c(TRUE, TRUE, TRUE)
  )
})


test_that("package configuration helpers return stable vectors", {
  package_list <- camtrapReport:::.getPackageList()
  github_list <- camtrapReport:::.getPackageGitHubList()
  gitlab_list <- camtrapReport:::.getPackageGitLabList()
  
  expect_type(package_list, "character")
  expect_false(anyNA(package_list))
  expect_false(any(package_list == ""))
  
  expect_type(github_list, "character")
  expect_type(gitlab_list, "character")
})


test_that("require helper rejects invalid package names", {
  require_package <- camtrapReport:::.require
  
  expect_false(require_package(character()))
  test_that("require helper handles valid and invalid package names", {
    require_package <- camtrapReport:::.require
    
    expect_false(require_package(character()))
    expect_false(require_package(NA_character_))
    expect_false(require_package(""))
    expect_false(
      require_package(
        "nonexistent_package_987654"
      )
    )
    
    expect_true(require_package("methods"))
  })
  expect_false(require_package(NA_character_))
  expect_false(require_package(""))
  expect_false(
    require_package(
      "nonexistent_package_987654"
    )
  )
  
  expect_true(require_package("methods"))
})


test_that("eval helper evaluates code in supplied environment", {
  env <- new.env(parent = baseenv())
  env$x <- 4
  
  expect_identical(
    camtrapReport:::.eval(
      "x * 3",
      env
    ),
    12
  )
})