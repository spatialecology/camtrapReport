test_that("installer inventory helpers return stable package vectors", {
  packages <- camtrapReport:::.getPackageList()
  github <- camtrapReport:::.getPackageGitHubList()
  gitlab <- camtrapReport:::.getPackageGitLabList()

  expect_type(packages, "character")
  expect_false(anyNA(packages))
  expect_false(any(packages == ""))
  expect_type(github, "character")
  expect_type(gitlab, "character")

  expect_true(camtrapReport:::.is.installed("methods"))
  expect_false(
    camtrapReport:::.is.installed(
      "a_package_that_does_not_exist_123"
    )
  )

  expect_identical(
    unname(
      camtrapReport:::.loadLib(
        list("methods", c("stats", "methods"))
      )
    ),
    c(TRUE, TRUE)
  )
})


test_that("pak remote references retain package names and sources", {
  github <- camtrapReport:::.pakRemoteReferences(
    c(examplePackage = "owner/repository"),
    source = "github"
  )

  gitlab <- camtrapReport:::.pakRemoteReferences(
    c(otherPackage = "group/project"),
    source = "gitlab"
  )

  expect_identical(
    github,
    c(examplePackage = "examplePackage=github::owner/repository")
  )

  expect_identical(
    gitlab,
    c(otherPackage = "otherPackage=gitlab::group/project")
  )

  expect_identical(
    camtrapReport:::.pakRemoteReferences(character(), "github"),
    stats::setNames(character(), character())
  )
})


test_that("pak remote references reject incomplete configuration", {
  expect_error(
    camtrapReport:::.pakRemoteReferences(
      "owner/repository",
      source = "github"
    ),
    "named character vector",
    fixed = TRUE
  )

  expect_error(
    camtrapReport:::.pakRemoteReferences(
      c(examplePackage = ""),
      source = "github"
    ),
    "must not be empty",
    fixed = TRUE
  )

  expect_error(
    camtrapReport:::.pakRemoteReferences(
      c(examplePackage = "owner/repository"),
      source = "invalid"
    ),
    "'arg' should be one of",
    fixed = TRUE
  )
})


test_that("pak reinstall references preserve existing query parameters", {
  references <- c(
    cranPackage = "cranPackage",
    githubPackage = "githubPackage=github::owner/repository?subdir=pkg"
  )

  expect_identical(
    camtrapReport:::.pakReinstallReferences(references),
    c(
      cranPackage = "cranPackage?reinstall",
      githubPackage = paste0(
        "githubPackage=github::owner/repository?subdir=pkg",
        "&reinstall"
      )
    )
  )

  expect_identical(
    camtrapReport:::.pakReinstallReferences(character()),
    character()
  )
})


test_that("pak installation helper handles empty input without pak", {
  expect_null(camtrapReport:::.installPak(character()))
})


test_that("pak installation helper gives a clear missing-package error", {
  local_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base"
  )

  expect_error(
    camtrapReport:::.installPak("examplePackage"),
    "Package 'pak' is required",
    fixed = TRUE
  )
})


test_that("install_All rejects invalid primary arguments", {
  expect_error(
    install_All(update = NA),
    "'update' must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    install_All(update = 1),
    "'update' must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    install_All(update = c(TRUE, FALSE)),
    "'update' must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    install_All(pkgs = 1),
    "'pkgs' must be NULL or a character vector",
    fixed = TRUE
  )

  expect_error(
    install_All(pkgs = c("methods", NA_character_)),
    "'pkgs' must be NULL or a character vector",
    fixed = TRUE
  )
})


test_that("install_All reports an empty optional inventory", {
  local_mocked_bindings(
    .getPackageList = function() character(),
    .getPackageGitHubList = function() character(),
    .package = "camtrapReport"
  )

  expect_output(
    result <- install_All(gitlab = FALSE),
    "No optional packages are configured for installation",
    fixed = TRUE
  )

  expect_null(result)
})


test_that("install_All does nothing when requested packages are installed", {
  checked_packages <- character()

  local_mocked_bindings(
    .getPackageList = function() c("methods", "stats"),
    .getPackageGitHubList = function() character(),
    .is.installed = function(n) {
      checked_packages <<- as.character(n)
      stats::setNames(rep(TRUE, length(n)), n)
    },
    .installPak = function(...) {
      fail("pak should not be called for an installed inventory")
    },
    .package = "camtrapReport"
  )

  expect_output(
    result <- install_All(
      pkgs = c(" methods ", "", "stats"),
      update = FALSE,
      gitlab = FALSE
    ),
    "All required packages have already been installed",
    fixed = TRUE
  )

  expect_null(result)
  expect_setequal(checked_packages, c("methods", "stats"))
})


test_that("install_All sends only missing CRAN packages to pak", {
  calls <- new.env(parent = emptyenv())

  local_mocked_bindings(
    .getPackageList = function() c("methods", "missingPackage"),
    .getPackageGitHubList = function() character(),
    .is.installed = function(n) {
      stats::setNames(n == "methods", n)
    },
    .installPak = function(references, ...) {
      calls$references <- references
      calls$dots <- list(...)
      invisible(NULL)
    },
    .package = "camtrapReport"
  )

  expect_null(
    install_All(
      update = FALSE,
      gitlab = FALSE,
      upgrade = TRUE
    )
  )

  expect_identical(unname(calls$references), "missingPackage")
  expect_identical(calls$dots, list(upgrade = TRUE))
})


test_that("install_All sends named GitHub and GitLab references to pak", {
  calls <- new.env(parent = emptyenv())

  local_mocked_bindings(
    .getPackageList = function() {
      c("cranPackage", "githubPackage", "gitlabPackage")
    },
    .getPackageGitHubList = function() {
      c(githubPackage = "owner/repository")
    },
    .getPackageGitLabList = function() {
      c(gitlabPackage = "group/project")
    },
    .is.installed = function(n) {
      stats::setNames(rep(FALSE, length(n)), n)
    },
    .installPak = function(references, ...) {
      calls$references <- references
      invisible(NULL)
    },
    .package = "camtrapReport"
  )

  expect_null(
    install_All(
      update = FALSE,
      github = TRUE,
      gitlab = TRUE
    )
  )

  expect_identical(
    calls$references,
    c(
      "cranPackage",
      githubPackage = "githubPackage=github::owner/repository",
      gitlabPackage = "gitlabPackage=gitlab::group/project"
    )
  )
})


test_that("remote package configuration takes precedence over CRAN", {
  references <- NULL

  local_mocked_bindings(
    .getPackageList = function() c("sharedPackage", "cranPackage"),
    .getPackageGitHubList = function() {
      c(sharedPackage = "owner/repository")
    },
    .is.installed = function(n) {
      stats::setNames(rep(FALSE, length(n)), n)
    },
    .installPak = function(x, ...) {
      references <<- x
      invisible(NULL)
    },
    .package = "camtrapReport"
  )

  expect_null(install_All(update = FALSE, gitlab = FALSE))

  expect_identical(
    references,
    c(
      "cranPackage",
      sharedPackage = "sharedPackage=github::owner/repository"
    )
  )
})


test_that("install_All rejects packages configured for two remotes", {
  local_mocked_bindings(
    .getPackageList = function() character(),
    .getPackageGitHubList = function() {
      c(sharedPackage = "owner/repository")
    },
    .getPackageGitLabList = function() {
      c(sharedPackage = "group/project")
    },
    .package = "camtrapReport"
  )

  expect_error(
    install_All(github = TRUE, gitlab = TRUE),
    "configured for both GitHub and GitLab",
    fixed = TRUE
  )
})


test_that("install_All omits disabled remote inventories", {
  installed_references <- NULL

  local_mocked_bindings(
    .getPackageList = function() "cranPackage",
    .getPackageGitHubList = function() {
      fail("disabled GitHub inventory should not be read")
    },
    .getPackageGitLabList = function() {
      fail("disabled GitLab inventory should not be read")
    },
    .is.installed = function(n) {
      stats::setNames(rep(FALSE, length(n)), n)
    },
    .installPak = function(references, ...) {
      installed_references <<- references
      invisible(NULL)
    },
    .package = "camtrapReport"
  )

  expect_null(
    install_All(
      github = FALSE,
      gitlab = FALSE
    )
  )

  expect_identical(installed_references, "cranPackage")
})


test_that("install_All update mode sends reinstall references to pak", {
  calls <- new.env(parent = emptyenv())

  local_mocked_bindings(
    .getPackageList = function() "optionalPackage",
    .getPackageGitHubList = function() {
      c(remotePackage = "owner/repository")
    },
    .detachPackage = function(n, ...) {
      calls$detached <- n
      invisible(NULL)
    },
    .installPak = function(references, ...) {
      calls$references <- references
      calls$dots <- list(...)
      invisible(NULL)
    },
    .package = "camtrapReport"
  )

  expect_null(
    install_All(
      update = TRUE,
      gitlab = FALSE,
      dependencies = TRUE
    )
  )

  expect_setequal(
    calls$detached,
    c("optionalPackage", "remotePackage")
  )

  expect_identical(
    calls$references,
    c(
      "optionalPackage?reinstall",
      remotePackage = paste0(
        "remotePackage=github::owner/repository",
        "?reinstall"
      )
    )
  )

  expect_identical(calls$dots, list(dependencies = TRUE))
})


test_that("install_All update mode protects base and recommended packages", {
  local_mocked_bindings(
    .getPackageList = function() c("methods", "stats"),
    .getPackageGitHubList = function() character(),
    .detachPackage = function(...) {
      fail("protected packages should not be detached")
    },
    .installPak = function(...) {
      fail("protected packages should not be reinstalled")
    },
    .package = "camtrapReport"
  )

  expect_output(
    result <- install_All(
      update = TRUE,
      gitlab = FALSE
    ),
    "There are no optional packages to update",
    fixed = TRUE
  )

  expect_null(result)
})
