test_that("installer inventory helpers return configured package vectors", {
  packages <- camtrapReport:::.getPackageList()
  github <- camtrapReport:::.getPackageGitHubList()
  gitlab <- camtrapReport:::.getPackageGitLabList()
  
  expect_type(packages, "character")
  expect_true(is.null(github) || is.character(github))
  expect_true(is.null(gitlab) || is.character(gitlab) || is.list(gitlab))
  expect_true(camtrapReport:::.is.installed("methods"))
  expect_false(
    camtrapReport:::.is.installed(
      "a_package_that_does_not_exist_123"
    )
  )
  expect_true(
    camtrapReport:::.loadLib(
      list(c("methods", "stats"))
    )
  )
})

test_that("install_All reports an already satisfied mocked inventory", {
  local_mocked_bindings(
    .getPackageList = function() character(),
    .getPackageGitHubList = function() {
      stats::setNames(character(), character())
    },
    .package = "camtrapReport"
  )
  
  expect_output(
    install_All(character(), update = FALSE),
    "already been installed"
  )
})

test_that("install_All exercises missing and update paths without installing", {
  state <- new.env(parent = emptyenv())
  state$installed <- character()
  
  installed <- function(x) {
    x <- as.character(x)
    
    stats::setNames(
      x %in% state$installed,
      x
    )
  }
  
  install_cran <- function(pkgs, ...) {
    state$installed <- union(
      state$installed,
      as.character(pkgs)
    )
    
    invisible(TRUE)
  }
  
  remove_mock_packages <- function(pkgs, ...) {
    state$installed <- setdiff(
      state$installed,
      as.character(pkgs)
    )
    
    invisible(TRUE)
  }
  
  install_github <- function(repository) {
    state$installed <- union(
      state$installed,
      "mockGitHubPackage"
    )
    
    TRUE
  }
  
  local_mocked_bindings(
    .getPackageList = function() "mockCranPackage",
    .getPackageGitHubList = function() {
      c(mockGitHubPackage = "owner/repository")
    },
    .is.installed = installed,
    install.packages = install_cran,
    remove.packages = remove_mock_packages,
    .installGitHub = install_github,
    .detachPackage = function(...) invisible(NULL),
    .package = "camtrapReport"
  )
  
  expect_output(
    install_All(character(), update = FALSE),
    "successfully installed"
  )
  
  expect_output(
    install_All(character(), update = TRUE),
    "successfully reinstalled"
  )
})