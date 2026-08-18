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
  
  install_github <- function(repository) {
    state$installed <- union(
      state$installed,
      "mockGitHubPackage"
    )
    
    TRUE
  }
  
  local_mocked_bindings(
    .getPackageList = function() {
      "mockCranPackage"
    },
    .getPackageGitHubList = function() {
      c(
        mockGitHubPackage = "owner/repository"
      )
    },
    .is.installed = installed,
    install.packages = install_cran,
    .installGitHub = install_github,
    .package = "camtrapReport"
  )
  
  expect_message(
    install_All(character(), update = FALSE),
    "successfully installed"
  )
  
  expect_message(
    expect_message(
      install_All(character(), update = TRUE),
      "All requested optional packages are installed"
    ),
    "successfully reinstalled"
  )
})
