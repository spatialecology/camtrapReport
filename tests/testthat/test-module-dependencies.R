test_that("install_all discovers dependencies in registered user modules", {
  module_dir <- copy_camtrap_module_library()
  module_file <- withr::local_tempfile(fileext = ".yml")

  writeLines(
    c(
      "---",
      'name: "dependency_test"',
      'title: "Dependency test"',
      'parent: "results"',
      'text: "A module dependency test."',
      "code: |",
      "  #| packages: mockModuleDependency",
      "  invisible(NULL)",
      "---"
    ),
    module_file
  )

  add_Module(
    module_file,
    after = "captures",
    test = FALSE,
    dir = module_dir
  )

  expect_contains(
    ct_internal(".registered_module_dirs")(),
    normalizePath(
      module_dir,
      winslash = "/",
      mustWork = TRUE
    )
  )
  expect_contains(
    ct_internal(".module_dependencies")(),
    "mockModuleDependency"
  )
})

test_that("package references replace matching discovered package names", {
  references <- ct_internal(".resolve_module_package_references")(
    c("dplyr", "mockModuleDependency"),
    package_references = c(
      mockModuleDependency = "owner/mockModuleDependency",
      "extraOwner/extraPackage"
    )
  )

  expect_setequal(
    references,
    c("dplyr", "owner/mockModuleDependency", "extraOwner/extraPackage")
  )
})

test_that("install_all delegates installation to pak", {
  state <- new.env(parent = emptyenv())

  local_mocked_bindings(
    .module_dependencies = function() {
      c("dplyr", "mockModuleDependency")
    },
    .missing_module_packages = function(packages) packages,
    .missing_package_references = function(package_references) {
      package_references
    },
    .pak_install_module_dependencies = function(
        packages,
        lib,
        upgrade,
        ask) {
      state$packages <- packages
      state$lib <- lib
      state$upgrade <- upgrade
      state$ask <- ask
      data.frame(package = packages)
    },
    .require = function(package) identical(package, "pak"),
    .package = "camtrapReport"
  )

  result <- install_all(
    package_references = c(
      mockModuleDependency = "owner/mockModuleDependency"
    ),
    lib = "test-library",
    upgrade = TRUE,
    ask = FALSE
  )

  expect_setequal(
    state$packages,
    c("dplyr", "owner/mockModuleDependency")
  )
  expect_identical(state$lib, "test-library")
  expect_false(state$upgrade)
  expect_false(state$ask)
  expect_s3_class(result, "data.frame")
})

test_that("install_all sends only missing module dependencies to pak", {
  state <- new.env(parent = emptyenv())

  local_mocked_bindings(
    .module_dependencies = function() {
      c("dplyr", "terra", "missingModuleDependency")
    },
    .require = function(package) {
      package %in% c("dplyr", "terra", "pak")
    },
    .pak_install_module_dependencies = function(
        packages,
        lib,
        upgrade,
        ask) {
      state$packages <- packages
      state$upgrade <- upgrade
      invisible(packages)
    },
    .package = "camtrapReport"
  )

  install_all(upgrade = TRUE, ask = FALSE)

  expect_identical(state$packages, "missingModuleDependency")
  expect_false(state$upgrade)
})

test_that("install_all does not reinstall an installed named reference", {
  local_mocked_bindings(
    .module_dependencies = function() "terra",
    .require = function(package) package %in% c("terra", "pak"),
    .pak_install_module_dependencies = function(...) {
      fail("pak should not be called when all dependencies are installed")
    },
    .package = "camtrapReport"
  )

  expect_message(
    result <- install_all(
      package_references = c(terra = "rspatial/terra"),
      ask = FALSE
    ),
    "already installed"
  )
  expect_null(result)
})

test_that("install_all validates logical arguments", {
  expect_error(
    install_all(upgrade = NA),
    "'upgrade' must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    install_all(ask = NA),
    "'ask' must be TRUE or FALSE",
    fixed = TRUE
  )
})
