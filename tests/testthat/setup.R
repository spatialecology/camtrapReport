.camtrap_test_registry_file <- file.path(
  tempdir(),
  paste0("camtrapReport-test-module-registries-", Sys.getpid(), ".rds")
)

options(camtrapReport.module_registry_file = .camtrap_test_registry_file)
withr::defer(
  unlink(.camtrap_test_registry_file, force = TRUE),
  testthat::teardown_env()
)
