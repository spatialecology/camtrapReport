test_that("module modifications reject the bundled read-only registry", {
  # The public API must refuse to modify the installed package registry.
  # This fails before the nonexistent YAML input can be read or copied.
  expect_error(
    add_Module("nonexistent-module.yml"),
    "Output inside an installed package or R library is not allowed",
    fixed = TRUE
  )
})

test_that("the writable registry helper requires an explicit directory", {
  expect_error(
    .writable_module_dir(dir = NULL),
    "Supply 'dir' for a copied module registry",
    fixed = TRUE
  )
})
