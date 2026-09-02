test_that("module tree helpers preserve parent-child ordering", {
  info <- data.frame(
    name = c("methods", "sampling", "results"),
    parent = c(".root", "methods", ".root"),
    stringsAsFactors = FALSE
  )

  expect_identical(.subtree_end(info, "methods"), 2L)
  expect_identical(.get_descendants(info, "methods"), "sampling")
  expect_identical(
    .ancestor_chain(
      "sampling",
      stats::setNames(info$parent, info$name)
    ),
    "methods"
  )

  inserted <- .insert_module_info(
    info,
    name = "effort",
    parent = "methods",
    after = "sampling"
  )
  expect_identical(inserted$name, c("methods", "sampling", "effort", "results"))

  before <- .insert_module_info(
    inserted,
    name = "locations",
    parent = "methods",
    before = "sampling"
  )
  expect_identical(before$name[2], "locations")
  expect_error(
    .insert_module_info(before, "bad", before = "methods", after = "results"),
    "only one"
  )
  expect_error(.subtree_end(info, "unknown"), "Unknown parent")
})

test_that("the public module listing supports tree and table views", {
  tree <- list_Modules(tree = TRUE)
  brief <- list_Modules(tree = FALSE, brief = TRUE, validate = TRUE)
  full <- list_Modules(tree = FALSE, brief = FALSE, include_trash = TRUE)

  expect_true(all(c("name", "parent", "level", "label") %in% names(tree)))
  expect_identical(ncol(brief), 5L)
  expect_true(is.data.frame(full) || is.list(full))
})

test_that("modules can be added, deleted, restored, and purged in a temporary library", {
  module_dir <- copy_camtrap_module_library()
  parent_file <- write_test_module(
    .register_camtrap_test_path(
      tempfile("test_parent_", fileext = ".yml")
    ),
    "test_parent",
    parent = "results"
  )
  child_file <- write_test_module(
    .register_camtrap_test_path(
      tempfile("test_child_", fileext = ".yml")
    ),
    "test_child",
    parent = "test_parent"
  )

  parent_added <- .add_Module(
    parent_file,
    after = "captures",
    dir = module_dir,
    test = FALSE
  )
  child_added <- .add_Module(
    child_file,
    dir = module_dir,
    test = FALSE
  )

  expect_true(file.exists(parent_added$file))
  expect_true(file.exists(child_added$file))
  expect_identical(child_added$module@parent, "test_parent")

  listed <- .list_Modules(dir = module_dir, validate = TRUE)
  located <- .locate_Module(
    c("test_parent", basename(child_added$file)),
    dir = module_dir
  )
  audit <- .audit_Modules(dir = module_dir, validate = TRUE)

  expect_true(all(c("test_parent", "test_child") %in% listed$name))
  expect_setequal(located$module_name, c("test_parent", "test_child"))
  expect_length(audit$in_file_not_info, 0L)
  expect_error(
    .delete_Module("test_parent", recursive = FALSE, dir = module_dir),
    "has child"
  )

  deleted <- .delete_Module(
    "test_parent",
    recursive = TRUE,
    dir = module_dir
  )
  expect_setequal(deleted$deleted, c("test_parent", "test_child"))
  expect_false(any(c("test_parent", "test_child") %in% deleted$info$name))
  expect_identical(nrow(.list_Trash(dir = module_dir)), 2L)

  restored <- .recover_Module(
    batch_id = deleted$batch_id,
    dir = module_dir,
    test = TRUE
  )
  expect_setequal(restored$recovered, c("test_parent", "test_child"))
  expect_true(all(c("test_parent", "test_child") %in% restored$info$name))
  expect_identical(nrow(.list_Trash(dir = module_dir)), 0L)

  remaining_index <- .purge_Trash(
    recovered_only = TRUE,
    dir = module_dir
  )
  expect_identical(nrow(remaining_index), 0L)
})

test_that("module inventory reports invalid and duplicate YAML files", {
  module_dir <- copy_camtrap_module_library()
  invalid <- file.path(module_dir, "invalid.yml")
  duplicate <- file.path(module_dir, "duplicate-intro.yml")
  intro <- .find_module_file(module_dir, "introduction")

  writeLines(c("---", "name: [not valid"), invalid)
  file.copy(intro, duplicate)

  inventory <- .module_inventory(
    dir = module_dir,
    include_trash = TRUE,
    validate = TRUE
  )
  listed <- .list_Modules(
    dir = module_dir,
    include_invalid = TRUE,
    validate = TRUE
  )
  validation <- .validate_module(invalid)

  expect_false(all(inventory$parse_ok))
  expect_true(any(inventory$duplicate_module_name))
  expect_true(any(listed$status == "parse_error"))
  expect_false(validation$parse_ok)
  expect_s3_class(validation, "camtrap_module_validation")
  expect_error(.module_file_map(module_dir), "Duplicate module names")

  unlink(duplicate)
  valid <- .validate_module(intro, render = "parse")
  expect_true(valid$parse_ok)
  expect_true(valid$valid_s4)
})

test_that("empty module directories get a usable module index", {
  module_dir <- .register_camtrap_test_path(
    tempfile("empty-modules-")
  )
  dir.create(module_dir)

  info <- .read_modules_info(
    module_dir,
    level0 = c("introduction", "results"),
    create_if_missing = TRUE
  )
  inventory <- .module_inventory(dir = module_dir)
  trash <- .read_trash_index(module_dir)

  expect_identical(info$name, c("introduction", "results"))
  expect_identical(nrow(inventory), 0L)
  expect_identical(nrow(trash), 0L)
  expect_error(.modules_info_path(tempdir()), "Could not find")
})
