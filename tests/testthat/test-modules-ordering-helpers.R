test_that("norm_parent standardizes root parent values", {
  norm_parent <- camtrapReport:::.norm_parent
  
  expect_identical(norm_parent(NULL), ".root")
  expect_identical(norm_parent(character()), ".root")
  expect_identical(norm_parent(NA_character_), ".root")
  expect_identical(norm_parent(""), ".root")
  expect_identical(norm_parent("   "), ".root")
  expect_identical(norm_parent("root"), ".root")
  expect_identical(norm_parent("ROOT"), ".root")
  expect_identical(norm_parent(".root"), ".root")
  expect_identical(norm_parent(" methods "), "methods")
})


test_that("empty_info returns the expected empty structure", {
  result <- camtrapReport:::.empty_info()
  
  expect_s3_class(result, "data.frame")
  
  expect_named(
    result,
    c("ID", "name", "parent")
  )
  
  expect_identical(nrow(result), 0L)
  expect_type(result$ID, "integer")
  expect_type(result$name, "character")
  expect_type(result$parent, "character")
})


test_that("resequence_info handles NULL and empty inputs", {
  resequence_info <- camtrapReport:::.resequence_info
  
  null_result <- resequence_info(NULL)
  
  empty_result <- resequence_info(
    data.frame(
      name = character(),
      parent = character(),
      stringsAsFactors = FALSE
    )
  )
  
  expect_identical(
    null_result,
    camtrapReport:::.empty_info()
  )
  
  expect_identical(
    empty_result,
    camtrapReport:::.empty_info()
  )
})


test_that("resequence_info validates required columns", {
  expect_error(
    camtrapReport:::.resequence_info(
      data.frame(
        name = "methods",
        stringsAsFactors = FALSE
      )
    ),
    "must contain columns",
    fixed = TRUE
  )
  
  expect_error(
    camtrapReport:::.resequence_info(
      data.frame(
        parent = ".root",
        stringsAsFactors = FALSE
      )
    ),
    "must contain columns",
    fixed = TRUE
  )
})


test_that("resequence_info trims, normalizes, removes blanks and duplicates", {
  input <- data.frame(
    ID = c(10L, 20L, 30L, 40L, 50L),
    name = c(
      " introduction ",
      "methods",
      "",
      "methods",
      "results"
    ),
    parent = c(
      "root",
      " .root ",
      "methods",
      "introduction",
      " methods "
    ),
    extra = letters[1:5],
    stringsAsFactors = FALSE
  )
  
  result <- camtrapReport:::.resequence_info(input)
  
  expected <- data.frame(
    ID = 1:3,
    name = c(
      "introduction",
      "methods",
      "results"
    ),
    parent = c(
      ".root",
      ".root",
      "methods"
    ),
    stringsAsFactors = FALSE
  )
  
  rownames(result) <- NULL
  rownames(expected) <- NULL
  
  expect_identical(result, expected)
})


test_that("ancestor_chain returns ancestors in nearest-first order", {
  parent_lookup <- c(
    introduction = ".root",
    methods = ".root",
    sampling = "methods",
    cameras = "sampling"
  )
  
  expect_identical(
    camtrapReport:::.ancestor_chain(
      "cameras",
      parent_lookup
    ),
    c("sampling", "methods")
  )
  
  expect_identical(
    camtrapReport:::.ancestor_chain(
      "sampling",
      parent_lookup
    ),
    "methods"
  )
  
  expect_identical(
    camtrapReport:::.ancestor_chain(
      "methods",
      parent_lookup
    ),
    character()
  )
})


test_that("subtree_end identifies complete nested subtrees", {
  info <- data.frame(
    ID = 1:6,
    name = c(
      "introduction",
      "methods",
      "sampling",
      "cameras",
      "analysis",
      "results"
    ),
    parent = c(
      ".root",
      ".root",
      "methods",
      "sampling",
      "methods",
      ".root"
    ),
    stringsAsFactors = FALSE
  )
  
  expect_identical(
    camtrapReport:::.subtree_end(
      info,
      ".root"
    ),
    6L
  )
  
  expect_identical(
    camtrapReport:::.subtree_end(
      info,
      "methods"
    ),
    5L
  )
  
  expect_identical(
    camtrapReport:::.subtree_end(
      info,
      "sampling"
    ),
    4L
  )
  
  expect_identical(
    camtrapReport:::.subtree_end(
      info,
      "cameras"
    ),
    4L
  )
  
  expect_error(
    camtrapReport:::.subtree_end(
      info,
      "unknown"
    ),
    "Unknown parent",
    fixed = TRUE
  )
})


test_that("subtree_end handles empty information", {
  expect_identical(
    camtrapReport:::.subtree_end(
      camtrapReport:::.empty_info(),
      ".root"
    ),
    0L
  )
})


test_that("insert_row supports beginning, middle, and end positions", {
  input <- data.frame(
    name = c("a", "c"),
    parent = c(".root", ".root"),
    stringsAsFactors = FALSE
  )
  
  row_b <- data.frame(
    name = "b",
    parent = ".root",
    stringsAsFactors = FALSE
  )
  
  at_start <- camtrapReport:::.insert_row(
    input,
    row_b,
    1L
  )
  
  in_middle <- camtrapReport:::.insert_row(
    input,
    row_b,
    2L
  )
  
  at_end <- camtrapReport:::.insert_row(
    input,
    row_b,
    99L
  )
  
  expect_identical(
    at_start$name,
    c("b", "a", "c")
  )
  
  expect_identical(
    in_middle$name,
    c("a", "b", "c")
  )
  
  expect_identical(
    at_end$name,
    c("a", "c", "b")
  )
})


test_that("insert_row handles empty data frames", {
  empty <- data.frame(
    name = character(),
    parent = character(),
    stringsAsFactors = FALSE
  )
  
  row <- data.frame(
    name = "methods",
    parent = ".root",
    stringsAsFactors = FALSE
  )
  
  expect_identical(
    camtrapReport:::.insert_row(
      empty,
      row,
      1L
    ),
    row
  )
})


test_that("guess_root_insert_pos follows canonical root order", {
  level0 <- c(
    "introduction",
    "methods",
    "results",
    "acknowledgements",
    "appendix"
  )
  
  info <- data.frame(
    ID = 1:3,
    name = c(
      "introduction",
      "results",
      "appendix"
    ),
    parent = rep(".root", 3),
    stringsAsFactors = FALSE
  )
  
  expect_identical(
    camtrapReport:::.guess_root_insert_pos(
      info,
      "methods",
      level0
    ),
    2L
  )
  
  expect_identical(
    camtrapReport:::.guess_root_insert_pos(
      info,
      "acknowledgements",
      level0
    ),
    3L
  )
  
  expect_identical(
    camtrapReport:::.guess_root_insert_pos(
      info,
      "custom_section",
      level0
    ),
    4L
  )
  
  expect_identical(
    camtrapReport:::.guess_root_insert_pos(
      camtrapReport:::.empty_info(),
      "methods",
      level0
    ),
    1L
  )
})


test_that("insert_module_info adds the first module", {
  result <- camtrapReport:::.insert_module_info(
    info = camtrapReport:::.empty_info(),
    name = "methods",
    parent = ".root"
  )
  
  expected <- data.frame(
    ID = 1L,
    name = "methods",
    parent = ".root",
    stringsAsFactors = FALSE
  )
  
  rownames(result) <- NULL
  rownames(expected) <- NULL
  
  expect_identical(result, expected)
})


test_that("insert_module_info inserts root modules in canonical order", {
  info <- data.frame(
    ID = 1:3,
    name = c(
      "introduction",
      "results",
      "appendix"
    ),
    parent = rep(".root", 3),
    stringsAsFactors = FALSE
  )
  
  result <- camtrapReport:::.insert_module_info(
    info = info,
    name = "methods",
    parent = ".root"
  )
  
  expect_identical(
    result$name,
    c(
      "introduction",
      "methods",
      "results",
      "appendix"
    )
  )
  
  expect_identical(
    result$ID,
    1:4
  )
})


test_that("insert_module_info appends children after the parent subtree", {
  info <- data.frame(
    ID = 1:4,
    name = c(
      "methods",
      "sampling",
      "analysis",
      "results"
    ),
    parent = c(
      ".root",
      "methods",
      "methods",
      ".root"
    ),
    stringsAsFactors = FALSE
  )
  
  result <- camtrapReport:::.insert_module_info(
    info = info,
    name = "modelling",
    parent = "methods"
  )
  
  expect_identical(
    result$name,
    c(
      "methods",
      "sampling",
      "analysis",
      "modelling",
      "results"
    )
  )
  
  expect_identical(
    result$parent[result$name == "modelling"],
    "methods"
  )
})


test_that("insert_module_info supports before and after placement", {
  info <- data.frame(
    ID = 1:4,
    name = c(
      "methods",
      "sampling",
      "analysis",
      "results"
    ),
    parent = c(
      ".root",
      "methods",
      "methods",
      ".root"
    ),
    stringsAsFactors = FALSE
  )
  
  before_result <- camtrapReport:::.insert_module_info(
    info = info,
    name = "camera_setup",
    parent = "methods",
    before = "analysis"
  )
  
  after_result <- camtrapReport:::.insert_module_info(
    info = info,
    name = "camera_setup",
    parent = "methods",
    after = "sampling"
  )
  
  expect_identical(
    before_result$name,
    c(
      "methods",
      "sampling",
      "camera_setup",
      "analysis",
      "results"
    )
  )
  
  expect_identical(
    after_result$name,
    c(
      "methods",
      "sampling",
      "camera_setup",
      "analysis",
      "results"
    )
  )
})


test_that("insert_module_info permits insertion at subtree boundary", {
  info <- data.frame(
    ID = 1:4,
    name = c(
      "methods",
      "sampling",
      "results",
      "appendix"
    ),
    parent = c(
      ".root",
      "methods",
      ".root",
      ".root"
    ),
    stringsAsFactors = FALSE
  )
  
  result <- camtrapReport:::.insert_module_info(
    info = info,
    name = "analysis",
    parent = "methods",
    before = "results"
  )
  
  expect_identical(
    result$name,
    c(
      "methods",
      "sampling",
      "analysis",
      "results",
      "appendix"
    )
  )
  
  expect_identical(
    result$parent[result$name == "analysis"],
    "methods"
  )
})


test_that("insert_module_info rejects invalid requests", {
  info <- data.frame(
    ID = 1:3,
    name = c(
      "methods",
      "sampling",
      "results"
    ),
    parent = c(
      ".root",
      "methods",
      ".root"
    ),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    camtrapReport:::.insert_module_info(
      info,
      name = "",
      parent = ".root"
    ),
    "Module name is empty",
    fixed = TRUE
  )
  
  expect_error(
    camtrapReport:::.insert_module_info(
      info,
      name = "methods",
      parent = ".root"
    ),
    "already exists",
    fixed = TRUE
  )
  
  expect_error(
    camtrapReport:::.insert_module_info(
      info,
      name = "new_module",
      parent = ".root",
      before = "methods",
      after = "results"
    ),
    "Use only one",
    fixed = TRUE
  )
  
  expect_error(
    camtrapReport:::.insert_module_info(
      info,
      name = "new_module",
      parent = "unknown"
    ),
    "Parent not found",
    fixed = TRUE
  )
  
  expect_error(
    camtrapReport:::.insert_module_info(
      info,
      name = "new_module",
      parent = "methods",
      before = "unknown"
    ),
    "was not found",
    fixed = TRUE
  )
  
  expect_error(
    camtrapReport:::.insert_module_info(
      info,
      name = "new_module",
      parent = "methods",
      after = "unknown"
    ),
    "was not found",
    fixed = TRUE
  )
})


test_that("insert_module_info rejects placement beyond parent subtree", {
  info <- data.frame(
    ID = 1:4,
    name = c(
      "methods",
      "sampling",
      "results",
      "appendix"
    ),
    parent = c(
      ".root",
      "methods",
      ".root",
      ".root"
    ),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    camtrapReport:::.insert_module_info(
      info = info,
      name = "analysis",
      parent = "methods",
      after = "results"
    ),
    "outside the subtree",
    fixed = TRUE
  )
  
  expect_error(
    camtrapReport:::.insert_module_info(
      info = info,
      name = "analysis",
      parent = "methods",
      before = "appendix"
    ),
    "outside the subtree",
    fixed = TRUE
  )
})
