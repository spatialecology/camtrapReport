make_report_object_test_chunk <- function(
    name,
    parent,
    code = "1 + 1"
) {
  methods::new(
    ".Rchunk",
    parent = parent,
    name = name,
    setting = "echo=FALSE",
    packages = "stats",
    code = code
  )
}


new_report_object_test_api <- function(status = FALSE) {
  cm <- camR$new()
  
  if (isTRUE(status)) {
    cm$statusReportObjects <- list()
  } else {
    cm$reportObjects <- list()
  }
  
  add <- if (isTRUE(status)) {
    cm$addStatusReportObject
  } else {
    cm$addReportObject
  }
  
  objects <- function() {
    if (isTRUE(status)) {
      cm$statusReportObjects
    } else {
      cm$reportObjects
    }
  }
  
  list(
    cm = cm,
    add = add,
    objects = objects
  )
}


test_that("root and nested report sections can be replaced", {
  for (status in c(FALSE, TRUE)) {
    api <- new_report_object_test_api(status)
    add <- api$add
    
    root <- reportSection(
      name = "root",
      title = "Original root",
      txt = "Original root text."
    )
    
    child <- reportSection(
      name = "child",
      title = "Original child",
      parent = "root",
      txt = "Original child text."
    )
    
    add(root)
    add(child)
    
    replacement_root <- reportSection(
      name = "root",
      title = "Replacement root",
      txt = "Replacement root text."
    )
    
    add(replacement_root)
    
    tree <- api$objects()
    
    expect_type(
      tree$root,
      "list"
    )
    
    expect_true(
      "child" %in% names(tree$root)
    )
    
    expect_identical(
      tree$root$root@title,
      "Replacement root"
    )
    
    expect_identical(
      tree$root$root@txt,
      "Replacement root text."
    )
    
    expect_identical(
      tree$root$root@headLevel,
      1
    )
    
    replacement_child <- reportSection(
      name = "child",
      title = "Replacement child",
      parent = "root",
      txt = "Replacement child text."
    )
    
    add(replacement_child)
    
    tree <- api$objects()
    
    expect_identical(
      tree$root$child@title,
      "Replacement child"
    )
    
    expect_identical(
      tree$root$child@txt,
      "Replacement child text."
    )
    
    expect_identical(
      tree$root$child@headLevel,
      2
    )
  }
})


test_that("third-level report sections can be added and replaced", {
  for (status in c(FALSE, TRUE)) {
    api <- new_report_object_test_api(status)
    add <- api$add
    
    add(
      reportSection(
        name = "root",
        title = "Root",
        txt = "Root text."
      )
    )
    
    add(
      reportSection(
        name = "child",
        title = "Child",
        parent = "root",
        txt = "Child text."
      )
    )
    
    add(
      reportSection(
        name = "grandchild",
        title = "Original grandchild",
        parent = "child",
        txt = "Original grandchild text."
      )
    )
    
    replacement <- reportSection(
      name = "grandchild",
      title = "Replacement grandchild",
      parent = "child",
      txt = "Replacement grandchild text."
    )
    
    add(replacement)
    
    tree <- api$objects()
    
    expect_type(
      tree$root,
      "list"
    )
    
    expect_type(
      tree$root$child,
      "list"
    )
    
    expect_true(
      "grandchild" %in% names(tree$root$child)
    )
    
    expect_identical(
      tree$root$child$grandchild@title,
      "Replacement grandchild"
    )
    
    expect_identical(
      tree$root$child$grandchild@txt,
      "Replacement grandchild text."
    )
    
    expect_identical(
      tree$root$child$grandchild@headLevel,
      3
    )
  }
})


test_that("root report chunks can be added, converted to a list, and replaced", {
  for (status in c(FALSE, TRUE)) {
    api <- new_report_object_test_api(status)
    add <- api$add
    
    add(
      reportSection(
        name = "root",
        title = "Root",
        txt = "Root text."
      )
    )
    
    add(
      make_report_object_test_chunk(
        name = "chunk_a",
        parent = "root",
        code = "first version of chunk a"
      )
    )
    
    tree <- api$objects()
    
    expect_s4_class(
      tree$root@Rchunk,
      ".Rchunk"
    )
    
    expect_identical(
      tree$root@Rchunk@code,
      "first version of chunk a"
    )
    
    add(
      make_report_object_test_chunk(
        name = "chunk_a",
        parent = "root",
        code = "replacement version of chunk a"
      )
    )
    
    tree <- api$objects()
    
    expect_s4_class(
      tree$root@Rchunk,
      ".Rchunk"
    )
    
    expect_identical(
      tree$root@Rchunk@code,
      "replacement version of chunk a"
    )
    
    add(
      make_report_object_test_chunk(
        name = "chunk_b",
        parent = "root",
        code = "first version of chunk b"
      )
    )
    
    tree <- api$objects()
    chunks <- tree$root@Rchunk
    
    expect_type(
      chunks,
      "list"
    )
    
    expect_true(
      all(c("chunk_a", "chunk_b") %in% names(chunks))
    )
    
    add(
      make_report_object_test_chunk(
        name = "chunk_b",
        parent = "root",
        code = "replacement version of chunk b"
      )
    )
    
    add(
      make_report_object_test_chunk(
        name = "chunk_c",
        parent = "root",
        code = "chunk c"
      )
    )
    
    tree <- api$objects()
    chunks <- tree$root@Rchunk
    
    expect_type(
      chunks,
      "list"
    )
    
    expect_true(
      all(
        c(
          "chunk_a",
          "chunk_b",
          "chunk_c"
        ) %in% names(chunks)
      )
    )
    
    expect_identical(
      chunks$chunk_b@code,
      "replacement version of chunk b"
    )
    
    expect_identical(
      chunks$chunk_c@code,
      "chunk c"
    )
  }
})


test_that("second-level report chunks can be added and replaced", {
  for (status in c(FALSE, TRUE)) {
    api <- new_report_object_test_api(status)
    add <- api$add
    
    add(
      reportSection(
        name = "root",
        title = "Root",
        txt = "Root text."
      )
    )
    
    add(
      reportSection(
        name = "child",
        title = "Child",
        parent = "root",
        txt = "Child text."
      )
    )
    
    add(
      make_report_object_test_chunk(
        name = "child_chunk_a",
        parent = "child",
        code = "first child chunk"
      )
    )
    
    add(
      make_report_object_test_chunk(
        name = "child_chunk_a",
        parent = "child",
        code = "replacement child chunk"
      )
    )
    
    add(
      make_report_object_test_chunk(
        name = "child_chunk_b",
        parent = "child",
        code = "second child chunk"
      )
    )
    
    add(
      make_report_object_test_chunk(
        name = "child_chunk_c",
        parent = "child",
        code = "third child chunk"
      )
    )
    
    tree <- api$objects()
    chunks <- tree$root$child@Rchunk
    
    expect_type(
      chunks,
      "list"
    )
    
    expect_true(
      all(
        c(
          "child_chunk_a",
          "child_chunk_b",
          "child_chunk_c"
        ) %in% names(chunks)
      )
    )
    
    expect_identical(
      chunks$child_chunk_a@code,
      "replacement child chunk"
    )
    
    expect_identical(
      chunks$child_chunk_b@code,
      "second child chunk"
    )
  }
})


test_that("third-level report chunks can be added and replaced", {
  for (status in c(FALSE, TRUE)) {
    api <- new_report_object_test_api(status)
    add <- api$add
    
    add(
      reportSection(
        name = "root",
        title = "Root",
        txt = "Root text."
      )
    )
    
    add(
      reportSection(
        name = "child",
        title = "Child",
        parent = "root",
        txt = "Child text."
      )
    )
    
    add(
      reportSection(
        name = "grandchild",
        title = "Grandchild",
        parent = "child",
        txt = "Grandchild text."
      )
    )
    
    add(
      make_report_object_test_chunk(
        name = "grandchild_chunk_a",
        parent = "grandchild",
        code = "first grandchild chunk"
      )
    )
    
    add(
      make_report_object_test_chunk(
        name = "grandchild_chunk_a",
        parent = "grandchild",
        code = "replacement grandchild chunk"
      )
    )
    
    add(
      make_report_object_test_chunk(
        name = "grandchild_chunk_b",
        parent = "grandchild",
        code = "second grandchild chunk"
      )
    )
    
    tree <- api$objects()
    chunks <- tree$root$child$grandchild@Rchunk
    
    expect_type(
      chunks,
      "list"
    )
    
    expect_true(
      all(
        c(
          "grandchild_chunk_a",
          "grandchild_chunk_b"
        ) %in% names(chunks)
      )
    )
    
    expect_identical(
      chunks$grandchild_chunk_a@code,
      "replacement grandchild chunk"
    )
    
    expect_identical(
      chunks$grandchild_chunk_b@code,
      "second grandchild chunk"
    )
  }
})


test_that("unsupported objects and chunks with unknown parents are ignored", {
  for (status in c(FALSE, TRUE)) {
    api <- new_report_object_test_api(status)
    add <- api$add
    
    add(
      reportSection(
        name = "root",
        title = "Root",
        txt = "Root text."
      )
    )
    
    original_tree <- api$objects()
    
    expect_silent(
      add(42)
    )
    
    expect_identical(
      api$objects(),
      original_tree
    )
    
    orphan_chunk <- make_report_object_test_chunk(
      name = "orphan_chunk",
      parent = "unknown_parent",
      code = "orphan code"
    )
    
    expect_silent(
      add(orphan_chunk)
    )
    
    tree <- api$objects()
    
    expect_named(
      tree,
      "root"
    )
    
    expect_null(
      tree$root@Rchunk
    )
  }
})
