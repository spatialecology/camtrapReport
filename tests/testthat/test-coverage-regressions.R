make_sampling_text_report <- function(
  sampling_design = NULL,
  camera_model = NULL,
  bait_use = NULL,
  camera_height = NULL,
  capture_method = NULL,
  individual_animals = NULL,
  is_eow = FALSE
) {
  object <- camR$new()
  object$info <- list(
    is.EOW = is_eow,
    json = list(
      project = list(
        samplingDesign = sampling_design,
        captureMethod = capture_method,
        individualAnimals = individual_animals
      )
    )
  )
  object$data <- list(
    deployments = data.frame(
      cameraModel = camera_model,
      baitUse = bait_use,
      cameraHeight = camera_height,
      stringsAsFactors = FALSE
    )
  )
  object$reportTextElements <- list()
  object
}


test_that("sampling text describes supported survey configurations", {
  mixed <- make_sampling_text_report(
    sampling_design = c("simpleRandom", "targeted", "customDesign"),
    camera_model = c("A", "B", "C", "D"),
    bait_use = c(TRUE, FALSE, TRUE, FALSE),
    camera_height = c(0.5, 1, 1.5, 2),
    capture_method = c("motionDetection", "timeLapse", "audio"),
    individual_animals = c(TRUE, FALSE),
    is_eow = TRUE
  )

  .get_sampling_text(mixed)
  expect_match(mixed$reportTextElements$sampling, "EOW camera-trap protocol")
  expect_match(mixed$reportTextElements$sampling, "combines")
  expect_match(mixed$reportTextElements$sampling, "Multiple camera models")
  expect_match(mixed$reportTextElements$sampling, "mixture of baited")
  expect_match(mixed$reportTextElements$sampling, "ranging from 0.5 to 2")
  expect_match(mixed$reportTextElements$sampling, "motion detection")
  expect_match(mixed$reportTextElements$sampling, "both the identification")

  single <- make_sampling_text_report(
    sampling_design = "simpleRandom",
    camera_model = "ToyCam",
    bait_use = FALSE,
    camera_height = 0.75,
    capture_method = "activityDetection",
    individual_animals = FALSE
  )

  .get_sampling_text(single)
  expect_match(single$reportTextElements$sampling, "simple random")
  expect_match(single$reportTextElements$sampling, "ToyCam")
  expect_match(single$reportTextElements$sampling, "No bait")
  expect_match(single$reportTextElements$sampling, "0.75 m")
  expect_match(single$reportTextElements$sampling, "activity detection")
  expect_match(single$reportTextElements$sampling, "not specifically")

  paired <- make_sampling_text_report(
    sampling_design = c("experimental", "opportunistic"),
    camera_model = c("A", "B"),
    bait_use = TRUE,
    camera_height = c(0.5, 1),
    capture_method = "audio",
    individual_animals = TRUE
  )

  .get_sampling_text(paired)
  expect_match(paired$reportTextElements$sampling, "experimental and")
  expect_match(paired$reportTextElements$sampling, "Bait was used")
  expect_match(paired$reportTextElements$sampling, "audio recording")
  expect_match(paired$reportTextElements$sampling, "designed to support")

  unknown <- make_sampling_text_report(
    sampling_design = "customDesign"
  )
  unknown$data$deployments <- data.frame(dummy = 1)

  .get_sampling_text(unknown)
  expect_match(unknown$reportTextElements$sampling, "customDesign")
})


test_that("project text handles EOW, habitat, species, and source metadata", {
  object <- camR$new()
  object$info <- list(
    json = list(
      name = "Toy EOW survey",
      project = list(title = "Toy camera project"),
      sources = list(list(title = "Toy annotation source"))
    )
  )
  object$data <- list(
    deployments = data.frame(
      habitat = c("forest", "grassland", "forest"),
      stringsAsFactors = FALSE
    )
  )
  object$habitat <- data.frame(
    habitat = "wetland",
    stringsAsFactors = FALSE
  )
  object$data_status <- list(
    Species = list(
      Table = data.frame(
        scientificName = c("Vulpes vulpes", "Capreolus capreolus"),
        captures = c(5, 10),
        stringsAsFactors = FALSE
      )
    )
  )
  object$reportTextElements <- list()

  .project_info(object)
  expect_true(object$info$is.EOW)
  expect_identical(object$reportTextElements$name, "EOW")
  expect_length(object$reportTextElements$habitat_values, 3L)
  expect_match(object$reportTextElements$habitat_text, "mosaic")
  expect_identical(
    object$data_status$Species$most_observed_sp,
    c("Capreolus capreolus", "Vulpes vulpes")
  )
  expect_identical(
    object$reportTextElements$data_source,
    "Toy annotation source"
  )

  object$info$json <- list(
    name = "Toy survey",
    project = list(title = "Independent project")
  )
  object$data$deployments <- data.frame(habitat = "forest")
  object$habitat <- data.frame()
  object$data_status$Species$Table <- data.frame(unrelated = 1)

  .project_info(object)
  expect_false(object$info$is.EOW)
  expect_match(object$reportTextElements$habitat_text, "mostly forest")
  expect_identical(
    object$data_status$Species$most_observed_sp,
    character()
  )
  expect_identical(object$reportTextElements$data_source, "")
})


test_that("author text filters organizations and orders contact authors", {
  object <- camR$new()
  object$info <- list(json = list(contributors = NULL))
  expect_identical(.get_authors_text(object), "")

  object$info$json$contributors <- data.frame(
    title = c("University Team", "Ada Alpha", "Ben Beta", "Ada Alpha"),
    role = c("contact", "author", "contact", "contact"),
    stringsAsFactors = FALSE
  )
  expect_identical(.get_authors_text(object), "Ada Alpha and Ben Beta*")

  object$info$json$contributors <- list(
    list(title = "Cara Gamma", role = "author"),
    list(title = "Ada Alpha", role = "author"),
    list(title = "Ben Beta", role = "contact")
  )
  expect_identical(
    .get_authors_text(object),
    "Ada Alpha, Cara Gamma, and Ben Beta*"
  )

  object$info$json$contributors <- list(list(role = "author"))
  expect_identical(.get_authors_text(object), "")

  object$info$json$contributors <- list(list(title = character()))
  expect_identical(.get_authors_text(object), "")
})


test_that("taxonomy helpers can be tested without remote services", {
  classification <- function(database) {
    if (identical(database, "gbif")) {
      return(list(
        first = data.frame(
          rank = c("kingdom", "phylum", "class", "order"),
          name = c("Animalia", "Chordata", "Mammalia", "Carnivora")
        ),
        second = data.frame(
          name = c("Animalia", "Chordata", "Aves", "Passeriformes")
        )
      ))
    }

    list(
      first = data.frame(
        rank = c("class", "order"),
        name = c("Mammalia", "Carnivora")
      ),
      second = data.frame(
        rank = "species",
        name = "Toy species"
      )
    )
  }

  local_mocked_bindings(
    .require = function(...) TRUE,
    .eval = function(code, env) {
      if (grepl("get_gbifid|get_uid", code)) {
        return(data.frame(ids = c("1", "2")))
      }
      if (grepl('db = "gbif"', code, fixed = TRUE)) {
        return(classification("gbif"))
      }
      classification("ncbi")
    },
    .package = "camtrapReport"
  )

  gbif <- .getMissingTaxon_GBIF(c("Species one", "Species two"))
  ncbi <- .getMissingTaxon_NCBI(c("Species one", "Species two"))

  expect_identical(gbif$class, c("Mammalia", "Aves"))
  expect_identical(gbif$order, c("Carnivora", "Passeriformes"))
  expect_identical(ncbi$class, c("Mammalia", NA_character_))
  expect_identical(ncbi$order, c("Carnivora", NA_character_))
})


test_that("taxonomy helpers return safe failures from mocked lookups", {
  local_mocked_bindings(
    .require = function(...) FALSE,
    .package = "camtrapReport"
  )
  expect_error(.getMissingTaxon_GBIF("Toy species"), "taxize")
  expect_error(.getMissingTaxon_NCBI("Toy species"), "taxize")

  local_mocked_bindings(
    .require = function(...) TRUE,
    .eval = function(...) data.frame(not_ids = "missing"),
    .package = "camtrapReport"
  )
  gbif <- .getMissingTaxon_GBIF("Toy species")
  ncbi <- .getMissingTaxon_NCBI("Toy species")
  expect_true(is.na(gbif$class))
  expect_true(is.na(ncbi$order))
})


test_that("public module wrappers dispatch without changing package files", {
  calls <- new.env(parent = emptyenv())
  calls$added <- list()
  calls$deleted <- NULL
  calls$restored <- NULL
  calls$purged <- NULL
  module_info <- data.frame(
    ID = 1:2,
    name = c("introduction", "results"),
    parent = c(".root", ".root"),
    stringsAsFactors = FALSE
  )
  info_path <- tempfile("camtrapReport-modules-info-", fileext = ".csv")
  on.exit(unlink(info_path, force = TRUE), add = TRUE)

  local_mocked_bindings(
    .section_dir = function(...) tempdir(),
    .modules_info_path = function(...) info_path,
    .validate_module = function(...) {
      list(parse_ok = TRUE, valid_s4 = TRUE, messages = character())
    },
    .add_Module = function(...) {
      calls$added[[length(calls$added) + 1L]] <- list(...)
      invisible("added")
    },
    .read_modules_info = function(...) module_info,
    .insert_module_info = function(info, name, parent, ...) {
      data.frame(
        ID = seq_len(nrow(info) + 1L),
        name = c(info$name, name),
        parent = c(info$parent, parent),
        stringsAsFactors = FALSE
      )
    },
    .delete_Module = function(...) {
      calls$deleted <- list(...)
      invisible("deleted")
    },
    .recover_Module = function(...) {
      calls$restored <- list(...)
      invisible("restored")
    },
    .purge_Trash = function(...) {
      calls$purged <- list(...)
      invisible("purged")
    },
    .package = "camtrapReport"
  )

  expect_identical(add_Module("toy.yml"), "added")
  expect_identical(
    add_Module("toy.yml", test = "default", object = camR$new()),
    "added"
  )
  expect_length(calls$added, 2L)
  expect_false(calls$added[[1]]$test)
  expect_true(calls$added[[2]]$test)

  expect_silent(move_Module("results", parent = ".root"))
  expect_identical(remove_Module("results", recursive = FALSE), "deleted")
  expect_identical(restore_Module("results", test = FALSE), "restored")
  expect_identical(empty_trash(name = "results", id = "batch"), "purged")

  expect_false(calls$deleted$recursive)
  expect_false(calls$restored$test)
  expect_identical(calls$purged$batch_id, "batch")
})


test_that("module listing wrappers cover tree, table, and trash results", {
  module_table <- as.data.frame(
    stats::setNames(
      replicate(13, character(), simplify = FALSE),
      paste0("column", seq_len(13))
    ),
    stringsAsFactors = FALSE
  )
  module_table[1, ] <- as.list(rep("value", 13))
  trash <- data.frame(name = "deleted", stringsAsFactors = FALSE)

  local_mocked_bindings(
    .section_dir = function(...) tempdir(),
    .read_modules_info = function(...) {
      data.frame(name = "root", parent = ".root")
    },
    .module_tree_df = function(info) {
      data.frame(name = info$name, level = 1L)
    },
    .list_Modules = function(...) module_table,
    .list_Trash = function(...) trash,
    .package = "camtrapReport"
  )

  tree <- list_Modules()
  brief <- list_Modules(tree = FALSE)
  full <- list_Modules(
    tree = FALSE,
    brief = FALSE,
    include_trash = TRUE,
    validate = TRUE
  )

  expect_identical(tree$name, "root")
  expect_identical(ncol(brief), 5L)
  expect_named(full, c("modules", "trash"))
  expect_identical(full$trash$name, "deleted")
})


make_coverage_test_chunk <- function(name, parent, code) {
  methods::new(
    ".Rchunk",
    parent = parent,
    name = name,
    setting = "echo=FALSE",
    packages = "stats",
    code = code
  )
}


new_coverage_report_api <- function(status = FALSE) {
  object <- camR$new()
  if (status) {
    object$statusReportObjects <- list()
  } else {
    object$reportObjects <- list()
  }

  list(
    cm = object,
    add = if (status) {
      object$addStatusReportObject
    } else {
      object$addReportObject
    },
    objects = function() {
      if (status) object$statusReportObjects else object$reportObjects
    }
  )
}


exercise_duplicate_chunks <- function(status = FALSE) {
  api <- new_coverage_report_api(status)
  add <- api$add
  root_a <- reportSection("root", "Root A", txt = "A")
  root_b <- reportSection("root", "Root B", txt = "B")

  if (status) {
    api$cm$statusReportObjects <- list(root = list(root_a, root_b))
  } else {
    api$cm$reportObjects <- list(root = list(root_a, root_b))
  }

  add(make_coverage_test_chunk("first", "root", "first"))
  add(make_coverage_test_chunk("first", "root", "replacement"))
  add(make_coverage_test_chunk("second", "root", "second"))
  add(make_coverage_test_chunk("third", "root", "third"))

  chunks <- api$objects()$root[[1]]@Rchunk
  expect_type(chunks, "list")
  expect_identical(chunks$first@code, "replacement")
  expect_identical(chunks$third@code, "third")
}


test_that("duplicate section representations retain named report chunks", {
  exercise_duplicate_chunks(status = FALSE)
  exercise_duplicate_chunks(status = TRUE)
})


exercise_nested_chunks <- function(status = FALSE, deep = FALSE) {
  api <- new_coverage_report_api(status)
  root <- reportSection("root", "Root", txt = "root")
  child <- reportSection("child", "Child", parent = "root", txt = "child")

  if (deep) {
    grandchild_a <- reportSection(
      "grandchild",
      "Grandchild A",
      parent = "child",
      txt = "A"
    )
    grandchild_b <- reportSection(
      "grandchild",
      "Grandchild B",
      parent = "child",
      txt = "B"
    )
    tree <- list(
      root = list(
        root = root,
        child = list(
          child = child,
          grandchild = list(grandchild_a, grandchild_b)
        )
      )
    )
    parent <- "grandchild"
  } else {
    child_b <- reportSection(
      "child",
      "Child B",
      parent = "root",
      txt = "B"
    )
    tree <- list(
      root = list(
        root = root,
        child = list(child, child_b)
      )
    )
    parent <- "child"
  }

  if (status) {
    api$cm$statusReportObjects <- tree
  } else {
    api$cm$reportObjects <- tree
  }

  api$add(make_coverage_test_chunk("first", parent, "first"))
  api$add(make_coverage_test_chunk("first", parent, "replacement"))

  result <- api$objects()
  chunks <- if (deep) {
    result$root$child$grandchild[[1]]@Rchunk
  } else {
    result$root$child[[1]]@Rchunk
  }

  expect_s4_class(chunks, ".Rchunk")
  expect_identical(chunks@code, "replacement")
}


test_that("nested duplicate sections retain replacement report chunks", {
  for (status in c(FALSE, TRUE)) {
    exercise_nested_chunks(status, deep = FALSE)
    exercise_nested_chunks(status, deep = TRUE)
  }
})
