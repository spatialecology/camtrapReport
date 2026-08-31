make_test_chunk <- function(name, parent, code = "1 + 1") {
  methods::new(
    ".Rchunk",
    parent = parent,
    name = name,
    setting = "echo=FALSE",
    packages = "stats",
    code = code
  )
}

exercise_report_object_tree <- function(cm, status = FALSE) {
  add <- if (status) cm$addStatusReportObject else cm$addReportObject
  field <- if (status) "statusReportObjects" else "reportObjects"

  cm[[field]] <- list()
  root <- reportSection("root", "Root", txt = "root")
  child <- reportSection("child", "Child", parent = "root", txt = "child")
  grandchild <- reportSection(
    "grandchild",
    "Grandchild",
    parent = "child",
    txt = "grandchild"
  )

  add(root)
  add(make_test_chunk("root_chunk_1", "root"))
  add(make_test_chunk("root_chunk_2", "root"))
  add(child)
  add(make_test_chunk("child_chunk_1", "child"))
  add(make_test_chunk("child_chunk_2", "child"))
  add(grandchild)
  add(make_test_chunk("grandchild_chunk", "grandchild"))

  expect_error(
    add(reportSection("orphan", parent = "unknown")),
    "parent.*unknown"
  )

  cm[[field]]
}

test_that("camReport stores nested ecological report sections and chunks", {
  cm <- camR$new()
  tree <- exercise_report_object_tree(cm, status = FALSE)

  expect_named(tree, "root")
  expect_type(tree$root, "list")
  expect_true("child" %in% names(tree$root))
})

test_that("camReport stores nested data-status sections and chunks", {
  cm <- camR$new()
  tree <- exercise_report_object_tree(cm, status = TRUE)

  expect_named(tree, "root")
  expect_type(tree$root, "list")
  expect_true("child" %in% names(tree$root))
})

test_that("camReport grouping and counters behave consistently", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  scientific_names <- cm$get_speciesNames(all = TRUE)

  expect_error(cm$add_group("bad", "not a list"), "named list")
  expect_error(cm$add_group("bad", list(family = "Canidae")), "only items")

  cm$add_group("test_group", list(scientificName = scientific_names[1]))
  expect_identical(
    cm$get_group("test_group")$scientificName,
    scientific_names[1]
  )
  expect_error(cm$set_focus_group("unknown_group"), "not defined")

  available_group <- setdiff(names(cm$species_summary), "count")[[1]]
  cm$set_focus_group(available_group)
  group_species <- cm$get_speciesNames(available_group)
  available_groups <- setdiff(names(cm$species_summary), "count")
  multiple_groups <- available_groups[
    seq_len(min(2L, length(available_groups)))
  ]

  expect_identical(cm$setting$focus_groups, available_group)
  expect_gt(length(group_species), 0L)
  expect_gt(length(cm$get_speciesNames(multiple_groups)), 0L)
  expect_true(
    all(
      cm$get_focus_group(group_species[1]) %in%
        names(cm$species_summary)
    )
  )
  expect_named(
    cm$get_focus_group(group_species[seq_len(min(2L, length(group_species)))]),
    group_species[seq_len(min(2L, length(group_species)))]
  )
  expect_error(cm$get_speciesNames(123), "group is unknown")
  expect_error(cm$get_speciesNames("unknown_group"), "group is unknown")

  cm$recetFigTabNumber()
  expect_identical(cm$getFigureNumber(), "**Figure.1**:")
  expect_identical(cm$getFigureNumber(FALSE), 2)
  expect_identical(cm$getTableNumber(), "**Table 1**:")
  expect_identical(cm$getTableNumber(FALSE), 2)

  empty <- camR$new()
  empty$add_group("defined_group", list(scientificName = "Vulpes vulpes"))
  empty$set_focus_group("defined_group")
  expect_identical(empty$setting$focus_groups, "defined_group")
})

test_that("camReport ecological summaries work on the bundled toy dataset", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  year <- cm$years[[1]]
  species <- cm$get_speciesNames(all = TRUE)
  species <- species[!is.na(species) & nzchar(species)][1:2]

  total_richness <- cm$richness(spList = species)
  annual_richness <- cm$richness(year = year, spList = species)
  by_location <- cm$species_summary_by_location(
    year = year,
    spList = species,
    cor_matrix = FALSE
  )
  pa_correlation <- suppressWarnings(cm$species_summary_by_location(
    spList = species,
    cor_matrix = TRUE,
    PA = TRUE
  ))
  count_correlation <- suppressWarnings(cm$species_summary_by_location(
    spList = species,
    cor_matrix = TRUE,
    PA = FALSE
  ))

  expect_s3_class(total_richness, "data.frame")
  expect_true("Richness" %in% names(total_richness))
  expect_true("year" %in% names(annual_richness))
  expect_true(
    all(
      c("scientificName", "total_observations") %in%
        names(by_location)
    )
  )
  expect_true(is.null(pa_correlation) || is.matrix(pa_correlation))
  expect_true(is.null(count_correlation) || is.matrix(count_correlation))
  expect_error(cm$richness(year = 1900), "No records")
  expect_error(cm$species_summary_by_location(year = 1900), "No records")
})

test_that("camReport extraction, filtering, and show methods are callable", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  year <- cm$years[[1]]

  subset <- cm$get_data_subset(year)
  expect_named(
    subset,
    c(
      "deployments",
      "media",
      "observations",
      "locations",
      "taxonomy",
      "sequences"
    )
  )
  expect_true(all(subset$deployments$Year == year))
  expect_identical(cm$extractYears(update = TRUE), sort(cm$years))

  output <- capture.output(cm$show())
  expect_match(output[[1]], "Camera trap Object")

  empty <- camR$new()
  empty$filterKeep <- list()
  empty$filterExclude <- list()
  empty$filterCount <- numeric()
  expect_warning(empty$filter(), "No filtering condition")
})

test_that("S4 show methods print report sections and metadata", {
  section <- reportSection("shown", "Shown title", txt = "Shown text")
  cm <- camtrap_test_report()
  metadata <- info(cm, c("title", "siteName", "years"))

  section_output <- capture.output(show(section))
  info_output <- capture.output(show(metadata))

  expect_true(any(grepl("Shown title", section_output, fixed = TRUE)))
  expect_true(any(grepl("siteName", info_output, fixed = TRUE)))
  expect_true(any(grepl("years", info_output, fixed = TRUE)))
})
