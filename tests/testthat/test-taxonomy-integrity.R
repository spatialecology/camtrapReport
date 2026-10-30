test_that("both source-taxonomy paths preserve classes and other ranks", {
  for (vernacular in list(list(), c(eng = "Red fox"))) {
    source <- list(
      taxonID = "https://www.gbif.org/species/5219243",
      scientificName = "Vulpes vulpes", family = "Canidae",
      order = "Carnivora", class = "Mammalia", phylum = "Chordata",
      kingdom = "Animalia", taxonRank = "species",
      vernacularNames = vernacular, count = 100L
    )
    value <- .get_Taxonomic_DF(list(source))
    expect_identical(value$class, "Mammalia")
    expect_identical(value$order, source$order)
    expect_identical(value$family, source$family)
    expect_identical(value$scientificName, source$scientificName)
    expect_identical(value$taxonID, "5219243")
    expect_identical(value$phylum, "Chordata")
    expect_identical(value$kingdom, "Animalia")
    expect_false("count" %in% names(value))
  }
})

test_that("fresh public and internal defaults consistently prohibit GBIF lookup", {
  expect_identical(formals(methods::getGeneric("camData"))$enrich_taxonomy,
                   FALSE)
  method <- methods::selectMethod("camData", "character")
  expect_identical(formals(method)$enrich_taxonomy, FALSE)
  expect_identical(formals(.read_camdp)$enrich_taxonomy, FALSE)
})

test_that("import preserves complete source taxonomy and all observation tables", {
  path <- copy_camtrap_test_dataset()
  before <- .read_camdp(path)
  metadata_path <- file.path(path, "datapackage.json")
  metadata <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)
  bird_orders <- c("Accipitriformes", "Anseriformes", "Pelecaniformes",
                   "Columbiformes", "Passeriformes", "Piciformes", "Galliformes")
  for (i in seq_along(metadata$taxonomic)) {
    metadata$taxonomic[[i]]$class <- if (
      metadata$taxonomic[[i]]$order %in% bird_orders
    ) "Aves" else "Mammalia"
  }
  jsonlite::write_json(metadata, metadata_path, auto_unbox = TRUE, pretty = TRUE)
  local_mocked_bindings(
    .getMissingTaxon_GBIF = function(...) stop("Default import must be offline"),
    .package = "camtrapReport"
  )
  old_search <- search()
  value <- .read_camdp(path)
  expect_identical(search(), old_search)
  expect_identical(value$data[setdiff(names(value$data), "taxonomy")],
                   before$data[setdiff(names(before$data), "taxonomy")])
  expect_identical(value$data$taxonomy[setdiff(names(value$data$taxonomy),
                                              "class")],
                   before$data$taxonomy[setdiff(names(before$data$taxonomy),
                                               "class")])
  expect_identical(value$data$taxonomy$class,
                   vapply(metadata$taxonomic, function(taxon) taxon$class,
                          character(1)))
  expect_identical(value$data$taxonomy$class[
    value$data$taxonomy$scientificName == "Vulpes vulpes"], "Mammalia")
  expect_identical(value$data$taxonomy$class[
    value$data$taxonomy$scientificName == "Parus major"], "Aves")
  original <- .require
  local_mocked_bindings(
    .require = function(package) {
      if (identical(package, "taxize")) TRUE else original(package)
    },
    .getMissingTaxon_GBIF = function(x) {
      data.frame(scientificName = x, class = "External class",
                 order = "External order")
    }, .package = "camtrapReport"
  )
  external <- capture_report_conditions(
    .read_camdp(path, enrich_taxonomy = TRUE)
  )
  expect_identical(external$value$data, value$data)
})

test_that("external taxonomy fills missing ranks without replacing source data", {
  source <- data.frame(
    scientificName = c("Species one", "Species two", "Species three"),
    taxonID = c("1", "2", "3"), class = c("Mammalia", NA, ""),
    order = c("Carnivora", "", "Source order"),
    family = c("Canidae", "Source family", NA), stringsAsFactors = FALSE
  )
  lookup <- data.frame(
    scientificName = c("Species three", "Species two", "Species one"),
    class = c("Aves", "Mammalia", NA),
    order = c("External order", "Carnivora", "Wrong order"),
    stringsAsFactors = FALSE
  )
  value <- .fill_missing_taxonomy(source, lookup)
  expect_identical(value$class, c("Mammalia", "Mammalia", "Aves"))
  expect_identical(value$order, c("Carnivora", "Carnivora", "Source order"))
  expect_identical(value[c("scientificName", "taxonID", "family")],
                   source[c("scientificName", "taxonID", "family")])
  ambiguous <- rbind(lookup, lookup[2, ])
  expect_identical(.fill_missing_taxonomy(source, ambiguous)$class[2],
                   NA_character_)
  unmatched <- lookup
  unmatched$scientificName <- paste0("Unmatched ", seq_len(nrow(lookup)))
  expect_identical(.fill_missing_taxonomy(source, unmatched), source)
  expect_warning(value <- .fill_missing_taxonomy(source, list()),
                 "invalid result", fixed = TRUE)
  expect_identical(value, source)
  absent_order <- source[setdiff(names(source), "order")]
  value <- .fill_missing_taxonomy(absent_order, lookup)
  expect_identical(value$order, c("Wrong order", "Carnivora", "External order"))
})

test_that("GBIF ranks require a matching identifier, name and explicit rank", {
  calls <- new.env(parent = emptyenv())
  calls$names <- character()
  local_mocked_bindings(
    .require = function(...) TRUE,
    .eval = function(code, env) {
      if (grepl("get_gbifid", code, fixed = TRUE)) {
        expect_match(code, "rows = NA", fixed = TRUE)
        expect_match(code, "ask = FALSE", fixed = TRUE)
        expect_match(code, "strict = TRUE", fixed = TRUE)
        calls$names <- c(calls$names, env$.name)
        return(data.frame(ids = match(env$.name,
                                     c("Species one", "Species two"))))
      }
      # Reverse order intentionally: list positions do not identify taxa.
      list(
        `2` = data.frame(rank = c("class", "order", "species"),
                         name = c("Aves", "Passeriformes", "Species two"),
                         id = c("20", "21", "2")),
        `1` = data.frame(rank = c("class", "order", "species"),
                         name = c("Mammalia", "Carnivora", "Species one"),
                         id = c("10", "11", "1"))
      )
    }, .package = "camtrapReport"
  )
  before <- search()
  value <- .getMissingTaxon_GBIF(c("Species one", "Species two",
                                  "Species one", NA, ""))
  expect_identical(value$scientificName, c("Species one", "Species two"))
  expect_identical(value$class, c("Mammalia", "Aves"))
  expect_identical(value$order, c("Carnivora", "Passeriformes"))
  expect_identical(calls$names, c("Species one", "Species two"))
  expect_identical(search(), before)
})

test_that("partial GBIF failures retain successful matches without leaking names", {
  local_mocked_bindings(
    .require = function(...) TRUE,
    .eval = function(code, env) {
      if (grepl("get_gbifid", code, fixed = TRUE)) {
        if (identical(env$.name, "Private unavailable species")) {
          warning("Transport detail: Private unavailable species")
          stop("Network unavailable")
        }
        return(data.frame(ids = "1"))
      }
      list(`1` = data.frame(rank = c("class", "order", "species"),
                            name = c("Mammalia", "Carnivora", "Species one"),
                            id = c("10", "11", "1")))
    }, .package = "camtrapReport"
  )
  value <- capture_report_conditions(
    .getMissingTaxon_GBIF(c("Species one", "Private unavailable species"))
  )
  expect_identical(value$value$class, c("Mammalia", NA_character_))
  expect_length(value$warnings, 1L)
  expect_match(value$warnings, "incomplete for 1 of 2", fixed = TRUE)
  expect_false(grepl("Private unavailable species", value$warnings, fixed = TRUE))
  expect_length(value$messages, 0L)
})

test_that("GBIF rejects ambiguous or malformed identifier results", {
  for (identifier_result in list(NULL, data.frame(), data.frame(other = "1"),
                   data.frame(ids = c("1", "2")), data.frame(ids = NA),
                   data.frame(ids = "not-an-id"),
                   data.frame(ids = "1", multiple_matches = TRUE,
                              pattern_match = FALSE))) {
    local_mocked_bindings(
      .require = function(...) TRUE,
      .eval = function(code, env) {
        if (!grepl("get_gbifid", code, fixed = TRUE)) {
          stop("Classification must not run without an unambiguous ID")
        }
        identifier_result
      }, .package = "camtrapReport"
    )
    expect_warning(value <- .getMissingTaxon_GBIF("Species one"),
                   "incomplete for 1 of 1", fixed = TRUE)
    expect_true(all(is.na(value[c("class", "order")])))
  }
})

test_that("GBIF never invents ranks for malformed or unmatched lineages", {
  valid <- data.frame(rank = c("class", "order", "species"),
                      name = c("Mammalia", "Carnivora", "Species one"),
                      id = c("10", "11", "1"))
  wrong_name <- valid
  wrong_name$name[3] <- "Other species"
  wrong_id <- valid
  wrong_id$id[3] <- "2"
  missing_rank <- valid[c("name", "id")]
  missing_target <- valid[1:2, ]
  for (lineage in list(NULL, list(), list(`2` = valid),
                       list(`1` = wrong_name), list(`1` = wrong_id),
                       list(`1` = missing_rank), list(`1` = missing_target),
                       list(`1` = NA),
                       stats::setNames(list(valid, valid), c("1", "1")))) {
    local_mocked_bindings(
      .require = function(...) TRUE,
      .eval = function(code, env) {
        if (grepl("get_gbifid", code, fixed = TRUE)) {
          return(data.frame(ids = "1"))
        }
        lineage
      }, .package = "camtrapReport"
    )
    expect_warning(value <- .getMissingTaxon_GBIF("Species one"),
                   "incomplete for 1 of 1", fixed = TRUE)
    expect_true(all(is.na(value[c("class", "order")])))
  }
})

test_that("missing or conflicting GBIF ranks remain unavailable", {
  for (ranks in list(
    data.frame(rank = c("class", "species"),
               name = c("Mammalia", "Species one"), id = c("10", "1")),
    data.frame(rank = c("class", "class", "species"),
               name = c("Mammalia", "Aves", "Species one"),
               id = c("10", "11", "1"))
  )) {
    local_mocked_bindings(
      .require = function(...) TRUE,
      .eval = function(code, env) {
        if (grepl("get_gbifid", code, fixed = TRUE)) {
          return(data.frame(ids = "1"))
        }
        list(`1` = ranks)
      }, .package = "camtrapReport"
    )
    expect_warning(value <- .getMissingTaxon_GBIF("Species one"),
                   "incomplete for 1 of 1", fixed = TRUE)
    expect_true(is.na(value$order))
    expect_identical(value$class, if (nrow(ranks) == 2L) "Mammalia" else
                       NA_character_)
  }
})

test_that("classification transport errors leave missing optional ranks", {
  local_mocked_bindings(
    .require = function(...) TRUE,
    .eval = function(code, env) {
      if (grepl("get_gbifid", code, fixed = TRUE)) {
        return(data.frame(ids = "1"))
      }
      stop("Classification network failure")
    }, .package = "camtrapReport"
  )
  expect_warning(value <- .getMissingTaxon_GBIF("Species one"),
                 "incomplete for 1 of 1", fixed = TRUE)
  expect_true(all(is.na(value[c("class", "order")])))
})

test_that("requested enrichment without taxize preserves a usable import", {
  original <- .require
  local_mocked_bindings(
    .require = function(package) {
      if (identical(package, "taxize")) FALSE else original(package)
    },
    .getMissingTaxon_GBIF = function(...) stop("Must not query GBIF"),
    .package = "camtrapReport"
  )
  offline <- .read_camdp(camtrap_test_dataset())
  expect_warning(
    requested <- .read_camdp(camtrap_test_dataset(), enrich_taxonomy = TRUE),
    "'taxize' is not installed", fixed = TRUE
  )
  expect_identical(requested$data, offline$data)
})

test_that("cached objects are reused without lookup after a consent change", {
  directory <- tempfile("camtrapReport-taxonomy-cache-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  object <- camtrap_test_report()$copy()
  object$info$directory <- directory
  saveRDS(object, file.path(directory, "__camReport_Object.rds"))
  local_mocked_bindings(
    .read_camdp = function(...) stop("Cached input must not be reimported"),
    .getMissingTaxon_GBIF = function(...) stop("Cached input must not query GBIF"),
    .check_camdata_cache_metadata = function(...) invisible(NULL),
    .package = "camtrapReport"
  )
  cached <- camData(directory)
  consent <- camData(directory, enrich_taxonomy = TRUE)
  expect_identical(cached$data$taxonomy, object$data$taxonomy)
  expect_identical(consent$data$taxonomy, object$data$taxonomy)
  expect_identical(cached$data$observations, object$data$observations)

  state <- new.env(parent = emptyenv())
  state$consent <- NULL
  local_mocked_bindings(
    .read_camdp = function(file, enrich_taxonomy, ...) {
      state$consent <- enrich_taxonomy
      stop("Explicit rebuild probe", call. = FALSE)
    }, .package = "camtrapReport"
  )
  expect_error(suppressMessages(camData(directory, update = TRUE)),
               "Explicit rebuild probe", fixed = TRUE)
  expect_identical(state$consent, FALSE)
  expect_error(suppressMessages(camData(directory, update = TRUE,
                                        enrich_taxonomy = TRUE)),
               "Explicit rebuild probe", fixed = TRUE)
  expect_identical(state$consent, TRUE)
})
