# Internal taxonomy utilities for camtrapReport
# Licence: MIT
#--------

.taxonomy_scalar <- function(x) {
  if (!is.atomic(x) || length(x) != 1L || is.na(x) ||
      !nzchar(trimws(as.character(x)))) {
    return(NA_character_)
  }
  as.character(x)
}

.fill_missing_taxonomy <- function(source, lookup) {
  required <- c("scientificName", "class", "order")
  if (!is.data.frame(lookup) || !all(required %in% names(lookup))) {
    warning("GBIF enrichment returned an invalid result; source taxonomy ",
            "has been retained.", call. = FALSE)
    return(source)
  }
  for (rank in c("class", "order")) {
    if (!rank %in% names(source)) {
      source[[rank]] <- rep(NA_character_, nrow(source))
    }
  }
  for (i in seq_len(nrow(source))) {
    matched <- which(lookup$scientificName == source$scientificName[i])
    if (length(matched) != 1L) next
    for (rank in c("class", "order")) {
      current <- .taxonomy_scalar(source[[rank]][i])
      replacement <- .taxonomy_scalar(lookup[[rank]][matched])
      if (is.na(current) && !is.na(replacement)) {
        source[[rank]][i] <- replacement
      }
    }
  }
  source
}

# Aggregate third-party conditions without printing submitted species names.
.taxonomy_lookup_stage <- function(code, env) {
  state <- new.env(parent = emptyenv())
  state$problem <- FALSE

  value <- tryCatch(
    withCallingHandlers(
      .eval(code, env),
      warning = function(w) {
        state$problem <- TRUE
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) {
      state$problem <- TRUE
      NULL
    }
  )

  list(value = value, problem = state$problem)
}

.getMissingTaxon_GBIF <- function(x) {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & nzchar(trimws(x))]
  result <- data.frame(scientificName = x, class = rep(NA_character_, length(x)),
                       order = rep(NA_character_, length(x)),
                       stringsAsFactors = FALSE)
  if (!length(x)) return(result)
  if (!.require("taxize")) {
    stop("The taxize package is required for GBIF taxonomic lookup.")
  }

  incomplete <- rep(FALSE, length(x))
  for (i in seq_along(x)) {
    .name <- x[i]
    ids <- .taxonomy_lookup_stage(paste0(
      "as.data.frame(taxize::get_gbifid(.name, rows = NA, ask = FALSE, ",
      "messages = FALSE, strict = TRUE))"
    ), environment())
    .id <- ids$value
    incomplete[i] <- ids$problem
    if (!is.data.frame(.id) || !"ids" %in% names(.id) || nrow(.id) != 1L ||
        is.na(.id$ids[1]) || !grepl("^[0-9]+$", as.character(.id$ids[1]))) {
      incomplete[i] <- TRUE
      next
    }
    if ("multiple_matches" %in% names(.id) &&
        isTRUE(.id$multiple_matches[1]) &&
        !("pattern_match" %in% names(.id) && isTRUE(.id$pattern_match[1]))) {
      incomplete[i] <- TRUE
      next
    }
    classification <- .taxonomy_lookup_stage(
      'taxize::classification(.id$ids, db = "gbif")', environment()
    )
    incomplete[i] <- incomplete[i] || classification$problem
    lineage <- classification$value
    matched <- which(names(lineage) == as.character(.id$ids[1]))
    if (!is.list(lineage) || length(matched) != 1L ||
        !is.data.frame(lineage[[matched]]) ||
        !all(c("rank", "name", "id") %in% names(lineage[[matched]]))) {
      incomplete[i] <- TRUE
      next
    }
    lineage <- lineage[[matched]]
    target <- which(as.character(lineage$id) == as.character(.id$ids[1]))
    if (length(target) != 1L ||
        !identical(tolower(trimws(as.character(lineage$name[target]))),
                   tolower(trimws(.name)))) {
      incomplete[i] <- TRUE
      next
    }
    for (rank in c("class", "order")) {
      values <- unique(as.character(lineage$name[
        !is.na(lineage$rank) & tolower(lineage$rank) == rank
      ]))
      value <- .taxonomy_scalar(values)
      result[[rank]][i] <- value
      incomplete[i] <- incomplete[i] || is.na(value)
    }
  }
  if (any(incomplete)) {
    warning("GBIF enrichment was unavailable, ambiguous or incomplete for ",
            sum(incomplete), " of ", length(x),
            " queried taxa; unverified ranks remain missing and source ",
            "taxonomy is retained.", call. = FALSE)
  }
  result
}

#--------

.getMissingTaxon_NCBI <- function(x) {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]

  if (length(x) == 0) {
    return(data.frame(
      scientificName = character(),
      class = character(),
      order = character(),
      stringsAsFactors = FALSE
    ))
  }

  if (.require("taxize")) {
    .id <- try(
      as.data.frame(.eval(
        paste0(
          "taxize::get_uid(x, rows = 1, ask = FALSE, ",
          "messages = FALSE)"
        ),
        environment()
      )),
      silent = TRUE
    )

    if (inherits(.id, "try-error") || !"ids" %in% names(.id)) {
      return(data.frame(
        scientificName = x,
        class = NA_character_,
        order = NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    .x <- try(
      .eval(
        'taxize::classification(.id$ids, db = "ncbi")',
        environment()
      ),
      silent = TRUE
    )

    if (inherits(.x, "try-error")) {
      return(data.frame(
        scientificName = x,
        class = NA_character_,
        order = NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    .class <- vapply(
      .x,
      function(z) {
        if (is.data.frame(z) && "rank" %in% names(z) && "class" %in% z$rank) {
          z$name[z$rank == "class"][1]
        } else {
          NA_character_
        }
      },
      character(1)
    )

    .order <- vapply(
      .x,
      function(z) {
        if (is.data.frame(z) && "rank" %in% names(z) && "order" %in% z$rank) {
          z$name[z$rank == "order"][1]
        } else {
          NA_character_
        }
      },
      character(1)
    )

    names(.class) <- names(.order) <- NULL

    data.frame(
      scientificName = x,
      class = unlist(.class),
      order = unlist(.order),
      stringsAsFactors = FALSE
    )
  } else {
    stop("The taxize package is required for NCBI taxonomic lookup.")
  }
}
