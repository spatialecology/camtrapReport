# Internal taxonomy utilities for camtrapReport
# Licence: MIT
#--------
.taxonomy_rank_value <- function(z, rank, fallback_row = NA_integer_) {
  if (!is.data.frame(z) || !"name" %in% names(z)) {
    return(NA_character_)
  }
  
  value <- NULL
  
  if ("rank" %in% names(z) && rank %in% z$rank) {
    position <- which(z$rank == rank)[1]
    value <- z$name[position]
  } else if (!is.na(fallback_row) && nrow(z) >= fallback_row) {
    value <- z$name[fallback_row]
  }
  
  if (length(value) == 0 || is.na(value[1])) {
    return(NA_character_)
  }
  
  as.character(value[1])
}

.getMissingTaxon_GBIF <- function(x) {
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
          "taxize::get_gbifid(x, rows = 1, ask = FALSE, ",
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
        'taxize::classification(.id$ids, db = "gbif")',
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
    
    w <- which(is.na(names(.x)))
    
    if (length(w) > 0) {
      .x <- .x[-w]
      x <- x[-w]
    }
    
    .class <- vapply(
      .x,
      .taxonomy_rank_value,
      character(1),
      rank = "class"
    )
    
    .order <- vapply(
      .x,
      .taxonomy_rank_value,
      character(1),
      rank = "order"
    )
    
    names(.class) <- names(.order) <- NULL
    
    data.frame(
      scientificName = x,
      class = .class,
      order = .order,
      stringsAsFactors = FALSE
    )
  } else {
    stop("The taxize package is required for GBIF taxonomic lookup.")
  }
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
      .taxonomy_rank_value,
      character(1),
      rank = "class",
      fallback_row = 3L
    )
    
    .order <- vapply(
      .x,
      .taxonomy_rank_value,
      character(1),
      rank = "order",
      fallback_row = 4L
    )
    
    names(.class) <- names(.order) <- NULL
    
    data.frame(
      scientificName = x,
      class = .class,
      order = .order,
      stringsAsFactors = FALSE
    )
  } else {
    stop("The taxize package is required for NCBI taxonomic lookup.")
  }
}
