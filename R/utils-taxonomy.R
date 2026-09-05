# Internal taxonomy utilities for camtrapReport
# Licence: MIT
#--------

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
    
    .class <- sapply(.x, function(z) {
      if (is.data.frame(z) && "rank" %in% names(z) && "class" %in% z$rank) {
        z$name[z$rank == "class"][1]
      } else if (is.data.frame(z) && nrow(z) >= 3) {
        z$name[3]
      } else {
        NA_character_
      }
    })
    
    .order <- sapply(.x, function(z) {
      if (is.data.frame(z) && "rank" %in% names(z) && "order" %in% z$rank) {
        z$name[z$rank == "order"][1]
      } else if (is.data.frame(z) && nrow(z) >= 4) {
        z$name[4]
      } else {
        NA_character_
      }
    })
    
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
    
    .class <- sapply(.x, function(z) {
      if (is.data.frame(z) && "rank" %in% names(z) && "class" %in% z$rank) {
        z$name[z$rank == "class"][1]
      } else {
        NA_character_
      }
    })
    
    .order <- sapply(.x, function(z) {
      if (is.data.frame(z) && "rank" %in% names(z) && "order" %in% z$rank) {
        z$name[z$rank == "order"][1]
      } else {
        NA_character_
      }
    })
    
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
