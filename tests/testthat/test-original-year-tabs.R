year_tab_test_code <- function(file) {
  module <- rmarkdown::yaml_front_matter(system.file(
    "reportSections", file, package = "camtrapReport"
  ))
  module[grep("code", names(module), fixed = TRUE)]
}

year_tab_test_expression <- function(x) {
  if (!is.call(x) && !is.expression(x)) return(x)
  # Namespace qualification must still exercise the existing controlled fits,
  # plots and failure simulations instead of bypassing those fixture bindings.
  namespace_mocks <- list(
    dplyr = c("transmute", "mutate", "left_join"),
    iNEXT = c("iNEXT", "ggiNEXT"),
    ggplot2 = c("theme_minimal", "theme", "element_blank", "element_line",
                "element_text", "labs", "scale_color_manual",
                "scale_fill_manual", "scale_x_continuous",
                "scale_y_continuous", "coord_cartesian", "guides",
                "guide_legend")
  )
  if (is.call(x) && identical(x[[1]], as.name("::")) &&
      as.character(x[[3]]) %in% namespace_mocks[[as.character(x[[2]])]]) {
    return(x[[3]])
  }
  if (is.call(x) && identical(x[[1]], as.name("::")) &&
      as.character(x[[2]]) %in% c("leaflet", "htmltools", "tidyr", "grid")) {
    return(as.name(paste0("mock_", x[[2]], "_", x[[3]])))
  }
  for (i in seq_along(x)) x[i] <- list(year_tab_test_expression(x[[i]]))
  x
}

year_tab_test_env <- function(years, aggregate = TRUE) {
  env <- new.env(parent = asNamespace("camtrapReport"))
  calls <- new.env(parent = emptyenv())
  calls$fits <- list()
  calls$basemaps <- list()
  calls$figures <- 0L
  species <- c("Vulpes vulpes", "Sus scrofa")
  locations <- expand.grid(
    Year = years,
    locationName = paste0("Location ", seq_len(4L)),
    stringsAsFactors = FALSE
  )
  locations$longitude <- 4 + match(locations$locationName,
                                  unique(locations$locationName)) / 100
  locations$latitude <- 50 + match(locations$locationName,
                                  unique(locations$locationName)) / 100
  locations$cameraHeight <- 0.5
  locations$Habitat_Type <- "Forest"
  locations$Year_List <- as.character(locations$Year)
  locations$BaitUse_List <- "No"
  locations$Species_List <- paste(species, collapse = ", ")
  locations$CaptureMethod_List <- "motionDetection"
  locations$Total_Photos <- 12L
  locations$Classify_By_List <- "Example observer"
  locations$Setup_By_List <- "Example observer"
  density_data <- do.call(rbind, lapply(species, function(sp) {
    data.frame(locations, scientificName = sp, total_observations = 4L)
  }))
  deployments <- data.frame(
    deploymentID = paste0("deployment-", seq_len(nrow(locations))),
    year = locations$Year
  )
  observations <- do.call(rbind, lapply(species, function(sp) {
    data.frame(deploymentID = deployments$deploymentID, scientificName = sp)
  }))

  env$object <- list(
    years = years,
    setting = list(focus_groups = "focal", locationLegend = TRUE),
    data_merged = locations,
    data = list(deployments = deployments, observations = observations),
    study_area = NULL,
    reportObjectElements = list(richness_palette = c("#0072B2", "#D55E00")),
    get_speciesNames = function(...) species,
    getFigureNumber = function() {
      calls$figures <- calls$figures + 1L
      paste0("**Figure.", calls$figures, "**:")
    },
    richness = function(year = NULL, ...) {
      out <- locations
      if (!is.null(year)) out <- out[out$Year == year, , drop = FALSE]
      out$Richness <- 2L
      out$Community_Composition <- out$Species_List
      out
    },
    species_summary_by_location = function(year = NULL, cor_matrix, ...) {
      if (is.null(year) && !aggregate) return(NULL)
      if (cor_matrix) return(matrix(c(1, 0.5, 0.5, 1), nrow = 2L))
      if (is.null(year)) return(density_data)
      density_data[density_data$Year == year, , drop = FALSE]
    },
    spatial_density = function(...) matrix(1),
    add_report_basemap = function(map, bounds, ...) {
      calls$basemaps[[length(calls$basemaps) + 1L]] <- bounds
      map
    }
  )
  env$mock_leaflet_leaflet <- function(data, ...) list(data = data)
  env$mock_leaflet_colorFactor <- function(...) {
    function(x) rep("#0072B2", length(x))
  }
  for (name in c("addCircleMarkers", "addPolygons", "addLegend", "fitBounds",
                 "addRasterImage", "addLayersControl", "setView")) {
    env[[paste0("mock_leaflet_", name)]] <- function(map, ...) map
  }
  for (name in c("labelFormat", "pathOptions", "layersControlOptions")) {
    env[[paste0("mock_leaflet_", name)]] <- function(...) list()
  }
  env$mock_htmltools_htmlEscape <- as.character
  env$mock_htmltools_tagList <- function(...) list(...)
  env$mock_tidyr_expand_grid <- function(...) {
    args <- list(...)
    out <- do.call(expand.grid, c(
      rev(args), list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
    ))
    out[, names(args), drop = FALSE]
  }
  env$mock_grid_unit <- function(...) 0
  env$.require <- function(...) FALSE
  env$par <- function(...) invisible(NULL)
  env$.basic_corrplot <- function(...) invisible(NULL)
  env$.get_projected_vect <- function(x) x
  env$transmute <- dplyr::transmute
  env$mutate <- dplyr::mutate
  env$left_join <- dplyr::left_join
  env$iNEXT <- function(x, q, datatype) {
    calls$fits[[length(calls$fits) + 1L]] <- list(
      years = names(x), data = x, q = q, datatype = datatype,
      draw = stats::runif(1L)
    )
    list(iNextEst = list(size_based = data.frame(m = c(4, 8), qD.UCL = c(2, 3))))
  }
  env$ggiNEXT <- function(...) list(layers = list())
  for (name in c("theme_minimal", "theme", "element_blank", "element_line",
                 "element_text", "labs", "scale_color_manual",
                 "scale_fill_manual", "scale_x_continuous",
                 "scale_y_continuous", "coord_cartesian", "guides",
                 "guide_legend")) {
    env[[name]] <- function(...) list(layers = list())
  }
  env$`+` <- function(e1, e2) e1
  env$print <- function(x, ...) {
    cat("\n<div class='mock-module-figure'>Rendered figure</div>\n")
    invisible(x)
  }
  list(env = env, calls = calls)
}

run_year_tab_test_module <- function(file, years, aggregate = TRUE) {
  context <- year_tab_test_env(years, aggregate)
  chunks <- year_tab_test_code(file)
  output <- capture.output(for (code in chunks) {
    expr <- year_tab_test_expression(parse(text = code))
    for (statement in expr) {
      result <- withVisible(eval(statement, envir = context$env))
      if (result$visible) context$env$print(result$value)
    }
  })
  list(output = output, calls = context$calls, env = context$env)
}

year_tab_test_headings <- function(output, level = 3L) {
  marker <- paste(rep("#", level), collapse = "")
  lines <- grep(paste0("^", marker, " "), output, value = TRUE)
  trimws(sub("[[:space:]]*[{].*$", "", substring(lines, level + 2L)))
}

test_that("original modules put aggregates before descending year tabs", {
  files <- c("location.yml", "richness.yml", "co_occurrence.yml",
             "spatial_density.yml", "species_accumulation.yml")
  for (years in list(2024, c(2023, 2025, 2022))) {
    for (file in files) {
      result <- run_year_tab_test_module(file, years)
      aggregate <- if (file == "species_accumulation.yml") "Comparison" else "Total"
      expect_identical(
        year_tab_test_headings(result$output),
        c(aggregate, as.character(sort(years, decreasing = TRUE))),
        info = file
      )
      aggregate_heading <- grep("^### ", result$output, value = TRUE)[1L]
      expect_match(aggregate_heading, ".active", fixed = TRUE, info = file)
    }
  }
})

test_that("unavailable aggregates do not invent a Total analysis", {
  result <- run_year_tab_test_module(
    "co_occurrence.yml", c(2022, 2025), aggregate = FALSE
  )
  expect_identical(year_tab_test_headings(result$output), c("2025", "2022"))
  expect_false(any(grepl(".active", result$output, fixed = TRUE)))

  result <- run_year_tab_test_module("species_accumulation.yml", 2024)
  expect_identical(year_tab_test_headings(result$output), c("Comparison", "2024"))
  expect_true(any(grepl(
    "Not enough valid year-level data to compare", result$output, fixed = TRUE
  )))
  expect_length(result$calls$fits, 1L)
})

test_that("spatial density keeps species tabs nested beneath each year", {
  result <- run_year_tab_test_module("spatial_density.yml", c(2022, 2025))
  headings <- grep("^#{3,4} ", result$output, value = TRUE)
  expect_identical(
    sub("[[:space:]]*[{].*$", "", headings),
    c("### Total", "#### Sus scrofa", "#### Vulpes vulpes",
      "### 2025", "#### Sus scrofa", "#### Vulpes vulpes",
      "### 2022", "#### Sus scrofa", "#### Vulpes vulpes")
  )
  expect_length(result$calls$basemaps, 6L)
  expect_identical(result$calls$figures, 6L)
})

test_that("accumulation display changes preserve model and palette order", {
  result <- .with_preserved_seed(88L, run_year_tab_test_module(
    "species_accumulation.yml", c(2025, 2022, 2023)
  ))
  fits <- result$calls$fits
  expect_identical(
    lapply(fits, function(x) x$years),
    list(c("2022", "2023", "2025"), "2022", "2023", "2025")
  )
  expect_identical(
    vapply(fits, function(x) x$draw, numeric(1)),
    .with_preserved_seed(88L, stats::runif(4L))
  )
  expect_true(all(vapply(fits, function(x) identical(x$q, 0), logical(1))))
  expect_true(all(vapply(fits, function(x) {
    identical(x$datatype, "incidence_freq")
  }, logical(1))))
  expect_identical(names(result$env$year_cols), c("2022", "2023", "2025"))
  expect_identical(
    unname(result$env$year_cols), c("#4477AA", "#66CCEE", "#228833")
  )
})

test_that("failed annual accumulation fits stay in their own year tabs", {
  context <- year_tab_test_env(c(2022, 2025))
  original_fit <- context$env$iNEXT
  context$env$iNEXT <- function(x, q, datatype) {
    if (identical(names(x), "2022")) stop("known annual failure")
    original_fit(x, q, datatype)
  }
  output <- capture.output(eval(
    year_tab_test_expression(parse(text = year_tab_test_code(
      "species_accumulation.yml"
    )[[1L]])),
    envir = context$env
  ))
  expect_identical(year_tab_test_headings(output), c("Comparison", "2025", "2022"))
  error_line <- grep("iNEXT failed for year 2022: known annual failure",
                     output, fixed = TRUE)
  expect_length(error_line, 1L)
  expect_gt(error_line, grep("^### 2022 ", output))
})

test_that("original year tabs render with the aggregate marked active", {
  skip_if_not(rmarkdown::pandoc_available())
  skip_if_not_installed("xml2")
  files <- c("location.yml", "richness.yml", "co_occurrence.yml",
             "spatial_density.yml", "species_accumulation.yml")
  labels <- c("Camera Locations", "Richness", "Species Co-occurrence",
              "Spatial Density", "Species Accumulation Curves")
  parts <- lapply(seq_along(files), function(i) {
    result <- run_year_tab_test_module(files[i], c(2023, 2025, 2022))
    c(paste0("## ", labels[i], " {.tabset}"), result$output, "\n## End\n")
  })
  dir <- tempfile("camtrapReport-year-tabs-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  rmd <- file.path(dir, "year-tabs.Rmd")
  writeLines(c(
    "---", "title: Original year tabs", "output:",
    "  html_document:", "    mathjax: null", "---",
    unlist(parts, use.names = FALSE)
  ), rmd)
  html <- rmarkdown::render(rmd, quiet = TRUE, envir = new.env())
  doc <- xml2::read_html(html)
  tabsets <- xml2::xml_find_all(doc, "//div[contains(@class, 'level2 tabset')]")
  expect_length(tabsets, 5L)
  for (tabset in tabsets) {
    children <- xml2::xml_find_all(tabset, "./div[contains(@class, 'level3')]")
    headings <- xml2::xml_text(xml2::xml_find_all(children, "./h3"))
    expect_true(headings[1L] %in% c("Total", "Comparison"))
    expect_identical(headings[-1L], c("2025", "2023", "2022"))
    expect_match(xml2::xml_attr(children[1L], "class"), "active", fixed = TRUE)
  }
})
