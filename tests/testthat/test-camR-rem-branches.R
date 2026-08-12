test_that("get_REM returns existing stored results without refitting", {
  original <- camtrap_test_report()
  report <- original$copy(shallow = FALSE)
  
  candidate_species <- unique(
    as.character(
      report$observed_counts$scientificName
    )
  )
  
  candidate_groups <- vapply(
    candidate_species,
    function(species) {
      group <- report$get_focus_group(species)
      
      if (
        length(group) == 0L ||
        is.na(group[1]) ||
        !nzchar(group[1])
      ) {
        return(NA_character_)
      }
      
      as.character(group[1])
    },
    character(1)
  )
  
  valid <- which(!is.na(candidate_groups))
  
  expect_gt(
    length(valid),
    0L
  )
  
  species <- candidate_species[valid[1]]
  group <- candidate_groups[valid[1]]
  
  stored_result <- data.frame(
    scientificName = species,
    Year = 9999,
    density = 1.25,
    stringsAsFactors = FALSE
  )
  
  report$rem[[group]] <- list()
  report$rem[[group]][[species]] <- stored_result
  
  report$.any_data_for_rem <- stats::setNames(
    TRUE,
    species
  )
  
  result <- report$get_REM(species)
  
  expect_true(
    species %in% names(result)
  )
  
  expect_identical(
    result[[species]],
    stored_result
  )
})


test_that("get_REM returns existing yearly results", {
  original <- camtrap_test_report()
  report <- original$copy(shallow = FALSE)
  
  candidate_species <- unique(
    as.character(
      report$observed_counts$scientificName
    )
  )
  
  candidate_groups <- vapply(
    candidate_species,
    function(species) {
      group <- report$get_focus_group(species)
      
      if (
        length(group) == 0L ||
        is.na(group[1]) ||
        !nzchar(group[1])
      ) {
        return(NA_character_)
      }
      
      as.character(group[1])
    },
    character(1)
  )
  
  valid <- which(!is.na(candidate_groups))
  
  expect_gt(
    length(valid),
    0L
  )
  
  species <- candidate_species[valid[1]]
  group <- candidate_groups[valid[1]]
  
  expect_gt(
    length(report$years),
    0L
  )
  
  year <- report$years[1]
  result_name <- paste0(species, "_", year)
  
  stored_result <- data.frame(
    scientificName = species,
    Year = year,
    density = 2.5,
    stringsAsFactors = FALSE
  )
  
  report$rem[[group]] <- list()
  report$rem[[group]][[result_name]] <- stored_result
  
  report$.any_data_for_rem <- stats::setNames(
    TRUE,
    species
  )
  
  result <- report$get_REM(species)
  
  expect_true(
    result_name %in% names(result)
  )
  
  expect_identical(
    result[[result_name]],
    stored_result
  )
})
