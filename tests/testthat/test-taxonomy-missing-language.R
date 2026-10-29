missing_language_test_taxa <- function() {
  taxa <- lapply(seq_len(3L), function(i) {
    list(
      taxonID = paste0("https://example.invalid/taxon/", i),
      scientificName = c("Vulpes vulpes", "Capreolus capreolus", "Meles meles")[i],
      family = c("Canidae", "Cervidae", "Mustelidae")[i],
      order = c("Carnivora", "Artiodactyla", "Carnivora")[i],
      taxonRank = "species"
    )
  })
  taxa[[1L]]$vernacularNames <- list(eng = "Red fox")
  taxa[[2L]]$vernacularNames <- list(nld = "Ree")
  taxa
}

test_that("missing vernacular languages preserve existing taxonomy selection", {
  taxa <- missing_language_test_taxa()
  normal <- .get_Taxonomic_DF(taxa[1:2])
  for (missing in list(list(), character(), NULL)) {
    input <- taxa
    input[[3L]]$vernacularNames <- missing
    result <- .get_Taxonomic_DF(input)
    expect_identical(nrow(result), 3L)
    expect_identical(names(result), names(normal))
    expect_identical(result[1:2, , drop = FALSE], normal)
    expect_identical(result$taxonID, as.character(seq_len(3L)))
    expect_identical(result$scientificName,
                     vapply(taxa, function(x) x$scientificName, character(1)))
    expect_identical(result$family,
                     vapply(taxa, function(x) x$family, character(1)))
    expect_identical(result$order,
                     vapply(taxa, function(x) x$order, character(1)))
    expect_identical(result$taxonRank, rep("species", 3L))
    expect_identical(result$vernacularNames.eng, c("Red fox", "", ""))
  }
})

test_that("complete vernacular mappings retain their original language values", {
  taxa <- missing_language_test_taxa()[1:2]
  taxa[[1L]]$vernacularNames <- list(eng = "Red fox", nld = "Vos")
  taxa[[2L]]$vernacularNames <- list(eng = "Roe deer", nld = "Ree")
  result <- .get_Taxonomic_DF(taxa)
  expect_identical(result$vernacularNames.eng, c("Red fox", "Roe deer"))
  expect_identical(result$vernacularNames.nld, c("Vos", "Ree"))
  expect_identical(result$scientificName,
                   vapply(taxa, function(x) x$scientificName, character(1)))
  expect_identical(result$order, c("Carnivora", "Artiodactyla"))
})
