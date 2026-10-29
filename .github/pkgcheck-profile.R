# The official pkgcheck action uses pak before checking the package. pak does
# not read DESCRIPTION's Additional_repositories field when resolving packages.
# Configure this CI session only; retain the image's existing repositories.
local({
  repositories <- getOption("repos")
  if (is.null(repositories)) repositories <- character()
  repositories <- repositories[
    !is.na(repositories) & nzchar(repositories)
  ]
  repositories[repositories == "@CRAN@"] <- "https://cloud.r-project.org"
  universe <- "https://camera-traps.r-universe.dev"
  repositories <- repositories[repositories != universe]
  if (!length(repositories)) {
    repositories <- c(CRAN = "https://cloud.r-project.org")
  }
  options(repos = c(repositories, camera_traps = universe))
})
