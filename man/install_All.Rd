\name{install_All}
\alias{install_All}

\title{
Install packages required by camtrapReport
}

\description{
Install packages required for the full camtrapReport workflow, including packages used by optional report modules.
}

\usage{
install_All(
  pkgs = NULL,
  update = FALSE,
  include_dev = FALSE,
  include_github = TRUE,
  include_gitlab = TRUE,
  ...
)
}

\arguments{
  \item{pkgs}{Optional character vector of package names to install. If \code{NULL}, the default camtrapReport package list is used.}

  \item{update}{Logical. If \code{TRUE}, reinstall GitHub or GitLab packages and attempt to install or update CRAN packages.}

  \item{include_dev}{Logical. If \code{TRUE}, also install developer packages such as \code{testthat}, \code{pkgdown}, \code{covr}, \code{usethis}, and \code{devtools}.}

  \item{include_github}{Logical. If \code{TRUE}, install required GitHub packages.}

  \item{include_gitlab}{Logical. If \code{TRUE}, install required GitLab packages when a GitLab package list is available.}

  \item{...}{Additional arguments passed to \code{install.packages}.}
}

\value{
Invisibly returns \code{TRUE} if all required packages are available, and \code{FALSE} otherwise.
}

\details{
This function checks whether packages required by camtrapReport and its optional report modules are available. Missing CRAN packages are installed using \code{install.packages}. Additional packages from GitHub or GitLab can also be installed when requested and when the relevant package lists are available.

The function is intended as a convenience helper for installing optional workflow and module dependencies. Core package dependencies should still be declared in the package \code{DESCRIPTION} file.
}

\seealso{
\code{camData}, \code{report}, \code{status}
}

\examples{
\dontrun{
install_All()

install_All(update = TRUE)

install_All(include_dev = TRUE)
}
}