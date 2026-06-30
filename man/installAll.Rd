\name{install_All}
\alias{install_All}
\alias{install_All,ANY-method}


\title{Install packages required by camtrapReport}


\description{
Installs packages required for the full camtrapReport workflow, including packages used by optional report modules.
}

\usage{
install_All(pkgs,update,...)

}

\arguments{
  \item{pkgs}{Optional character vector of package names to install. If `NULL`, the default camtrapReport package list is used.}
  \item{update}{Logical. If `TRUE`, reinstall GitHub/GitLab packages and attempt to install or update CRAN packages.}
  \item{...}{Additional arguments passed to `install.packages()`}
  }

\details{
The function extracts the list of packages specified in the modules and make sure they are installed, otherwise, some report sections may be excluded from the report.

}



\references{
ebrahimni et al. (under-review) camtrapReport: an R package for...

}

\author{Elham Ebrahimi \email{eebrahimi.bio@gmail.com}}


\examples{
\dontrun{

install_All()

}
}

