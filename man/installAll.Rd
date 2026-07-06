\name{install_All}
\alias{install_All}
\alias{install_All,ANY-method}

\title{
Install packages required by camtrapReport
}

\description{
Install packages required for the full camtrapReport workflow, including packages used by optional report modules.
}

\usage{
install_All(pkgs, update, ...)
}

\arguments{
  \item{pkgs}{Optional character vector of package names to install. If \code{NULL}, the default camtrapReport package list is used.}
  \item{update}{Logical. If \code{TRUE}, packages are reinstalled or updated where possible.}
  \item{...}{Additional arguments passed to \code{install.packages}.}
}

\details{
The function identifies packages required by camtrapReport and its report modules, checks whether they are installed, and attempts to install missing packages. This can be useful before generating reports that depend on optional modules or additional packages.

Some report sections may not render correctly if their required packages are not installed.
}

\value{
Called for its side effects. The function attempts to install required packages and prints a summary of packages that were installed or could not be installed.
}

\seealso{
\code{camData}, \code{report}, \code{status}
}

\examples{
\dontrun{
install_All()
}
}
