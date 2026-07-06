\docType{package}
\name{camtrapReport-package}
\alias{camtrapReport}
\alias{camtrapReport-package}

\title{
camtrapReport: Automated reports for camera-trap datasets
}

\description{
camtrapReport provides tools for generating automated reports from camera-trap datasets. The package supports data-status checks, ecological summaries, report customisation, and interactive report generation through a graphical user interface.
}

\details{
The package is designed to help users inspect, summarise, and report camera-trap data in a reproducible way. It works with \code{camReport} objects created by \code{camData} and can generate HTML reports describing data completeness, sampling design, species detections, temporal patterns, spatial summaries, and other camera-trap monitoring outputs.

Main user-facing functions include:

\describe{
  \item{\code{camData}}{Create a \code{camReport} object from a camera-trap dataset.}
  \item{\code{status}}{Generate a data-status report.}
  \item{\code{report}}{Generate an ecological report.}
  \item{\code{gui}}{Open an interactive graphical user interface for report customisation and generation.}
  \item{\code{info}}{Inspect or update report-level information.}
  \item{\code{reportSection}}{Create custom report sections.}
}
}

\seealso{
Useful links:
\itemize{
  \item \url{https://spatialecology.github.io/camtrapReport/}
  \item \url{https://github.com/spatialecology/camtrapReport}
  \item Report bugs at \url{https://github.com/spatialecology/camtrapReport/issues}
}
}

\keyword{package}
