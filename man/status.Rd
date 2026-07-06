\name{status}
\alias{status}
\alias{status,camReport-method}

\title{
Generate a data-status report
}

\description{
Generate an automated data-status report from a \code{camReport} object. The report summarises key information about data completeness, spatial and temporal coverage, annotation quality, and validation status.
}

\usage{
status(object, filename, view)

\S4method{status}{camReport}(object, filename = "data_status", view = TRUE)
}

\arguments{
  \item{object}{A \code{camReport} object.}
  \item{filename}{Character. Output filename or file path without extension. The default is \code{"data_status"}.}
  \item{view}{Logical. If \code{TRUE}, the generated HTML report is opened after rendering.}
}

\value{
Invisibly returns the path to the generated HTML report.
}

\details{
The data-status report provides an overview of the structure and quality of a camera-trap dataset. It is intended to help users inspect whether the dataset contains the required information for downstream report generation and analysis.
}

\seealso{
\code{camData}, \code{report}
}

\examples{
\dontrun{
cm <- camData("data-folder")

status(cm, view = TRUE)
}
}