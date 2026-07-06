\name{info}
\alias{info}
\alias{info<-}
\alias{info,camReport-method}
\alias{info<-,camReport,character-method}
\alias{info<-,camReport-method}

\title{
Get or set information in a camReport object
}

\description{
Get selected information fields from a \code{camReport} object or update the value of a field used in report generation.
}

\usage{
\S4method{info}{camReport}(x, name)

\S4method{info}{camReport}(x, name) <- value
}

\arguments{
  \item{x}{A \code{camReport} object created by \code{camData}.}
  \item{name}{Character. Name of the field to retrieve or update.}
  \item{value}{New value to assign to the specified field.}
}

\details{
A \code{camReport} object contains metadata and report information extracted from the camera-trap dataset, such as title, subtitle, authors, institute, site name, description, and acknowledgement text.

The \code{info} function can be used to inspect selected fields. The replacement form, \code{info(x, name) <- value}, can be used to update report information before generating the report.
}

\value{
\code{info} returns an object of class \code{camInfo} containing the requested fields. The replacement method returns the updated \code{camReport} object invisibly.
}

\seealso{
\code{camData}, \code{report}, \code{status}, \code{updateReportSection}
}

\examples{
\dontrun{
cm <- camData("data-folder")

info(cm)

xi <- info(cm, name = c("title", "authors"))

xi

info(cm, "title") <- "Camera-trap monitoring report"
}
}