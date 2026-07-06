\name{listReportSections}
\alias{listReportSections}
\alias{listReportSections,camReport-method}

\title{
List report sections
}

\description{
List the report sections currently stored in a \code{camReport} object.
}

\usage{
listReportSections(x)

\S4method{listReportSections}{camReport}(x)
}

\arguments{
  \item{x}{A \code{camReport} object.}
}

\value{
A data frame describing the available report sections, including section names, titles, parent sections, and section paths.
}

\details{
This function is useful for inspecting the structure of a report before updating, removing, or customising individual sections. The returned section names can be used in functions such as \code{updateReportSection}.
}

\seealso{
\code{camData}, \code{report}, \code{updateReportSection}, \code{reportSection}
}

\examples{
\dontrun{
cm <- camData("data-folder")

listReportSections(cm)
}
}
