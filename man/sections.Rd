\name{section_names}
\alias{section_names}
\alias{sections}
\alias{section_names,ANY-method}
\alias{sections,camReport-method}

\title{
Select report sections
}

\description{
Get the names of available report sections or update which sections are included in a \code{camReport} report.
}

\usage{
section_names(keep, exclude)

sections(x, n)
}

\arguments{
  \item{keep}{Optional character vector of section names to keep. The default is \code{"all"}.}
  \item{exclude}{Optional character vector of section names to exclude.}
  \item{x}{A \code{camReport} object created by \code{camData}.}
  \item{n}{Character vector giving the names of report sections to include in the report.}
}

\details{
The \code{section_names} function returns the names of available report sections. It can be used to identify valid section names before selecting sections with \code{sections}.

The \code{sections} function updates the report-section selection stored in a \code{camReport} object. Only the selected sections are included when the report is generated.
}

\value{
\code{section_names} returns a character vector of section names. \code{sections} returns the updated section names or updates the supplied \code{camReport} object.
}

\seealso{
\code{camData}, \code{report}, \code{updateReportSection}, \code{listReportSections}
}

\examples{
\dontrun{
cm <- camData("file.zip")

n <- section_names()

n

sections(cm, n)

report(cm)
}
}