\name{testSection}
\alias{testSection}
\alias{testSection,.textSection-method}

\title{
Test a report section
}

\description{
Render a \code{.textSection} object as a temporary HTML report to check whether the section text and associated R code can be rendered successfully.
}

\usage{
testSection(x, object, view)
}

\arguments{
  \item{x}{A \code{.textSection} object, usually created with \code{reportSection}.}
  \item{object}{Optional. A \code{camReport} object created with \code{camData}. This is required when the section code uses data or methods from a \code{camReport} object.}
  \item{view}{Logical. If \code{TRUE}, the rendered temporary HTML file is opened after rendering.}
}

\details{
This function is mainly intended for testing and debugging individual report sections before they are included in a full report. It can be useful when developing new report modules or when checking whether a section-specific code chunk runs correctly.

If the section contains code that depends on a \code{camReport} object, the \code{object} argument should be supplied.
}

\seealso{
\code{camData}, \code{reportSection}, \code{updateReportSection}
}

\examples{
tx <- reportSection(
  name = "introduction",
  title = "Introduction",
  parent = NULL,
  txt = "This is an introduction section.",
  code = {
    plot(1:10)
  }
)

tx

\dontrun{
testSection(tx)
}
}