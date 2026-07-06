\name{testSection}
\alias{testSection}
\alias{testSection,.textSection-method}

\title{
Test a report section
}

\description{
Render a \code{.textSection} object as a temporary HTML report to check whether the section text and code can be rendered successfully.
}

\usage{
testSection(x, object, view)
}

\arguments{
  \item{x}{A \code{.textSection} object, usually created with \code{reportSection}.}
  \item{object}{Optional \code{camReport} object created by \code{camData}. This is required when the section code needs access to report data.}
  \item{view}{Logical. If \code{TRUE}, the rendered temporary HTML file is opened after rendering.}
}

\details{
This function is mainly used to test custom report sections before they are added to a full \code{camReport} report. It checks whether the section text, R Markdown chunk settings, required packages, and R code can be rendered successfully.

The \code{object} argument is only needed when the R code in the section refers to a \code{camReport} object.
}

\value{
Invisibly returns the path to the rendered temporary HTML file.
}

\seealso{
\code{camData}, \code{reportSection}, \code{updateReportSection}, \code{report}
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