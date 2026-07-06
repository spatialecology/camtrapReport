\name{report}
\alias{report}
\alias{report,camReport-method}

\title{
Generate an ecological report
}

\description{
Generate an automated ecological report from a \code{camReport} object. The report is rendered as an HTML document using an intermediate R Markdown file created from the report sections stored in the object.
}

\usage{
report(object, filename, view, test)
}

\arguments{
  \item{object}{A \code{camReport} object created by \code{camData}.}
  \item{filename}{Optional character value giving the output filename or file path without extension. The default is \code{"report"}.}
  \item{view}{Logical. If \code{TRUE}, the generated HTML report is opened after rendering.}
  \item{test}{Logical. If \code{TRUE}, report sections are tested when report generation fails, helping identify problematic modules.}
}

\value{
Invisibly returns the path to the generated HTML report.
}

\details{
The function creates two files: an HTML report and the corresponding R Markdown file used to generate it. If \code{filename} is not specified, the files are written using the default base name \code{"report"}.

The report content is based on the report sections attached to the \code{camReport} object. These sections may contain text, tables, figures, and R code chunks.

If \code{test = TRUE}, the package attempts to identify problematic report sections when rendering fails. This can be useful during development or when adding custom report modules.
}

\seealso{
\code{camData}, \code{status}, \code{reportSection}, \code{updateReportSection}
}

\examples{
\dontrun{
cm <- camData("data-folder")

report(cm, view = TRUE)
}
}