\name{report}
\alias{report}
\alias{status}
\alias{report,camReport-method}
\alias{status,camReport-method}

\title{
Generate ecological and data-status reports
}

\description{
Generate automated HTML reports from a \code{camReport} object. The \code{report} function creates an ecological report, while the \code{status} function creates a data-status report summarising data completeness, structure, and quality.
}

\usage{
report(object, filename, view, test)

status(object, filename, view)
}

\arguments{
  \item{object}{A \code{camReport} object created by \code{camData}.}
  \item{filename}{Optional character value giving the output filename or file path without extension. If omitted, the report is written to the camera-trap data folder using the default filename.}
  \item{view}{Logical. If \code{TRUE}, the generated HTML report is opened after rendering.}
  \item{test}{Logical. If \code{TRUE}, report modules are tested when ecological report generation fails, helping identify problematic modules.}
}

\details{
Both functions create an intermediate R Markdown file and render it to HTML. By default, \code{report} creates \code{report.html} and \code{report.Rmd}, while \code{status} creates \code{data_status.html} and \code{data_status.Rmd}.

The ecological report is generated from the report modules attached to the \code{camReport} object. The data-status report is generated from status modules that summarise key aspects of the dataset, including spatial, temporal, annotation, validation, and species-level information.

If \code{test = TRUE} is used with \code{report}, the package attempts to test report modules when rendering fails. This can help identify modules that cause errors during report generation.
}

\value{
Invisibly returns the path to the generated HTML report.
}

\seealso{
\code{camData}, \code{reportSection}, \code{updateReportSection}, \code{section_names}
}

\examples{
\dontrun{
cm <- camData("data-folder")

# Generate ecological report
report(cm, view = TRUE)

# Generate data-status report
status(cm, view = TRUE)
}
}