\name{updateReportSection}
\alias{updateReportSection}
\alias{listReportSections}
\alias{updateReportSection,camReport-method}
\alias{listReportSections,camReport-method}

\title{
Update report sections
}

\description{
Update the content of a report section in a \code{camReport} object, or list the report sections currently attached to the object.
}

\usage{
updateReportSection(
  x,
  section,
  text,
  title,
  code,
  code_name,
  code_setting,
  packages,
  append_text,
  append_code
)

listReportSections(x)
}

\arguments{
  \item{x}{A \code{camReport} object created by \code{camData}.}
  \item{section}{Character. Name or title of the report section to update.}
  \item{text}{Optional character value used to update the text body of the report section.}
  \item{title}{Optional character value giving a new title for the report section.}
  \item{code}{Optional R code used to update or add a code chunk.}
  \item{code_name}{Optional character value giving the name of the R code chunk.}
  \item{code_setting}{Optional R Markdown chunk options for the code chunk.}
  \item{packages}{Optional character vector giving the R packages required by the code chunk.}
  \item{append_text}{Logical. If \code{TRUE}, the new text is appended to the existing section text. If \code{FALSE}, the existing text is replaced.}
  \item{append_code}{Logical. If \code{TRUE}, the new code is appended to the existing code chunk. If \code{FALSE}, the existing code is replaced.}
}

\details{
Report sections can be identified using either their name or title. The \code{listReportSections} function lists the sections attached to a \code{camReport} object and can be used to find the exact section names before updating them.

The \code{updateReportSection} function is useful for adapting the default report content to project-specific needs, including changing section titles, replacing or appending text, and adding or updating R code chunks.
}

\value{
\code{updateReportSection} returns the updated \code{camReport} object invisibly. \code{listReportSections} returns information about the report sections attached to the object.
}

\seealso{
\code{camData}, \code{report}, \code{reportSection}, \code{info}
}

\examples{
\dontrun{
cm <- camData("data-folder")

listReportSections(cm)

cm <- updateReportSection(
  cm,
  section = "introduction",
  text = "This report summarises camera-trap monitoring data for the study site."
)

report(cm, view = TRUE)
}
}