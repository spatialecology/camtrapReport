\name{updateReportSection}
\alias{updateReportSection}
\alias{listReportSections}
\alias{updateReportSection,camReport-method}
\alias{listReportSections,camReport-method}

\title{
Update report sections
}

\description{
Update the text, title, code, code-chunk settings, or required packages of a report section in a \code{camReport} object. The function \code{listReportSections} lists the available report sections and their names.
}

\usage{
updateReportSection(x, section, text, title, code, code_name,
                    code_setting, packages, append_text, append_code)

listReportSections(x)
}

\arguments{
  \item{x}{A \code{camReport} object created by \code{camData}.}
  \item{section}{Character. The name or title of the report section to update.}
  \item{text}{Optional character value used to replace or extend the text body of the selected section.}
  \item{title}{Optional character value used as a new section title.}
  \item{code}{Optional character value containing R code to replace, add, or append to the section code chunk.}
  \item{code_name}{Optional character value specifying the name of the R code chunk.}
  \item{code_setting}{Optional character value specifying chunk options for the R code chunk.}
  \item{packages}{Optional character vector giving the R packages required by the section code chunk.}
  \item{append_text}{Logical. If \code{TRUE}, the supplied text is appended to the existing section text. If \code{FALSE}, the existing text is replaced.}
  \item{append_code}{Logical. If \code{TRUE}, the supplied code is appended to the existing code chunk. If \code{FALSE}, the existing code is replaced.}
}

\details{
Report sections can be identified by their section name or section title. The function \code{listReportSections} can be used to inspect the available sections in a \code{camReport} object before updating a specific section.

This functionality is useful for customising automatically generated reports, for example by adding project-specific text, modifying figure code, or changing the packages required by a section.
}

\seealso{
\code{camData}, \code{report}, \code{info}
}

\examples{
\dontrun{
cm <- camData("data-folder")

listReportSections(cm)

updateReportSection(
  cm,
  section = "study_area",
  text = "This study was conducted in a protected forest landscape.",
  append_text = FALSE
)
}
}