\name{reportSection}
\alias{reportSection}
\alias{reportSection,character-method}

\title{
Create a report section
}

\description{
Create a report-section object that stores text and optional R code for a section of a \code{camReport} report. Report sections can be added to a report to make report generation more flexible, modular, and extensible.
}

\usage{
reportSection(name, title, parent, txt, code_setting, packages, code)
}

\arguments{
  \item{name}{Character. A unique name for the report section.}
  \item{title}{Character. The section title displayed in the report.}
  \item{parent}{Character or \code{NULL}. Name of the parent section. If \code{NULL}, the section is treated as a top-level section.}
  \item{txt}{Character or list. Text content to include in the report section.}
  \item{code_setting}{Optional R Markdown chunk options for the code chunk. For example, \code{{c(echo = FALSE, results = "asis")}}.}
  \item{packages}{Optional character vector giving the packages required by the code chunk.}
  \item{code}{Optional R code supplied inside braces, for example \code{{ plot(1:10) }}.}
}

\details{
Each report section must have a unique \code{name}. If a new section with the same name as an existing section is added to a report, the existing section is replaced.

A report section can contain text only, code only, or both text and code. When the code chunk needs access to the associated \code{camReport} object, the object can be referred to as \code{object} inside the code block.

This function is useful for creating custom report modules or extending the default camtrapReport workflow with project-specific text, tables, figures, or analyses.
}

\value{
A \code{.textSection} object.
}

\seealso{
\code{camData}, \code{report}, \code{updateReportSection}, \code{testSection}
}

\examples{
tx <- reportSection(
  name = "introduction",
  title = "Introduction",
  parent = NULL,
  txt = "This is an introduction section."
)

tx

tx2 <- reportSection(
  name = "methods",
  title = "Methods",
  parent = NULL,
  txt = "This section summarises the camera deployment design.",
  code_setting = {c(echo = FALSE, results = "asis", warning = FALSE)},
  packages = c("gt", "dplyr"),
  code = {
    object$camera_setup |>
      gt::gt() |>
      gt::tab_header(
        title = gt::md("**Camera Deployment Summary**"),
        subtitle = gt::md("Details of camera deployments per year")
      )
  }
)

tx2
}