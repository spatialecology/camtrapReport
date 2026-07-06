\name{info}
\alias{info}
\alias{info<-}
\alias{info,camReport-method}
\alias{info<-,camReport,character-method}
\alias{info<-,camReport-method}

\title{
Get or set report information fields
}

\description{
Get or update report-level information stored in a \code{camReport} object. These fields control information shown in generated reports, such as the report title, subtitle, authors, institute, site name, and logo path.
}

\usage{
\S4method{info}{camReport}(x, name)

\S4method{info}{camReport}(x, name) <- value
}

\arguments{
  \item{x}{A \code{camReport} object created by \code{camData}.}

  \item{name}{Optional character vector. Name or names of the information fields to retrieve or update. If omitted, all available information fields are returned.}

  \item{value}{New value to assign to the selected information field.}
}

\value{
For \code{info}, a \code{camInfo} object is returned when \code{name} is omitted. This object contains the available report information fields. When \code{name} is supplied, the value of the requested field or fields is returned.

For \code{info<-}, the selected field in the \code{camReport} object is updated.
}

\details{
A \code{camReport} object contains report-level information used when generating ecological and data-status reports. These fields are either extracted from the camera-trap dataset, generated automatically, or supplied by the user.

The fields returned by \code{info(x)} may include:

\describe{
  \item{\code{title}}{Main title of the generated report.}
  \item{\code{subtitle}}{Subtitle displayed below the report title.}
  \item{\code{authors}}{Authors or contributors shown in the report.}
  \item{\code{institute}}{Institute, organisation, or project affiliation shown in the report.}
  \item{\code{siteName}}{Name of the study site or monitoring area.}
  \item{\code{logoPath}}{Optional file path to a logo used in the report.}
}

The replacement form, \code{info(x, name) <- value}, can be used to customise these fields before generating a report.
}

\seealso{
\code{camData}, \code{report}, \code{status}
}

\examples{
\dontrun{
cm <- camData("data-folder")

# Show all report information fields
info(cm)

# The returned object may contain fields such as:
# title, subtitle, authors, institute, siteName, and logoPath

# Get one field
info(cm, "title")

# Get several fields
info(cm, c("title", "subtitle", "siteName"))

# Update the report title
info(cm, "title") <- "Camera-trap monitoring report"

# Update the subtitle
info(cm, "subtitle") <- "Ecological insights from camera-trap data"

# Update the site name
info(cm, "siteName") <- "Example monitoring site"

# Update authors or contributors
info(cm, "authors") <- "Monitoring team"

# Update institute or organisation
info(cm, "institute") <- "Example institute"

# Add a logo path
info(cm, "logoPath") <- "path/to/logo.png"

# Check updated information
info(cm)

# Generate a report using the updated information
report(cm, view = TRUE)
}
}