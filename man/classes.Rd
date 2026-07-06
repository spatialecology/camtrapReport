\name{camReport-classes}
\docType{class}

\alias{camReport-classes}
\alias{camReport}
\alias{camR}
\alias{camReport-class}
\alias{camInfo-class}

\alias{listORcharacter-class}
\alias{listORnull-class}
\alias{characterORlist-class}
\alias{characterORnull-class}
\alias{characterORlistORnull-class}
\alias{data.frameORnull-class}

\alias{show,camReport-method}
\alias{show,camInfo-method}

\title{
camReport classes
}

\description{
Classes used by camtrapReport to store camera-trap data, report information, report sections, settings, and generated outputs.
}

\details{
The main object used by the package is a \code{camReport} object, usually created with \code{camData}. This object stores the camera-trap dataset together with derived summaries, report settings, report information, and report sections used for generating ecological and data-status reports.

A \code{camInfo} object stores report-level information such as the report title, subtitle, authors, institute, site name, and optional logo path. These fields can be inspected and updated using \code{info}.

Several internal class unions are also used to allow flexible inputs, for example character values, lists, data frames, or \code{NULL} values in selected fields.
}

\section{Main camReport fields}{
A \code{camReport} object may contain fields such as:

\describe{
  \item{\code{title}}{Main title of the report.}
  \item{\code{subtitle}}{Subtitle displayed in the generated report.}
  \item{\code{data}}{Camera-trap data stored in Camtrap DP-like structure, including deployments, locations, media, observations, and related tables.}
  \item{\code{info}}{Report-level information, usually represented as a \code{camInfo} object.}
  \item{\code{setting}}{Report and analysis settings, such as selected species groups, years, and report options.}
  \item{\code{reportObjects}}{List of report sections used to generate the ecological report.}
  \item{\code{statusReportObjects}}{List of report sections used to generate the data-status report.}
}
}

\section{camInfo fields}{
A \code{camInfo} object may contain report metadata such as:

\describe{
  \item{\code{title}}{Main report title.}
  \item{\code{subtitle}}{Report subtitle.}
  \item{\code{authors}}{Authors or contributors shown in the report.}
  \item{\code{institute}}{Institute, organisation, or project affiliation shown in the report.}
  \item{\code{siteName}}{Name of the study site or monitoring area.}
  \item{\code{logoPath}}{Optional path to a logo file used in the report.}
}
}

\section{Internal helper classes}{
The package also defines internal helper classes and class unions used for flexible report-section and metadata handling, including:

\describe{
  \item{\code{characterORnull}}{Allows either a character value or \code{NULL}.}
  \item{\code{characterORlist}}{Allows either a character value or a list.}
  \item{\code{characterORlistORnull}}{Allows a character value, a list, or \code{NULL}.}
  \item{\code{listORnull}}{Allows either a list or \code{NULL}.}
  \item{\code{data.frameORnull}}{Allows either a data frame or \code{NULL}.}
}
}

\seealso{
\code{camData}, \code{info}, \code{report}, \code{status}, \code{reportSection}
}

\examples{
\dontrun{
cm <- camData("data-folder")

# Show a summary of the camReport object
cm

# Inspect report-level information
info(cm)

# Update report information
info(cm, "title") <- "Camera-trap monitoring report"

# Generate reports
report(cm, view = TRUE)
status(cm, view = TRUE)
}
}