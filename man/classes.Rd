\name{camReport-classes}

\docType{class}

\alias{camReport}
\alias{camReport-class}
\alias{camR}
\alias{camInfo-class}
\alias{characterORnull-class}
\alias{characterORlist-class}
\alias{characterORlistORnull-class}
\alias{listORnull-class}
\alias{data.frameORnull-class}
\alias{.Rchunk-class}
\alias{.textSection-class}
\alias{show,camReport-method}
\alias{show,camInfo-method}

\title{
camReport classes
}

\description{
Class definitions used by camtrapReport to store camera-trap data, report metadata, report sections, and intermediate report objects.
}

\section{camReport class}{
The \code{camReport} class is the main object class used by camtrapReport. Objects of this class are usually created with \code{camData}. A \code{camReport} object stores the camera-trap dataset, report metadata, processed summaries, report modules, status-report modules, and settings used for report generation.
}

\section{Main fields}{
Important fields in a \code{camReport} object include:
\describe{
  \item{\code{data}}{A list containing the camera-trap data tables, including observations, deployments, media, locations, sequences, and taxonomy.}
  \item{\code{habitat}}{Optional habitat information linked to camera locations.}
  \item{\code{study_area}}{Optional spatial object or path describing the study area.}
  \item{\code{siteName}}{Name of the study site.}
  \item{\code{title}}{Title used in the generated report.}
  \item{\code{subtitle}}{Subtitle used in the generated report.}
  \item{\code{authors}}{Author or contributor text used in the generated report.}
  \item{\code{institute}}{Institute or organisation text used in the generated report.}
  \item{\code{description}}{Study-area or project description used in the generated report.}
  \item{\code{years}}{Years included in the report summaries.}
  \item{\code{group_definition}}{Definitions of species or observation groups used in the report.}
  \item{\code{setting}}{Report settings, including selected focus groups and other configuration options.}
  \item{\code{data_status}}{Data-status summaries generated from the input dataset.}
  \item{\code{reportObjectElements}}{Report modules and related objects used to generate the ecological report.}
  \item{\code{statusReportObjects}}{Objects used to generate the data-status report.}
}
}

\section{Supporting classes}{
camtrapReport also defines supporting classes and class unions used internally, including \code{camInfo}, \code{.Rchunk}, \code{.textSection}, \code{characterORnull}, \code{characterORlist}, \code{characterORlistORnull}, \code{listORnull}, and \code{data.frameORnull}.
}

\seealso{
\code{camData}, \code{report}, \code{status}, \code{info}, \code{reportSection}
}
