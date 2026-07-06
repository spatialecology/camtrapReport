\name{camData}
\alias{camData}
\alias{camData,character-method}

\title{
Create a camReport object from a Camtrap DP dataset
}

\description{
Read a camera-trap dataset in Camtrap DP format and create a \code{camReport} object for data checking, ecological summaries, report customisation, and report generation.
}

\usage{
camData(data, habitat, study_area, ...)

\S4method{camData}{character}(data, habitat, study_area = NULL, ...)
}

\arguments{
  \item{data}{Character. Path to a camera-trap dataset in Camtrap DP format. The input can be a ZIP file, a JSON file, or a dataset folder containing the Camtrap DP metadata and associated tables.}

  \item{habitat}{Optional data frame containing habitat information for camera-trap locations. If omitted, the object is created without external habitat information.}

  \item{study_area}{Optional spatial boundary of the study area. This can be a path to a spatial file, such as a shapefile, a \code{SpatVector} object, or an \code{sf} object. If omitted, no study-area boundary is attached.}

  \item{...}{Additional arguments passed to internal data-reading or processing functions.}
}

\value{
A \code{camReport} reference-class object containing the processed camera-trap data, report information, report settings, derived summaries, and report sections.
}

\details{
\code{camData} is the main entry point for creating a camtrapReport workflow object. It reads a Camtrap DP dataset, processes the main camera-trap tables, derives data-status summaries, prepares report information, and attaches the default ecological and data-status report sections.

The returned \code{camReport} object can be inspected, customised, and used to generate reports with \code{report}, \code{status}, or \code{gui}.

When a habitat table is supplied, it is stored in the \code{camReport} object and can be used by report sections that summarise habitat information. When a study-area boundary is supplied, it is stored as spatial information associated with the object.

The function also prepares default filtering and grouping settings used by the package. These settings can later be reviewed or modified before report generation.
}

\seealso{
\code{report}, \code{status}, \code{gui}, \code{info}
}

\examples{
\dontrun{
# Create a camReport object from a Camtrap DP ZIP file
cm <- camData("veluwe.zip")

cm

# Create a camReport object with habitat information
habitat <- read.csv("habitat.csv")

cm <- camData(
  data = "veluwe.zip",
  habitat = habitat
)

# Create a camReport object with habitat information and a study-area boundary
cm <- camData(
  data = "veluwe.zip",
  habitat = habitat,
  study_area = "study_area.shp"
)

# Inspect report information
info(cm)

# Generate reports
status(cm, view = TRUE)
report(cm, view = TRUE)
}
}

\keyword{spatial}
\keyword{species}
\keyword{camera-trap}