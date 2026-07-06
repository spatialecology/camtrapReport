\name{camData}
\alias{camData}
\alias{camData,character-method}

\title{
Read camera-trap data in Camtrap DP format
}

\description{
Create a \code{camReport} object from a Camtrap DP dataset.
}

\usage{
camData(data, habitat, study_area, ...)
}

\arguments{
  \item{data}{Character. Path to a Camtrap DP dataset, provided as a ZIP file or an extracted dataset directory.}
  \item{habitat}{Optional data frame containing habitat information for camera locations.}
  \item{study_area}{Optional study-area boundary, provided as a spatial file path, a \code{SpatVector}, or an \code{sf} object.}
  \item{...}{Additional arguments.}
}

\details{
The function reads the input Camtrap DP dataset and creates a \code{camReport} object. The resulting object contains processed camera-trap data, metadata, summaries, report text, report modules, and data-status information used to generate automated reports.

If habitat information is provided, it is linked to camera locations and used in habitat-related summaries. If a study-area boundary is provided, it is stored with the object and used in spatial summaries where relevant.
}

\value{
A \code{camReport} object.
}

\seealso{
\code{report}, \code{status}, \code{info}, \code{gui}
}

\examples{
\dontrun{
habitat <- read.csv("habitat.csv")

cm <- camData(
  data = "dataset.zip",
  habitat = habitat,
  study_area = "study_area.shp"
)

cm

report(cm, view = TRUE)
}
}

\keyword{spatial}
\keyword{species}
\keyword{camera-trap}
