\name{camData}
\alias{camData}
\alias{camData,character-method}


\title{Read camera-trap Data in Camtrap-DP Format}


\description{
Creates a `camReport` object from a Camtrap-DP dataset.
}

\usage{
camData(data,habitat,study_area,...)

}

\arguments{
  \item{data}{either character which is the filename of the cameratrap data (as ZIP or Json file), or a datapackage object read through camtraptor (the \code{read_camtrap_dp} function) }
  \item{habitat}{Optional data frame containing habitat information.}
  \item{study_area}{Optional either name of a shapefile or a SpatVector object defining spatial boundary of a study site. }
  \item{...}{additional arguments}
  }

\details{
The records of the input data package are used to build a camReport object, containing processed camera-trap data and texts, graphs, etc. used to automate generating a report.

}

\value{
a ReferenceClass
}


\references{
ebrahimni et al. XXX



}

\author{Elham Ebrahimi \email{eebrahimi.bio@gmail.com}}


\examples{
\dontrun{

# filename of dataset: "veleuw.zip"

habitat <- read.csv('hanitat.csv')

cm <- camData("veleuw.zip",habitat,study_area='study_area.shp')

cm

}
}

\keyword{spatial}
\keyword{species}
\keyword{cameratrap}
