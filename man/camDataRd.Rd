\name{camData}
\alias{camData}
\alias{camData,character-method}
\alias{camData,datapackage-method}

\title{Create camera trap data object}


\description{
The camera-trap data, read as camtraptor data package, is the input.
}

\usage{
camData(data,habitat,study_area,...)

}

\arguments{
  \item{data}{either character which is the filename of the cameratrap data (as ZIP or Json file), or a datapackage object read through camtraptor (the \code{read_camtrap_dp} function) }
  \item{habitat}{data.frame of habitat types}
  \item{study_area}{either name of a shapefile or a SpatVector object defining spatial boundary of a study site. }
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

cm <- camData("veleuw.zip",'hanitat.csv',study_area='study_area.shp')

cm

# to override data, the fields in cm can be updated:

cm$authors <- c('Elham Ebrahimi')

cm$description <- "[Boswachterij Ughelen-Hoenderlo](https://www.staatsbosbeheer.nl/uit-in-de-natuur/locaties/veluwe) is a protected area managed by Staatsbosbeheer (Dutch State Forest Service). It is located Southeast of the city of Apeldoorn, roughly along the A1 highway (*Fig. 1*). The area is a mosaic of forest, heathland and driftsand, and is part of the Natura 2000-site [Veluwe]  https://www.natura2000.nl/gebieden/gelderland/veluwe). The landscape is heavily human-modified. Anthropogenic stressors including atmospheric nitrogen pollution, forest exploitation, dessication, wildlife culling, fragmentation by infrastructure, and recreation. \n\nBoswachterij Ughelen-Hoenderlo is home to a wide variety of wild mammals, including red deer (*Cervus elaphus*), roe deer (*Capreolus capreolus*), wild boar (*Sus scrofa*) and since the late 2020s also grey wolf (*Canis lupus*). Human-wildlife conflicts that involve these mammals include damage to agriculture and forestry, collisions with traffic, and predation of livestock. An important emerging concern is disease risk. African Swine Fever has not yet reached this area."

cm$sampling <-  "Based on the [EOW camera-trap protocol](https://enetwild.com/ct-protocol-for-wild-boar), at least 40 camera traps were deployed per survey in a 1 km grid for a minimum of one month. Cameras were unbaited, ensuring unbiased data collection of random wildlife movements. This systematic deployment provides representative ecological data for the study area, enabling trend analysis and spatiotemporal comparisons. The EOW protocol also includes camera calibration procedures, allowing researchers to georeferenced pixel data for precise spatial analysis.\n\n\nAt this site, surveys were conducted using Reconyx Hyperfire 2 Covert cameras, positioned at knee height (~50 cm) and secured to trees with a belt and cable lock, or when unavailable, mounted on stakes. Each camera was programmed to capture 10 consecutive images per trigger, with no delay between activations. Additionally, time-lapse images were recorded twice daily (noon and midnight) to monitor functionality and detect battery failure. At the start of the deployment, calibration images were taken using a barred stick, placed at 10–20 positions within the camera’s field of view for spatial reference.\n\n*More details on camera trap deployments can be found in the summarized information table below (Table 1).*"

# ---- To generate the report:

cm$reportObjects()

}
}

\keyword{spatial}
\keyword{species}
\keyword{cameratrap}
