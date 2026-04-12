\name{info}
\alias{info}
\alias{info<-}
\alias{info,camReport-method}
\alias{info<-,camReport,character-method}

\title{
Get or set field values in a \code{camReport} object 
}

\description{
This function facilitates to extract specific fields (e.g., title, authors, etc.) from a \code{camReport} object, or update the value of a field.
}

\usage{
\S4method{info}{camReport}(x, name)

\S4method{info}{camReport}(x, name) <- value

}

\arguments{
  \item{x}{A \code{camReport} object}
  \item{name}{character; names of the field from/to which the information is retrieved/assigned }
  \item{value}{the new value to assign to the specified field (name)}
}


\details{
The camReport object,created by the \code{camData} function, contains information extracted from the camera-trap dataset. To control some of these details go to report (e.g., title, subtitle, authors, institute, siteName, description, sampling,...), a user can check their values using the \code{info} function or update the value of a certain field.

}
\references{
Ebrahimi et al. (2025) xxx
}
\author{
Elham Ebrahimi
}



\seealso{
camData
}
\examples{


\dontrun{
cm <- camData("data-folder")

info(cm)

xi <- info(cm, name=c('title','authors'))

xi

info(cm,'title') <- 'This is NEW title...'

}

}

