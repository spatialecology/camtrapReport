\name{section_names}
\alias{section_names}
\alias{section_names,ANY,ANY-method}
\alias{sections,camReport,ANY-method}


\title{
Get available report section names
}

\description{
Returns the names of available report sections/modules. Users can optionally keep only selected sections or exclude selected sections.
}
\usage{
section_names(keep,exclude)

sections(x,n)
}

\arguments{
  \item{keep}{Optional character vector of section/module names to keep (default="all").}
  \item{exclude}{Optional character vector of section/module names to exclude}
  \item{x}{The `camReport` object created using the \code{camData} function}
  \item{n}{a character vector to specify the names of report sections to update which report sections should be used in the report}
  
}
\details{
The `section_names` function can help to retrieve and specify the right section names in the `sections` function

}
\references{
Ebrahimi et al. (2025) xxx
}
\author{
Elham Ebrahimi
}



\seealso{
updateReportSections
}
\examples{

\dontrun{
cm <- camData('file.zip')

n <- section_names()

n

sections(cm,n)

report(cm)

}

}

