\name{section_names}
\alias{section_names}
\alias{section_names,ANY-method}


\title{
Return the name of available report sections or update/specify the report sections
}

\description{
This function can help to select a subset of sections for generating the report.
}
\usage{
section_names(keep,exclude)

sections(x,n)
}

\arguments{
  \item{keep}{a character vector (default = 'all') specifies which report sections should be kept in the report}
  \item{exclude}{an optional character vector to specify which sections should be excluded}
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

section_name(cm,n)

report(cm)

}

}

