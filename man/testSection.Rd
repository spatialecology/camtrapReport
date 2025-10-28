\name{testSection}
\alias{testSection}
\alias{testSection,.textSection-method}


\title{
Test a sub-section object (\code{.textSection} class) 
}

\description{
If a sub-section object is created using the \code{reportSection} function, it can be tested to see how it looks like in a report.
}
\usage{
testSection(x,object,view)
}

\arguments{
  \item{x}{The subsection object created using the \code{reportSection} function}
  \item{object}{The camReport object created using the \code{camData} function}
  \item{view}{logical (default = TRUE); specifies whether the renderred html file would be displayed!}
  
}
\details{
The object (camReport) is only required if the chunk of R code in the section requires the object!

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
tx <- reportSection(name='introduction',title='Introduction',parent=NULL,txt="This is introduction section...",
                  code = { 
                  
                  # R code:
                  
                  print(plot(1:10))
                  
                  })

tx

\dontrun{testSection(tx)}

}

