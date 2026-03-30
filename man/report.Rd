\name{report}
\alias{report}
\alias{report,camReport-method}


\title{
Generating a report based on \code{camReport} object 
}

\description{
This function facilitates generating an HTML report by creating an RMarkdown file based on the modules added to \code{camReport} object, then rendering an HTML file containing the camtrap report sections.
}
\usage{
report(object,filename,view)
}

\arguments{
  \item{object}{The camReport object created by the \code{camData} function}
  \item{filename}{optional (default is "report"); a character specifying filename or path/filename to write the report and rmarkdown files. The default location to write the files is the data folder}
  \item{view}{logical (default = TRUE); specifies whether the renderred html file should be displayed!}
  
}
\details{
By default (if filename is not specified), two files including "report.html" and "report.rmd" will be written in the folder of the camera-trap dataset.

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

report(cm, view=T)



}

}

