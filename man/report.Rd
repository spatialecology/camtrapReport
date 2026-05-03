\name{report}
\alias{report}
\alias{status}
\alias{report,camReport-method}
\alias{status,camReport-method}


\title{
Generating automated reports based on a \code{camReport} object 
}

\description{
These functions facilitate generating two reports (data quality status, and ecological insights) automatically. Each report is an HTML rendered by creating an RMarkdown file based on the modules added to the \code{camReport} object.
}
\usage{
report(object,filename,view, test)

status(object,filename,view)
}

\arguments{
  \item{object}{The camReport object created by the \code{camData} function}
  \item{filename}{optional (default is "report"); a character specifying filename or path/filename to write the report and rmarkdown files. The default location to write the files is the data folder}
  \item{view}{logical (default = TRUE); specifies whether the renderred html file should be displayed!}
  \item{test}{logical (default = FALSE); specifies whether the sections should be tested!}
  
}
\details{
By default (if filename is not specified), two files including "report.html" and "report.rmd" ("data_status.html" and "data_status.rmd" for the data status report) will be written in the folder of the camera-trap dataset.

If `test = TRUE` is used, in case any error is caused in the report generation procedure, a testing procedure is started to test each section and exclude the problematic section.

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

report(cm, view=T) # ecological insights

status(cm, view=T) #  data quality status



}

}

