\name{report}
\alias{report}
\alias{status}
\alias{report,camReport-method}
\alias{status,camReport-method}


\title{
Generates an ecological or a data status report
}

\description{
These functions facilitate generating automated reports (data quality status, and ecological insights). Each report is an HTML rendered by creating an RMarkdown file based on the modules added to the \code{camReport} object.
}
\usage{
report(object,filename,view, test)

status(object,filename,view)
}

\arguments{
  \item{object}{`camReport` object created by the \code{camData} function}
  \item{filename}{optional (default is "report"); a character specifying filename or path/filename to write the report and rmarkdown files. The default location to write the files is the data folder}
  \item{view}{Logical. If `TRUE`, the generated HTML report is opened after rendering.}
  \item{test}{Logical. If `TRUE`, modules are tested if report generation fails.}
  
}
\details{
By default (if filename is not specified), two files including "report.html" and "report.rmd" ("data_status.html" and "data_status.rmd" for the data status report) will be written in the folder of the camera-trap dataset.

If `test = TRUE` is used, in case any error is caused in the report generation procedure, a testing procedure is started to test each section and exclude the problematic section.

}
\references{
Ebrahimi et al. (2026) xxx
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

