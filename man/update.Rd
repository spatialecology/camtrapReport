\name{updateReportSection}
\alias{updateReportSection}
\alias{listReportSections}
\alias{updateReportSection,camReport-method}
\alias{listReportSections,camReport-method}


\title{
Updating content of report sections 
}

\description{
This function can be used to update the content (text, code) of a section (module) within a \code{camReport} object.
}
\usage{
  updateReportSection(x,section,text,title,code,code_name,code_setting,packages,append_text,append_code)
  
  listReportSections(x)

}

\arguments{
  \item{x}{A camReport object created by the \code{camData} function}
  \item{section}{A character to refer to a report section (name or title of the report)}
  \item{text}{optional, to update the text body of the report section}
  \item{title}{optional, a new title for the section}
  \item{code}{optional, to update or add the R code chunk}
  \item{code_name}{optional, if R code chunk is provided, its name can be specified}
  \item{code_setting}{optional, setting for the new R chunk}
  \item{packages}{optional, name of R packages required by the new R chunk}
  \item{append_text}{optional; whether the new text should be appended to the previous text (default: FALSE)}
  \item{append_code}{optional; whether the new R code should be appended to the previous R code (default: FALSE)}
  
}
\details{
To find a report section, either title or name can be used in the \code{section} argument. The \code{listReportSections} is useful to list all sections added to a camReport object and check its exact name.

}
\references{
Ebrahimi et al. (2025) xxx
}
\author{
Elham Ebrahimi
}



\seealso{
info
}
\examples{


\dontrun{
cm <- camData("data-folder")

listReportSections(cm)



}

}

