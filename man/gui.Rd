\name{gui}
\alias{gui}
\alias{gui,camReport-method}


\title{
Launch the camtrapReport GUI
}

\description{
Opens an interactive Shiny interface for configuring and generating camtrapReport outputs from an existing `camReport` object.
}
\usage{

gui(object,launch.browser,max_upload_mb, ...)


}

\arguments{
  \item{object}{A `camReport` object, usually created with `camData()`}
  \item{launch.browser}{Logical. If `TRUE`, the application is opened in the default web browser.}
  \item{max_upload_mb}{Numeric. Maximum file-upload size in megabytes. The default is 2000.}
  \item{...}{Additional arguments passed to [shiny::runApp()]}
  
}
\details{
The function starts a shiny-based web-server and lunch a GUI interface.

}
\references{
Ebrahimi et al. (2026) xxx
}
\author{
Elham Ebrahimi
}



\examples{


\dontrun{
cm <- camData("data-folder")

gui(cm) 


}

}

