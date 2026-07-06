\name{gui}
\alias{gui}
\alias{gui,camReport-method}

\title{
Open the camtrapReport graphical user interface
}

\description{
Open an interactive Shiny interface for working with an existing \code{camReport} object. The interface allows users to inspect data, configure report settings, and generate camtrapReport outputs interactively.
}

\usage{
gui(object, launch.browser, max_upload_mb, ...)

\S4method{gui}{camReport}(object, launch.browser = TRUE, max_upload_mb = 2000, ...)
}

\arguments{
  \item{object}{A \code{camReport} object, usually created with \code{camData}.}

  \item{launch.browser}{Logical. If \code{TRUE}, the Shiny application is opened in the default web browser.}

  \item{max_upload_mb}{Numeric. Maximum file-upload size in megabytes. The default is \code{2000}.}

  \item{...}{Additional arguments passed to \code{shiny::runApp}.}
}

\value{
Opens the Shiny application. The function is mainly used for its interactive interface and does not return a report object.
}

\details{
The graphical user interface helps users customise report generation interactively. For example, users can select which species and years to include, review and edit report text and metadata, adjust report settings, and generate camtrapReport outputs without editing R code directly.

The \code{object} argument should be a valid \code{camReport} object. This object contains the camera-trap data, report information, report settings, and report sections used by the graphical interface.

The \code{launch.browser} argument controls whether the application is opened automatically in the default web browser. The \code{max_upload_mb} argument controls the maximum file-upload size allowed by the Shiny application.
}


\seealso{
\code{camData}, \code{report}, \code{status}, \code{info}
}

\examples{
\dontrun{
cm <- camData("data-folder")

gui(cm)

gui(cm, launch.browser = TRUE)

gui(cm, launch.browser = TRUE, max_upload_mb = 1000)
}
}