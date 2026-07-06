\name{gui}
\alias{gui}
\alias{gui,camReport-method}

\title{
Launch the camtrapReport GUI
}

\description{
Launch an interactive Shiny interface for configuring and generating camtrapReport outputs from an existing \code{camReport} object.
}

\usage{
gui(object, launch.browser, max_upload_mb, ...)
}

\arguments{
  \item{object}{A \code{camReport} object, usually created with \code{camData}.}
  \item{launch.browser}{Logical. If \code{TRUE}, the application is opened in the default web browser.}
  \item{max_upload_mb}{Numeric. Maximum file-upload size in megabytes. The default is \code{2000}.}
  \item{...}{Additional arguments passed to \code{shiny::runApp}.}
}

\details{
The function starts a Shiny-based graphical user interface for editing report metadata, selecting report sections, defining focus groups, and generating data-status or ecological reports.

The GUI can be used with an existing \code{camReport} object and provides an interactive alternative to configuring reports directly in R.
}

\value{
Invisibly returns the supplied \code{camReport} object after launching the application.
}

\seealso{
\code{camData}, \code{report}, \code{status}, \code{info}
}

\examples{
\dontrun{
cm <- camData("data-folder")

gui(cm)
}
}
