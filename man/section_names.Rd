\name{section_names}
\alias{section_names}
\alias{section_names,ANY-method}
\title{Get available report section names}
\usage{
section_names(keep, exclude)

\S4method{section_names}{ANY}(keep, exclude)
}
\arguments{
\item{keep}{Optional character vector of section/module names to keep.}

\item{exclude}{Optional character vector of section/module names to exclude.}
}
\value{
A character vector of section/module names.
}
\description{
Returns the names of available report sections/modules. Users can optionally
keep only selected sections or exclude selected sections.
}
