\name{add_Module}
\alias{add_Module}
\alias{add_Module,character-method}

\title{
Add a report module
}

\description{
Add a new YAML-defined report module to the camtrapReport module library.
}

\usage{
add_Module(x, before, after, test, object)

\S4method{add_Module}{character}(x, before, after, test, object)
}

\arguments{
  \item{x}{Character. Path to a YAML module file.}

  \item{before}{Optional character value. Name of the existing module before which the new module should be inserted.}

  \item{after}{Optional character value. Name of the existing module after which the new module should be inserted.}

  \item{test}{Logical. If \code{TRUE}, the module is tested before being added.}

  \item{object}{Optional \code{camReport} object used for testing a module that requires access to camera-trap data or report information.}
}

\value{
Invisibly returns information about the added module, including the module object, updated module table, module identifier, and copied file path.
}

\details{
Report modules define the content and structure of sections used in camtrapReport reports. A module is usually stored as a YAML file and may contain section metadata, text, R code, required packages, and rendering settings.

\code{add_Module} copies a YAML module file into the camtrapReport module library and adds it to the module index. The new module can be inserted before or after an existing module using \code{before} or \code{after}.

If \code{test = TRUE}, the function checks whether the module can be read and whether its structure is valid before adding it. If the module code requires access to a \code{camReport} object, the \code{object} argument should be supplied.
}

\seealso{
\code{list_Modules}, \code{move_Module}, \code{remove_Module}, \code{restore_Module}, \code{reportSection}, \code{testSection}
}

\examples{
\dontrun{
# Add a new module to the module library
add_Module("new_module.yml")

# Add a module after an existing module
add_Module("new_module.yml", after = "species_summary")

# Test the module before adding it
cm <- camData("data-folder")

add_Module(
  x = "new_module.yml",
  after = "species_summary",
  test = TRUE,
  object = cm
)

# Check the updated module list
list_Modules()
}
}