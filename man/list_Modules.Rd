\name{list_Modules}
\alias{list_Modules}
\alias{list_Modules,ANY-method}

\title{
List available report modules
}

\description{
List report modules available in the camtrapReport module library.
}

\usage{
list_Modules(tree, brief, include_trash, validate)

\S4method{list_Modules}{ANY}(tree, brief, include_trash, validate)
}

\arguments{
  \item{tree}{Logical. If \code{TRUE}, modules are returned as a hierarchical tree table.}
  \item{brief}{Logical. If \code{TRUE}, a shortened module summary is returned when \code{tree = FALSE}.}
  \item{include_trash}{Logical. If \code{TRUE}, active records from the module trash are included in the output.}
  \item{validate}{Logical. If \code{TRUE}, module YAML files are checked for validity while listing modules.}
}

\value{
A data frame describing available modules. If \code{include_trash = TRUE}, a list is returned containing active modules and trash records.
}

\details{
This function can be used to inspect the modules available for report generation. Modules can be returned either as a hierarchical tree or as a table with module metadata.

When \code{validate = TRUE}, the function checks whether module YAML files can be parsed and whether their structure is valid. This is useful for detecting missing, invalid, duplicated, or unlisted modules.
}

\seealso{
\code{add_Module}, \code{move_Module}, \code{remove_Module}, \code{restore_Module}, \code{empty_trash}
}

\examples{
\dontrun{
# List modules as a hierarchical tree
list_Modules()

# List modules as a table
list_Modules(tree = FALSE)

# Include trash records
list_Modules(tree = FALSE, include_trash = TRUE)

# Validate module YAML files while listing
list_Modules(tree = FALSE, validate = TRUE)
}
}