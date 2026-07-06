\name{empty_trash}
\alias{empty_trash}
\alias{empty_trash,ANY-method}

\title{
Empty the module trash
}

\description{
Permanently remove deleted report modules from the module trash folder.
}

\usage{
empty_trash(name, id)

\S4method{empty_trash}{ANY}(name, id)
}

\arguments{
  \item{name}{Optional character value. Name of the deleted module to permanently remove from the trash. If omitted, modules are selected using \code{id}, or all matching trash records are considered.}

  \item{id}{Optional character value. Deletion batch identifier to permanently remove from the trash.}
}

\value{
Invisibly returns the updated trash index.
}

\details{
\code{empty_trash} permanently removes modules that were previously deleted using \code{remove_Module}. Deleted modules are first moved to the module trash folder, where they can be restored using \code{restore_Module}. Once \code{empty_trash} is used, the selected trash records and associated module files are permanently removed.

The function can be used to purge a specific module by \code{name}, a deletion batch by \code{id}, or trash records according to the available arguments.
}

\seealso{
\code{remove_Module}, \code{restore_Module}, \code{list_Modules}
}

\examples{
\dontrun{
# Remove a module first
remove_Module("species_accumulation", recursive = TRUE)

# List modules and trash records
list_Modules(tree = FALSE, include_trash = TRUE)

# Permanently remove a module from the trash
empty_trash(name = "species_accumulation")

# Permanently remove a deletion batch from the trash
empty_trash(id = "20260630120000_12345")
}
}