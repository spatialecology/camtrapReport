\name{remove_Module}
\alias{remove_Module}
\alias{remove_Module,character-method}

\title{
Remove a report module
}

\description{
Move a report module to the module trash folder. If requested, child modules can also be removed.
}

\usage{
remove_Module(name, recursive)
}

\arguments{
  \item{name}{Character. Name of the module to remove.}
  \item{recursive}{Logical. If \code{TRUE}, child modules are removed together with the selected module.}
}

\value{
Invisibly returns information about the deleted module batch.
}

\details{
Removed modules are moved to the module trash folder rather than permanently deleted. This allows deleted modules to be recovered later if needed.
}

\seealso{
\code{recover_Module}, \code{list_Modules}, \code{audit_Modules}
}

\examples{
\dontrun{
remove_Module("species_accumulation", recursive = TRUE)
}
}