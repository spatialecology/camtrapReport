\name{add_Module}
\alias{add_Module}
\alias{move_Module}
\alias{remove_Module}
\alias{empty_trash}
\alias{restore_Module}
\alias{list_Modules}
\alias{add_Module,character-method}
\alias{move_Module,character-method}
\alias{remove_Module,character-method}
\alias{empty_trash,ANY-method}
\alias{restore_Module,character-method}
\alias{list_Modules,ANY-method}

\title{
Manage report modules
}

\description{
Manage report modules in camtrapReport. These functions can be used to list existing modules, add new modules, move modules within the report structure, remove modules, restore deleted modules, and empty the module trash.
}

\usage{
add_Module(x, before, after, test, object)

move_Module(name, before, after, parent, level0)

remove_Module(name, recursive)

restore_Module(name, batch_id, test)

empty_trash(name, id)

list_Modules(tree, brief, include_trash, validate)
}

\arguments{
  \item{x}{Character. Path to a module YAML file to add to the package.}
  \item{before}{Optional character value. Name of the module before which the new or moved module should be placed.}
  \item{after}{Optional character value. Name of the module after which the new or moved module should be placed.}
  \item{test}{Logical. If \code{TRUE}, the module is tested before being added or restored.}
  \item{object}{Optional \code{camReport} object used when testing a module that requires access to report data.}
  \item{name}{Character. Name of the module to move, remove, restore, or permanently delete from the trash.}
  \item{parent}{Optional character value. Name of the parent module. If omitted, the current parent is retained when moving a module.}
  \item{level0}{Optional character vector specifying the top-level module order.}
  \item{recursive}{Logical. If \code{TRUE}, child modules are removed together with the selected parent module.}
  \item{batch_id}{Optional character value. Identifier of a deleted module batch to restore.}
  \item{id}{Optional character value. Identifier of a deleted module batch to permanently remove from the trash.}
  \item{tree}{Logical. If \code{TRUE}, modules are returned as a hierarchical tree.}
  \item{brief}{Logical. If \code{TRUE}, a shortened module summary is returned when \code{tree = FALSE}.}
  \item{include_trash}{Logical. If \code{TRUE}, deleted modules in the trash are also listed.}
  \item{validate}{Logical. If \code{TRUE}, module YAML files are checked for validity when listing modules.}
}

\details{
camtrapReport uses a modular report architecture in which each report section is stored as a module. A module can contain section metadata, text, executable R code, required packages, and rendering settings.

\code{add_Module} adds a new YAML-defined module to the package module directory. The module can optionally be inserted before or after an existing module.

\code{move_Module} changes the position or parent of an existing module within the hierarchical report structure.

\code{remove_Module} moves a module to the module trash. If \code{recursive = TRUE}, child modules are also moved to the trash.

\code{restore_Module} restores a previously removed module from the trash.

\code{empty_trash} permanently removes modules from the trash.

\code{list_Modules} lists active modules and can optionally include deleted modules stored in the trash.
}

\value{
The returned value depends on the function used. \code{add_Module}, \code{move_Module}, \code{remove_Module}, \code{restore_Module}, and \code{empty_trash} return information invisibly about the performed operation. \code{list_Modules} returns a data frame or a hierarchical module tree.
}

\seealso{
\code{reportSection}, \code{updateReportSection}, \code{testSection}, \code{report}
}

\examples{
\dontrun{
# List modules as a tree
list_Modules()

# List modules in table form
list_Modules(tree = FALSE)

# Add a new module from a YAML file
add_Module("new_module.yml", after = "captures")

# Move a module
move_Module("species_accumulation", after = "richness")

# Remove a module and its child modules
remove_Module("species_accumulation", recursive = TRUE)

# Restore a removed module
restore_Module("species_accumulation")

# Empty the module trash
empty_trash()
}
}
