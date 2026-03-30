\name{add_Module}
\alias{add_Module}
\alias{move_Module}
\alias{remove_Module}
\alias{empty_trash}
\alias{restore_Module}
\alias{list_Modules}
\alias{add_Module,character-method}
\alias{move_Module,character-method}
\alias{restore_Module,character-method}


\title{
Managing modules
}

\description{
A list of functions are available to manage modules in the package including listing the existing modules, adding a new one or deleting an existing one, or moving the position of a module in a report
}
\usage{
add_Module(x,to,before,after,test)

move_Module(name,before,after,parent,level0)

remove_Module(name,recursive)

empty_trash(name,id)

list_Modules(tree,brief,include_trash,validate)
}

\arguments{
  \item{x}{The new module YAML filename}
  \item{before}{The name of module before which the new module should be added}
  \item{after}{The name of module after which the new module should be added}
  \item{test}{logical; specifying whether the module should be tested}
  \item{name}{The module name}
  \item{parent}{The parent name of module (modules are organised hierarchically as parent and childs)}
  \item{level0}{Optional; names of the modules in root: c("introduction", "methods", "results", "acknowledgement", "appendix")}
  \item{recursive}{logical; when it is TRUE, by deleting a parent module, its child modules are also deleted to avoid any inconsistencies in the report}
  \item{id}{The module id}
  \item{tree}{logical; specifies whether the list of modules should represents their hierarchical tree structure}
  \item{brief}{logical; if tree is FALSE, it specifies whether brief module information should be returned}
  \item{include_trash}{logical; if tree is FALSE, it specifies whether deleted modules in trash should also be listed}
  \item{validate}{logical; whether the modules should be tested for their validity}
  
}
\details{
Given that the package is extensible, designed based on a modular architecture, a set of functions are provided to facilitate module management. 

A module refers to an object designed to contain the contents of a section in the report. These contents can be texts, tables, executable R codes, etc. Multiple interfaces are available for a user to define a module.

An easy way is to use a YAML file (a template can be used) to define a module. While the file is created, it can be added to the package using the \code{add_Module} function.

Other functions can also be used to list the existing modules, delete or recover a module, or move a module to a different location within the hierarchical structure the modules are organised.




}
\references{
Ebrahimi et al. (2026) camtrapReport: xxx
}
\author{
Elham Ebrahimi
}

\examples{

list_Modules()

}

