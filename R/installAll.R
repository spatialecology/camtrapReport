# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update : June 2026
# Version 1.4
# Licence  MIT
#--------



.eval <- function(x,env) {
  eval(parse(text=x),envir=env)
}
#------
.is.installed <- function(n) {
  names(n) <- n
  sapply(n, function(x) length(unlist(lapply(.libPaths(), function(lib) find.package(x, lib, quiet=TRUE, verbose=FALSE)))) > 0)
}
#---------

.require <-function(x) {
  x <- as.character(x)
  xx <- unlist(lapply(.libPaths(), function(lib) find.package(x, lib, quiet=TRUE, verbose=FALSE)))
  if (length(xx) > 0) {
    .loaded <- eval(parse(text=paste0('require(',x,')')))
    return (.loaded)
  } else FALSE
}
#----------
.loadLib <- function(pkgs) {
  options(warn=-1)
  return(unlist(lapply(pkgs,function(x) {
    all(unlist(lapply(x,function(p) {.require(p)})))
  })))
  options(warn=0)
}
#---------

.getPackageList <- function() {
  p <- .get_module_packages()
  if (file.exists(system.file("external","camtrapReportConfig.rds",package="camtrapReport"))) {
    pl <- readRDS(system.file("external","camtrapReportConfig.rds",package="camtrapReport"))
    p <- unique(c(p,pl$packages))
  }
  p
}

#----
# List of the packages that should be installed from GitHUB:
.getPackageGitHubList <- function() {
  if (file.exists(system.file("external","camtrapReportConfig.rds",package="camtrapReport"))) {
    pl <- readRDS(system.file("external","camtrapReportConfig.rds",package="camtrapReport"))
    pl$github
  }
}
#----
.getPackageGitLabList <- function() {
  if (file.exists(system.file("external","camtrapReportConfig.rds",package="camtrapReport"))) {
    pl <- readRDS(system.file("external","camtrapReportConfig.rds",package="camtrapReport"))
    pl$gitlab
  }
}
#--------------





if (!isGeneric("install_All")) {
  setGeneric("install_All", function(pkgs,update,...)
    standardGeneric("install_All"))
}


setMethod('install_All', signature(pkgs='ANY'),
          function(pkgs,update=FALSE,...) {
            if (missing(update)) update <- FALSE
            pl <- .getPackageList()
            plG <- .getPackageGitHubList()
            #plGL <- .getPackageGitLabList()
            plGn <- names(plG)
            #plGLn <- names(plGL)
            if (!update) {
              p <- pl[!.is.installed(pl)]
              #pG <- plG[!.is.installed(plGn)]
              .n <- 0
              if (length(p) > 0) {
                for (i in seq_along(p)) {
                  .s <- try(install.packages(p[i],...),silent = TRUE)
                  if (!inherits(.s, "try-error")) .n <- .n + 1
                }
                #---
                p <- plGn[!.is.installed(plGn)]
                
                if (length(p) > 0) {
                  plG <- plG[plGn %in% p]
                  for (pG in plG) {
                    .s <- try(.eval("remotes::install_github(pG,quiet=TRUE,force = TRUE)",env=environment()),silent = TRUE)
                    if (!inherits(.s, "try-error")) .n <- .n + 1
                  }
                }
                #----
                #p <- plGLn[!.is.installed(plGLn)]
                
                # if (length(p) > 0) {
                #   plGL <- plGL[plGLn %in% p]
                #   for (i in seq_along(plGL)) {
                #     pG <- plGL[[i]][['repo']]
                #     .h <- plGL[[i]][['host']]
                #     .s <- .s <- try(.eval("remotes::install_gitlab(pG,host = .h,quiet=TRUE,force = TRUE)",env=environment()),silent = TRUE)
                #     if (!inherits(.s, "try-error")) .n <- .n + 1
                #   }
                # }
                #p <- unique(c(pl,plGn,plGLn))
                
                p <- unique(c(pl,plGn))
                p <- p[!.is.installed(p)]
                if (length(.n) > 0) cat(paste('\n',.n,' packages are successfully installed...\n'))
                if (length(p) > 0) cat(paste('The following packages could not be installed:\n.... ',paste(p,collapse=', '),'\n'))
                
              } else {
                .n <- 0
                p <- plGn[!.is.installed(plGn)]
                
                if (length(p) > 0) {
                  plG <- plG[plGn %in% p]
                  for (pG in plG) {
                    .s <- try(.eval("remotes::install_github(pG,quiet=TRUE,force = TRUE)",env=environment()),silent = TRUE)
                    if (!inherits(.s, "try-error")) .n <- .n + 1
                  }
                }
                #----
                # p <- plGLn[!.is.installed(plGLn)]
                # 
                # if (length(p) > 0) {
                #   plGL <- plGL[plGLn %in% p]
                #   for (i in seq_along(plGL)) {
                #     pG <- plGL[[i]][['repo']]
                #     .h <- plGL[[i]][['host']]
                #     .s <- .s <- try(.eval("remotes::install_gitlab(pG,host = .h,quiet=TRUE,force = TRUE)",env=environment()),silent = TRUE)
                #     if (!inherits(.s, "try-error")) .n <- .n + 1
                #   }
                # }
                #p <- unique(c(plGn,plGLn))
                p <- unique(c(plGn))
                
                p <- p[!.is.installed(p)]
                if (.n > 0 | length(p) > 0) {
                  if (.n > 0) cat(paste('\n',.n,' packages are successfully installed...\n'))
                  if (length(p) > 0) cat(paste('The following packages could not be installed:\n.... ',paste(p,collapse=', '),'\n'))
                } else {
                  cat(paste('\n All required packages have been already installed!\n'))
                }
              }
              
            } else {
              p <- pl[!pl %in% c('stats','utils','parallel','base','grDevice','tools','methods','graphics','compiler','datasets','profile','grid')]
              
              if (length(c(p,plGn)) > 0) {
                s <- rep(TRUE,length(c(p,plGn)))
                .n <- 0
                if (length(p) > 0) {
                  .detachPackage(p)
                  pi <- p[.is.installed(p)]
                  if (length(pi) > 0) pi <- try(remove.packages(pi),silent = TRUE)
                  
                  
                  for (i in seq_along(p)) {
                    .s <- try(install.packages(p[i],...),silent = TRUE)
                    if (!inherits(.s, "try-error")) .n <- .n + 1
                  }
                }
                
                if (length(plGn) > 0) {
                  .detachPackage(plGn)
                  plGi <- plGn[.is.installed(plGn)]
                  if (length(pGi) > 0) {
                    plGi <- try(remove.packages(plGi),silent = TRUE)
                    for (pG in plG) {
                      .s <- try(.eval("remotes::install_github(pG,quiet=TRUE,force = TRUE)",env=environment()),silent = TRUE)
                      if (!inherits(.s, "try-error")) .n <- .n + 1
                    }
                  }
                }
                #---
                # if (length(plGLn) > 0) {
                #   .detachPackage(plGLn)
                #   plGi <- plGLn[.is.installed(plGLn)]
                #   if (length(pGi) > 0) {
                #     plGi <- try(remove.packages(plGi),silent = TRUE)
                #     
                #     for (i in seq_along(plGL)) {
                #       pG <- plGL[[i]][['repo']]
                #       .h <- plGL[[i]][['host']]
                #       .s <- .s <- try(.eval("devtools::install_gitlab(pG,host = .h,quiet=TRUE,force = TRUE)",env=environment()),silent = TRUE)
                #       if (!inherits(.s, "try-error")) .n <- .n + 1
                #     }
                #   }
                # }
                #---
                #p <- unique(c(plGn,plGLn))
                p <- unique(c(plGn))
                
                p <- p[!.is.installed(p)]
                if (.n > 0 | length(p) > 0) {
                  if (.n > 0) cat(paste('\n',.n,' packages are successfully installed...\n'))
                  if (length(p) > 0) cat(paste('The following packages could not be installed:\n.... ',paste(p,collapse=', '),'\n'))
                } else {
                  cat(paste('\n All required packages already installed... (they are not UPDATED!) \n'))
                }
                
              } else cat(paste('\n There is no package to install!\n'))
            }
            
          }
)



