# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  May 2026
# Version 1.1
# Licence GPL v3
#--------



if (!isGeneric("section_names")) {
  setGeneric("section_names", function(keep,exclude)
    standardGeneric("section_names"))
}


setMethod('section_names', signature(keep='ANY'), 
          function(keep,exclude) {
            if (missing(keep)) keep <- NULL # if NULL, keep all modules 
            if (missing(exclude)) exclude <- NULL
            
            #---------
            n <- .get_module_names()
            
            if (is.character(keep) && length(keep) > 0) {
              w <- keep %in% n
              if (!all(w)) {
                if (!any(w)) stop('None of the specified section/module names in keep are available; use modeule_names() to get a list of existing modules...!')
                warning(paste0("Several section/module names specified in keep are not available: ",.paste_comma_and(keep[!w])))
              }
              #----
              n <- keep[w]
              w <- .check_parent(n)
              if (!is.null(w)) {
                n <- n[!n %in% w]
              }
              #----
              return(n)
            }
            #-----
            if (is.character(exclude) && length(exclude) > 0) {
              w <- exclude %in% n
              if (!all(w)) {
                if (!any(w)) stop('None of the specified section/module names in exclude are available; use modeule_names() to get a list of existing modules...!')
                warning(paste0("Several section/module names specified in exclude are not available: ",.paste_comma_and(exclude[!w])))
              }
              exclude <- exclude[w]
              
              n <- n[!n %in% exclude]
              w <- .check_parent(n)
              if (!is.null(w)) {
                n <- n[!n %in% w]
              }
              return(n)
            } else return(n)
            
          }
)

#-------

if (!isGeneric("sections")) {
  setGeneric("sections", function(x,n)
    standardGeneric("sections"))
}


setMethod('sections', signature(x='camReport'), 
          function(x,n) {
            if (missing(n)) n <- NULL
            else if (!is.character(n)) {
              n <- NULL
              warning('`n` should be character...(it is ignored -> NULL)')
            }
            #---------
            w <- sort(c(which(is.na(x$reportObjectElements$Modules_info$tested)),which(x$reportObjectElements$Modules_info$tested)))
            nn <- x$reportObjectElements$Modules_info$name[w]
            if (is.null(n)) {
              return(nn)
            }
            #------
            if (!all(n %in% nn)) {
              if (all(n %in% x$reportObjectElements$Modules_info$name)) {
                message('\nSome of the specified sections are excluded as their test results were problematic...!')
              } else {
                if (!any(n %in% nn)) stop('None of the specified section names are known... (use the `section_names` function to get the correct names of the available sections)!')
                else message('\nSome of the specified section names are unknown and ignored... (use the `section_names` function to get the correct names of the available sections)!')
              }
            }
            
            n <- n[n %in% nn]
            #------
            .attach_modules(x,n = n)
            
            message('\nthe report sections are updated...')
            
          }
)

