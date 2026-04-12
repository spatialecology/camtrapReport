# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  April 2026
# Version 1.0
# Licence GPL v3
#--------


if (!isGeneric("info")) {
  setGeneric("info", function(x,name)
    standardGeneric("info"))
}


setMethod('info', signature(x='camReport'), 
          function(x,name) {
            if (missing(name)) name <- NULL
            
            .inf <- list()
            
            if (is.null(name)) {
              #name <- c('title','subtitle','authors','institute','siteName','description','sampling')
              name <- c('title','subtitle','authors','institute','siteName')
            } else {
              #.inf <- camR$fields()
              #.inf <- .inf[.inf == "character"]
              name <- name[name %in% names(camR$fields())]
              if (length(name) == 0) {
                warning('The specified name(s) are not identified/available in the camReport object (the defaults are used)...!')
                name <- c('title','subtitle','authors','institute','siteName')
              }
            }
            #-----
            for (n in name) {
              .inf[[n]] <- x[[n]]
            }
            class(.inf) <- 'camInfo'
            
            .inf
          }
)


# 
if (!isGeneric("info<-")) {
  setGeneric("info<-", function(x,name,value)
    standardGeneric("info<-"))
}


setReplaceMethod('info', signature(x='camReport',name='character'), 
                 function(x,name,value) {
                   
                   if (length(name) > 1) stop('Only one field (name) can be updated...!')
                   
                   if (tolower(name) %in% c('introduction','study area','image processing','sampling efforts','sampling effort','acknowledgements','acknowledgement')) {
                     
                     if (tolower(name) == 'study area') {
                       x$description <- value
                       return(invisible(x))
                     }
                     
                     if (tolower(name) == 'introduction') {
                       return(updateReportSection(x,'introduction',text=value))
                     }
                     
                     if (tolower(name) == 'image processing') {
                       return(updateReportSection(x,'introduction',text=value))
                     }
                     
                     if (tolower(name) %in% c('sampling efforts','sampling effort')) {
                       return(updateReportSection(x,'sampling efforts',text=value))
                     }
                     
                     
                     if (tolower(name) %in% c('acknowledgements','acknowledgement')) {
                       return(updateReportSection(x,'acknowledgements',text=value))
                     }
                     
                   } else {
                     .f <- camR$fields()
                     
                     name <- name[name %in% names(.f)] 
                     
                     if (length(name) == 0) stop('The specified name is not identified or available in the camReport object...!')
                     
                     x[[name]] <- value
                     
                     
                     invisible(x)
                   }
                   
                 }
)

#-----------