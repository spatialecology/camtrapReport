# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  July 2025
# Version 1.0
# Licence GPL v3
#--------



if (!isGeneric("reportSection")) {
  setGeneric("reportSection", function(name,title,parent,txt,code_setting,code)
    standardGeneric("reportSection"))
}


setMethod('reportSection', signature(name='character'), 
          function(name,title,parent,txt,code_setting,code) {
            if (missing(title)) title <- ""
            if (missing(parent)) parent <- NULL
            if (missing(txt)) txt <- NULL
            if (missing(code_setting)) code_setting <- NULL
            if (missing(code)) code <- NULL
            else code <- substitute(code)
            #------------
            .x <- .getTextObj(name=name,title=title,parent=parent,txt=txt)
            #------------
            
            if (!is.null(code)) {
              if (!is.null(code_setting) && as.character(substitute(code_setting))[1] == '{' ) {
                code_setting <- substitute(code_setting)
                code_setting <- as.character(code_setting)[-1]
                code_setting <- .trim(code_setting)
                code_setting <- .rmChar(code_setting,rm=c(1,2),rmLast = TRUE)
              }
              
              
              #----
              
              
              if (as.character(code)[1] != '{' ) stop('code should be placed within { } ')
              
              code <- paste(as.character(code)[-1],collapse = '\n')
              
              .xc <- new('.Rchunk',parent=parent,name=paste0(name,'_code'),setting=code_setting,code=code)
              
              .x@Rchunk <- .xc
            }
            .x
          }
)
