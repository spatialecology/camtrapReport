# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  April 2026
# Version 1.2
# Licence GPL v3
#--------


.getTextObj <- function(name=NULL,title=NULL,parent=NULL,headLevel=1,txt=NULL) {
  new('.textSection',name=name,title=title,parent=parent,headLevel=headLevel,txt=txt)
}
#---


.getRchunk <- function(parent=NULL,name=NULL,setting=NULL,packages=NULL,code) {
  
  if (!is.null(setting) && as.character(substitute(setting))[1] == '{' ) {
    setting <- substitute(setting)
    setting <- as.character(setting)[-1]
    setting <- .trim(setting)
    setting <- .rmChar(setting,rm=c(1,2),rmLast = TRUE)
  }
  
  
  #----
  code <- substitute(code)
  
  if (as.character(code)[1] != '{' ) stop('code should be placed within { } ')
  
  code <- paste(as.character(code)[-1],collapse = '\n')
  
  new('.Rchunk',parent=parent,name=name,setting=setting,packages=packages,code=code)
  
  
}
#----

.glueTextSection <- function(x,.envir) {
  #if (!is.null(x@parent)) headLevel <- 1
  #----
  .title <- paste0(paste(rep('#',x@headLevel),collapse = ''),' ',x@title)
  
  if (!is.null(x@txt)) .out <- paste0(c(.title,sapply(x@txt,glue::glue,.envir=.envir)),collapse = '\n\n')
  else .out <- paste0(.title,'\n\n')
  #---
  if (!is.null(x@Rchunk)) {
    
    if (is.list(x@Rchunk)) {
      for (i in seq_along(x@Rchunk)) {
        if (is.null(x@Rchunk[[i]]@setting)) {
          .p1 <- paste0('```{r ',x@Rchunk[[i]]@name,'}')
        } else {
          .p1 <- paste0('```{r ',x@Rchunk[[i]]@name,',',paste(x@Rchunk[[i]]@setting,collapse = ','),'}')
        }
        .p1 <- paste0(.p1,'\n',x@Rchunk[[i]]@code,'\n','```')
        .out <- paste0(.out,'\n\n',.p1,'\n\n')
      }
      
      
    } else {
      if (is.null(x@Rchunk@setting)) {
        p1 <- paste0('```{r ',x@Rchunk@name,'}')
      } else {
        p1 <- paste0('```{r ',x@Rchunk@name,',',paste(x@Rchunk@setting,collapse = ','),'}')
      }
      p1 <- paste0(p1,'\n',x@Rchunk@code,'\n','```')
      .out <- paste0(.out,'\n\n',p1,'\n\n')
    }
    
  }
  .out
}
#--------

if (!isGeneric("reportSection")) {
  setGeneric("reportSection", function(name,title,parent,txt,code_setting,packages,code)
    standardGeneric("reportSection"))
}


setMethod('reportSection', signature(name='character'), 
          function(name,title,parent,txt,code_setting,packages,code) {
            if (missing(title)) title <- ""
            if (missing(parent)) parent <- NULL
            if (missing(txt)) txt <- NULL
            if (missing(code_setting)) code_setting <- NULL
            if (missing(code)) code <- NULL
            else code <- substitute(code)
            
            #if (missing(object)) object <- NULL
            if (missing(packages)) packages <- NULL
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
              
              .xc <- new('.Rchunk',parent=parent,name=paste0(name,'_code'),setting=code_setting,packages=packages,code=code)
              
              .x@Rchunk <- .xc
            }
            .x
          }
)
#===========
