# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  April 2026
# Version 1.2
# Licence GPL v3
#--------


setMethod ('show' , '.textSection',
           function ( object ) {
             cat('class                                 :' , class(object), '\n')
             cat('===========================================================','\n')
             cat('name of the object                    : ' , object@name , '\n')
             cat('title                                 : ' , if (length(object@title) > 0) object@title else "... NO Title!..." , '\n')
             if (is.null(object@parent)) cat('parent                                : ' ,  object@parent, '\n')
             cat('is R code included?                   : ' , !is.null(object@Rchunk) , '\n')
             cat('-----------------------------------------------------------','\n')
           }
)
#-----------


setMethod ('show' , 'camInfo',
           function ( object ) {
             if (length(object) > 0) {
               .cls <- sapply(object,function(x) class(x)[1])
               w <- which(.cls == 'character')
               cat('===========================================================','\n')
               if (length(w) > 0) {
                 for (n in names(object[w])) {
                   .nr <- .charN(n)
                   if (.nr < 25) .n <- paste0(n,paste(rep(' ',25 - .nr),collapse = ''),' : ',object[[n]])
                   else .n <- paste0(n,' : ',object[[n]])
                   
                   cat(.n, '\n')
                 }
               }
               #----
               w <- which(.cls != 'character')
               if (length(w) > 0) {
                 cat('\n ---------------------------------------------------------- \n')
                 for (n in names(object[w])) {
                   cat(n,' : ')
                   print(object[[n]])
                   
                 }
               }
               cat('===========================================================','\n')
             }
           }
)
# print.camInfo <- function(object) {
#   if (length(object) > 0) {
#     .cls <- sapply(object,function(x) class(x)[1])
#     w <- which(.cls == 'character')
#     cat('===========================================================','\n')
#     if (length(w) > 0) {
#       for (n in names(object[w])) {
#         .nr <- .charN(n)
#         if (.nr < 25) .n <- paste0(n,paste(rep(' ',25 - .nr),collapse = ''),' : ',object[[n]])
#         else .n <- paste0(n,' : ',object[[n]])
#         
#         cat(.n, '\n')
#       }
#     }
#     #----
#     w <- which(.cls != 'character')
#     if (length(w) > 0) {
#       cat('\n ---------------------------------------------------------- \n')
#       for (n in names(object[w])) {
#         cat(n,' : ')
#         print(object[[n]])
#         
#       }
#     }
#     cat('===========================================================','\n')
#   }
# }
