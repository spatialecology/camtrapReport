# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  July 2025
# Version 1.0
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