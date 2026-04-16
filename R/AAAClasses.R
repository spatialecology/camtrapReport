# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  April 2026
# Version 1.4
# Licence GPL v3
#--------




#setOldClass("ctdp")
setOldClass("datapackage")
setOldClass("camInfo")
#setOldClass("difftime")

setClassUnion("characterORnull", c("character", "NULL"))
setClassUnion("characterORlist", c("character", "list"))
setClassUnion("characterORlistORnull", c("character", "list","NULL"))
setClassUnion("listORnull", c("list","NULL"))
#setClassUnion("numericORdifftime", c("numeric","difftime"))
setClassUnion("data.frameORnull", c("data.frame","NULL"))

#-------
setClass('.Rchunk',
         representation(
           parent='characterORnull',
           name='characterORnull',
           setting='characterORnull',
           packages='characterORnull',
           code='character'
         )
)
#----------

setClassUnion(".RchunkORlistORnull", c(".Rchunk","list","NULL"))


setClass('.textSection',
         representation(
           parent='characterORnull',
           name='character',
           title='character',
           headLevel='numeric',
           txt='characterORlistORnull',
           id='numeric',
           Rchunk='.RchunkORlistORnull'
         )
)
# in txt slot, a list can be provided with items which are either character (text) or .Rchunk object!


