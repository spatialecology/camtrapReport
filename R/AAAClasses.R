# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  May 2025
# Version 1.1
# Licence GPL v3
#--------




setOldClass("ctdp")
setOldClass("datapackage")

setClassUnion("characterORnull", c("character", "NULL"))
setClassUnion("characterORlist", c("character", "list"))
setClassUnion("characterORlistORnull", c("character", "list","NULL"))
setClassUnion("listORnull", c("list","NULL"))
setClassUnion("data.frameORnull", c("data.frame","NULL"))
setClassUnion("PackedSpatVectorORnull", c("PackedSpatVector","NULL"))
#-------
setClass('.Rchunk',
         representation(
           parent='characterORnull',
           name='characterORnull',
           setting='character',
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


