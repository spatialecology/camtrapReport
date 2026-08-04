# Class definitions used by camtrapReport
# Licence: MIT
#--------

#' camReport classes
#'
#' Class definitions used by camtrapReport to store camera-trap data, report
#' metadata, report sections, and intermediate report objects.
#'
#' @section camReport class:
#' The `camReport` class is the main object class used by camtrapReport. Objects
#' of this class are usually created with [camData()]. A `camReport` object
#' stores the camera-trap dataset, report metadata, processed summaries, report
#' modules, status-report modules, and settings used for report generation.
#'
#' @section Main fields:
#' Important fields in a `camReport` object include:
#'
#' * `data`: A list containing the camera-trap data tables, including
#'   observations, deployments, media, locations, sequences, and taxonomy.
#' * `habitat`: Optional habitat information linked to camera locations.
#' * `study_area`: Optional spatial object or path describing the study area.
#' * `siteName`: Name of the study site.
#' * `title`: Title used in the generated report.
#' * `subtitle`: Subtitle used in the generated report.
#' * `authors`: Author or contributor text used in the generated report.
#' * `institute`: Institute or organisation text used in the generated report.
#' * `description`: Study-area or project description used in the generated
#'   report.
#' * `years`: Years included in the report summaries.
#' * `group_definition`: Definitions of species or observation groups used in
#'   the report.
#' * `setting`: Report settings, including selected focus groups and other
#'   configuration options.
#' * `data_status`: Data-status summaries generated from the input dataset.
#' * `reportObjectElements`: Report modules and related objects used to generate
#'   the ecological report.
#' * `statusReportObjects`: Objects used to generate the data-status report.
#'
#' @section Supporting classes:
#' camtrapReport also defines supporting classes and class unions used
#' internally, including `camInfo`, `.Rchunk`, `.textSection`,
#' `characterORnull`, `characterORlist`, `characterORlistORnull`,
#' `listORnull`, and `data.frameORnull`.
#'
#' @name camReport-classes
#' @aliases camReport camReport-class camR camInfo-class characterORnull-class characterORlist-class characterORlistORnull-class listORnull-class data.frameORnull-class .Rchunk-class .textSection-class show,camReport-method show,camInfo-method
#' @docType class
#' @seealso [camData()], [report()], [status()], [info()], [reportSection()]
NULL

#setOldClass("ctdp")
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

