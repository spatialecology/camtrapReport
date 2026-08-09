#' camtrapReport: Automated reports for camera-trap data
#'
#' Tools for reading Camtrap DP datasets, summarising camera-trap data, checking
#' data status, and generating automated HTML reports for camera-trap monitoring
#' projects.
#'
#' The package provides functions to create a [`camReport`][camReport-classes]
#' object from camera-trap data, inspect and update report metadata, manage
#' modular report sections, and generate ecological and data-status reports.
#'
#' Main user-facing functions include [camData()], [report()], [status()],
#' [info()], [reportSection()], [updateReportSection()], and [gui()].
#'
#' @seealso
#' Useful links:
#'
#' * <https://spatialecology.github.io/camtrapReport/>
#' * <https://github.com/spatialecology/camtrapReport>
#' * Report bugs at <https://github.com/spatialecology/camtrapReport/issues>
#'
#' @keywords package
#' @import methods
#' @importFrom graphics abline legend par points lines boxplot barplot arrows segments text axis image polygon
#' @importFrom grDevices colorRampPalette
#' @importFrom stats cor cov density quantile na.omit predict setNames time
#' @importFrom utils read.csv install.packages unzip head remove.packages tail timestamp
#' @importFrom glue glue
#' @importFrom data.table data.table
#' @importFrom dplyr bind_rows distinct filter group_by mutate select slice left_join
#' @importFrom terra hull crs expanse extract project rast readRDS unwrap vect wrap writeRaster
#' @importFrom lubridate interval
#' @export add_Module
#' @export camData
#' @export camR
#' @export empty_trash
#' @export gui
#' @export info
#' @export info<-
#' @export install_All
#' @export list_Modules
#' @export listReportSections
#' @export move_Module
#' @export remove_Module
#' @export report
#' @export reportSection
#' @export restore_Module
#' @export section_names
#' @export sections
#' @export status
#' @export testSection
#' @export updateReportSection
#' @exportClass camReport
#' @importFrom spatstat.geom owin ppp
#' @importFrom spatstat.explore quadrat.test
"_PACKAGE"