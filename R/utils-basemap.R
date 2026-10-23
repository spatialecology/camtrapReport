# Browser-side basemap recovery. R never downloads or bundles tiles; explicit
# offline mode makes no tile requests. Provider policy references are in dev/.
.report_basemap_sources <- function(mode) {
  osm_attribution <- paste0(
    '&copy; <a href="https://www.openstreetmap.org/copyright">',
    "OpenStreetMap</a> contributors"
  )
  topo_attribution <- paste0(
    osm_attribution,
    ', SRTM | Map: &copy; <a href="https://opentopomap.org">',
    'OpenTopoMap</a> (<a href="https://creativecommons.org/licenses/',
    'by-sa/3.0/">CC-BY-SA</a>)'
  )

  # This automatic chain is intended for noncommercial research reports.
  osm_sources <- list(
    list(
      name = "OpenStreetMap",
      url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
      attribution = osm_attribution,
      maxNativeZoom = 19L,
      requiresWebReferrer = TRUE
    ),
    list(
      name = "OpenStreetMap.DE",
      url = "https://tile.openstreetmap.de/{z}/{x}/{y}.png",
      attribution = osm_attribution,
      maxNativeZoom = 18L
    ),
    list(
      name = "OpenTopoMap",
      url = "https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
      attribution = topo_attribution,
      maxNativeZoom = 17L
    )
  )

  switch(
    mode,
    openstreetmap = osm_sources,
    cartodb = list(list(
      name = "CartoDB.Positron",
      url = paste0(
        "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"
      ),
      attribution = paste0(
        osm_attribution,
        ' &copy; <a href="https://carto.com/attributions">CARTO</a>'
      ),
      maxNativeZoom = 20L,
      subdomains = "abcd"
    )),
    esri = list(list(
      name = "Esri.WorldGrayCanvas",
      url = paste0(
        "https://server.arcgisonline.com/ArcGIS/rest/services/",
        "Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
      ),
      attribution = "Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ",
      maxNativeZoom = 16L
    )),
    list()
  )
}

.add_report_basemap_recovery <- function(map, group, mode) {
  script <- system.file(
    "report-assets", "basemap-recovery.js", package = "camtrapReport",
    mustWork = TRUE
  )
  js_code <- paste(readLines(script, warn = FALSE), collapse = "\n")
  config <- list(
    group = group,
    providers = .report_basemap_sources(mode),
    timeout = 8000L,
    errorLimit = 3L,
    failureRatio = 0.75
  )
  .eval(
    "htmlwidgets::onRender(map, js_code, data = config)",
    environment()
  )
}
