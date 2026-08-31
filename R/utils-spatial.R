.get_projected_sf <- function(x) {
  if (!.require("sf")) {
    return(NULL)
  }
  
  if (is.null(sf::st_crs(x))) {
    warning("Input sf object has no CRS; assuming EPSG:4326.")
    x <- sf::st_set_crs(x, 4326)
  }
  
  if (!identical(sf::st_crs(x)$epsg, 4326L)) {
    x <- sf::st_transform(x, 4326)
  }
  
  cen <- sf::st_coordinates(
    sf::st_centroid(
      sf::st_union(sf::st_geometry(x))
    )
  )
  
  lon <- cen[1]
  lat <- cen[2]
  
  if (abs(lat) <= 84) {
    .zone <- ((floor((lon + 180) / 6) %% 60) + 1)
    .epsg <- if (lat >= 0) 32600 + .zone else 32700 + .zone
    sf::st_transform(x, .epsg)
  } else {
    proj4 <- sprintf(
      "+proj=laea +lat_0=%.6f +lon_0=%.6f +datum=WGS84 +units=m +no_defs",
      lat,
      lon
    )
    
    sf::st_transform(x, proj4)
  }
}

#--------
.is.projected <- function(x) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    return(FALSE)
  }
  
  e <- try(as.vector(terra::ext(x)), silent = TRUE)
  
  if (inherits(e, "try-error") || length(e) != 4) {
    return(FALSE)
  }
  
  !all(e[1:2] >= -180 & e[1:2] <= 180 & e[3:4] >= -90 & e[3:4] <= 90)
}

#--------

.get_projected_vect <- function(x) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("The terra package is required for spatial projection.")
  }
  
  if (.is.projected(x)) {
    return(x)
  }

  cen <- colMeans(terra::crds(x), na.rm = TRUE)

  lon <- cen[1]
  lat <- cen[2]

  if (abs(lat) <= 84) {
    .zone <- ((floor((lon + 180) / 6) %% 60) + 1)
    .epsg <- if (lat >= 0) 32600 + .zone else 32700 + .zone
    terra::project(x, paste0("EPSG:", .epsg))
  } else {
    proj4 <- sprintf(
      "+proj=laea +lat_0=%.6f +lon_0=%.6f +datum=WGS84 +units=m +no_defs",
      lat,
      lon
    )

    terra::project(x, proj4)
  }
}

#--------

