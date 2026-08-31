# Internal spatial and plotting utilities for camtrapReport
# Licence: MIT
#--------

.basic_corrplot <- function(x, main = "Species Co-occurrence") {
  x[upper.tri(x, diag = TRUE)] <- NA
  
  .colors <- grDevices::colorRampPalette(c("red", "white", "blue"))(100)
  
  graphics::image(
    seq_len(ncol(x)),
    seq_len(nrow(x)),
    t(x[rev(seq_len(nrow(x))), , drop = FALSE]),
    col = .colors,
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = main
  )
  
  labels <- colnames(x)
  n <- length(labels)
  
  graphics::text(
    x = 1:n,
    y = graphics::par("usr")[3] - 0.5,
    labels = labels,
    srt = 45,
    adj = 1,
    xpd = TRUE
  )
  
  graphics::text(
    x = graphics::par("usr")[1] - 0.5,
    y = 1:n,
    labels = rev(labels),
    srt = 45,
    adj = 1,
    xpd = TRUE
  )
  
  graphics::legend(
    x = n / 1.2,
    y = n,
    legend = round(seq(-1, 1, length.out = 10), 2),
    fill = grDevices::colorRampPalette(c("red", "white", "blue"))(10),
    border = NA,
    bty = "n",
    y.intersp = 1,
    cex = 0.8
  )
}

#--------

.get_projected_sf <- function(x) {
  if (!.require("sf")) {
    return(NULL)
  }
  
  if (is.null(.eval('sf::st_crs(x)',environment()))) {
    warning("Input sf object has no CRS; assuming EPSG:4326.")
    .eval('sf::st_crs(x) <- 4326',environment())
  }
  
  if (!identical(.eval('sf::st_crs(x)$epsg',environment()), 4326L)) {
    x <- .eval('sf::st_transform(x, 4326)',environment())
  }
  
  cen <- .eval('sf::st_coordinates(
    sf::st_centroid(
      sf::st_union(sf::st_geometry(x))
    )
  )',environment())
  
  lon <- cen[1]
  lat <- cen[2]
  
  if (abs(lat) <= 84) {
    .zone <- ((floor((lon + 180) / 6) %% 60) + 1)
    .epsg <- if (lat >= 0) 32600 + .zone else 32700 + .zone
    .eval('sf::st_transform(x, .epsg)',environment())
  } else {
    proj4 <- sprintf(
      "+proj=laea +lat_0=%.6f +lon_0=%.6f +datum=WGS84 +units=m +no_defs",
      lat,
      lon
    )
    
    .eval('sf::st_transform(x, proj4)',environment())
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
  
  if (!.is.projected(x)) {
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
  } else {
    x
  }
}

