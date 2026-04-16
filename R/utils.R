# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  April 2026
# Version 2.2
# Licence GPL v3
#--------

.paste_comma_and <- function(x) {
  if (length(x) == 1) {
    return(as.character(x))
  } else {
    return(paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)]))
  }
}
#-----------
.trim <- function(x,squish=TRUE) {
  x <- strsplit(x,'')[[1]]
  if (x[1] == ' ') {
    i <- 1
    while(x[i] == ' ') {
      i <- i+1
    }
    x <- x[i:length(x)]
  }
  #-----
  if (x[length(x)] == ' ') {
    i <- length(x)
    while(x[i] == ' ') {
      i <- i-1
    }
    x <- x[1:i]
  }
  #----
  if (squish & length(x) > 0) {
    j <- FALSE
    k <- c()
    
    for (i in 1:length(x)) {
      if (x[i] == " ") {
        if (j) k <- c(k,i)
        else j <- TRUE
      } else j <- FALSE
      
    }
    if (length(k) > 0) x <- x[-k]
  }
  #----------
  if (length(x) > 0) paste(x,collapse='')
  else ""
}
#------------

# copied from the sdm package:
.require <-function(x) {
  x <- as.character(x)
  xx <- unlist(lapply(.libPaths(), function(lib) find.package(x, lib, quiet=TRUE, verbose=FALSE)))
  if (length(xx) > 0) {
    .loaded <- eval(parse(text=paste0('require(',x,')')))
    return (.loaded)
  } else FALSE
}
#-------
.eval <- function(x,env) {
  eval(parse(text=x),envir=env)
}
#-----------
.rmChar <- function(x,rm,rmLast=FALSE) {
  # rm: the character index to be removed!
  # rmLast: should the last character be removed?
  x <- strsplit(x,'')[[1]]
  x <- x[-rm]
  if (rmLast) x <- x[-length(x)]
  paste(x,collapse='')
  
}
#--------
# .getRchunk <- function(name=NULL,setting=NULL,code) {
#   
#   if (as.character(substitute(setting))[1] == '{' ) {
#     setting <- substitute(setting)
#     setting <- as.character(setting)[-1]
#     setting <- .trim(setting)
#     setting <- .rmChar(setting,rm=c(1,2),rmLast = TRUE)
#   }
#   
#   
#   #----
#   code <- substitute(code)
#   if (as.character(code)[1] != '{' ) stop('code should be placed within { } ')
#   if (is.null(setting)) {
#     p1 <- paste('```{r',name,'}')
#   } else {
#     p1 <- paste('```{r',name,',',paste(setting,collapse = ','),'}')
#   }
#   #--
#   p2 <- paste(as.character(code)[-1],collapse = '\n')
#   #---
#   paste0(p1,'\n',p2,'\n','```')
#   
# }
#-------
.findParent <- function(x,n) {
  if (length(x) == 0) return(NA)
  else {
    for (i in seq_along(x)) {
      if (is.list(x[[i]])) {
        .findParent(x[[i]],n)
      } else {
        if (x[[i]]@parent == n) {
          return(c(index=i,name=x[[i]]@name,parent=x[[i]]@parent))
        }
      }
    }
  }
}
#------
# get year based on the first 4 character!
# if interval=T, then it assumes year items in the interval field has 4 digits,
# so it identifies the components with 4 digits and extract them!
.getYear <- function(x,.interval=FALSE) {
  if (.interval) {
    .x <- strsplit(as.character(x),'-')
    lapply(.x,function(x) {
      .j <- sapply(x,function(k) length(strsplit(k,'')[[1]]))
      unique(as.numeric(names(.j[.j == 4])))
    })
  } else {
    as.numeric(substr(as.character(x),1,4))
  }
}
#----
.getMissingTaxon_GBIF <- function(x) {
  # this retrieves the class and order taxonomic information for each scientific name from GBIF
  # it uses the taxize package for the job:
  
  if (.require('taxize')) {
    .id <- "as.data.frame(get_gbifid(x,rows=1,ask=FALSE,messages=FALSE))"
    .id <- .eval(.id,env=environment())
    #----
    .x <- "classification(.id$ids,db='gbif')"
    .x <- .eval(.x,env=environment())
    w <- which(is.na(names(.x)))
    if (length(w) > 0) {
      .x <- .x[-w]
      x <- x[-w]
    }
    #----
    .class <- sapply(.x, function(x) x$name[3])
    .order <- sapply(.x, function(x) x$name[4])
    names(.class) <- names(.order) <- NULL
    
    data.frame(scientificName=x,class=.class,order=.order)
    
  } else stop('You need to install the taxize package first before executing this function...!')
  
  
  
}
#----
.getMissingTaxon_NCBI <- function(x) {
  # this retrieves the class and order taxonomic information for each scientific name from NCBI
  # it uses the taxize package for the job:
  
  if (.require('taxize')) {
    .id <- "as.data.frame(get_uid(x,rows=1,ask=FALSE,messages=FALSE))"
    .id <- .eval(.id,env=environment())
    #----
    .x <- "classification(.id$ids,db='ncbi')"
    .x <- .eval(.x,env=environment())
    #----
    .class <- .order <- rep(NA,length(.x))
    
    .class <- sapply(.x, function(x) if (length(x) > 1 && 'class' %in% x$rank) x$name[x$rank == 'class'] else NA)
    .order <- sapply(.x, function(x) if (length(x) > 1 && 'order' %in% x$rank) x$name[x$rank == 'order'] else NA)
    names(.class) <- names(.order) <- NULL
    
    data.frame(scientificName=x,class=unlist(.class),order=unlist(.order))
    
  } else stop('You need to install the taxize package first before executing this function...!')
  
  
}

#----------

# convert hms to hours:
.get_hour <- function(x) {
  as.numeric(format(as.POSIXct(x,format="%H:%M:%S"),"%H")) +
    as.numeric(format(as.POSIXct(x,format="%H:%M:%S"),"%M"))/60
}
#---------

.basic_corrplot <- function(x,main='Species Co-occurrence') {
  # a correlation plot using image function (used when corrplot is not available)!
  # x is a correlation matrix
  
  
  # Set upper triangle and diagonal to NA
  x[upper.tri(x, diag = TRUE)] <- NA
  
  # Define color palette
  .colors <- colorRampPalette(c("red", "white", "blue"))(100)
  
  # Plot the heatmap
  image(
    1:ncol(x), 1:nrow(x), t(x[nrow(x):1, ]),
    col = .colors, axes = FALSE, xlab = "", ylab = "", main = main
  )
  
  # Add axis labels with 45-degree rotation
  labels <- colnames(x)
  n <- length(labels)
  x_pos <- 1:n
  y_pos <- 1:n
  
  # X-axis labels
  text(x = x_pos, y = par("usr")[3] - 0.5, labels = labels, srt = 45, adj = 1, xpd = TRUE)
  
  # Y-axis labels
  text(x = par("usr")[1] - 0.5, y = y_pos, labels = rev(labels), srt = 45, adj = 1, xpd = TRUE)
  
  # Add color legend
  legend(
    x = n/1.2 , y = n,
    legend = round(seq(-1, 1, length.out = 10), 2),
    fill = colorRampPalette(c("red", "white", "blue"))(10),
    border = NA, bty = "n", y.intersp = 1, cex = 0.8
  )
  
}
#######


.get_projected_sf <- function(x) {
  # automatically project a spatial data with a geographic CRS
  # identify the best metric CRS given the size and location of data
  # x is an sf object!
  if (!.require("sf")) return(NULL)
  
  # Ensure data is geographic
  .eval("if (sf::st_crs(x)$epsg != 4326) {
      x <- sf::st_transform(x, 4326)
    }
    # Centroid longitude & latitude
    cen <- sf::st_coordinates(sf::st_centroid(sf::st_union(.xxs)))",env=environment())
    
  
  cen <- colMeans(crds(.xs))
  
  lon <- cen[1]; lat <- cen[2]
  if (abs(lat) <= 84) {
    # UTM zone calculation
    .zone <- ((floor((lon + 180) / 6) %% 60) + 1)
    .epsg <- if (lat >= 0) 32600 + .zone else 32700 + .zone
    return(.eval("sf::st_transform(x, .epsg)",env=environment()))
  } else {
    proj4 <- sprintf(
      "+proj=laea +lat_0=%.6f +lon_0=%.6f +datum=WGS84 +units=m +no_defs",
      lat, lon
    )
    return(.eval("sf::st_transform(x, proj4)",env=environment()))
  }
  
  
}

.is.projected <- function(x) {
  e <- as.vector(ext(x))
  !all(e >= -180 & e <= 180)
}
#----

.get_projected_vect <- function(x) {
  # automatically project a spatial data with a geographic CRS
  # identify the best metric CRS given the size and location of data
  # x is an SpatVector (terra)
  
  if (!.is.projected(x)) {
    cen <- colMeans(crds(x))
    lon <- cen[1]; lat <- cen[2]
    if (abs(lat) <= 84) {
      # UTM zone calculation
      .zone <- ((floor((lon + 180) / 6) %% 60) + 1)
      .epsg <- if (lat >= 0) 32600 + .zone else 32700 + .zone
      return(project(x, paste0("EPSG:",.epsg)))
    } else {
      proj4 <- sprintf(
        "+proj=laea +lat_0=%.6f +lon_0=%.6f +datum=WGS84 +units=m +no_defs",
        lat, lon
      )
      return(project(x, proj4))
    }
  } else {
    warning('The input dataset seems projected, so no projection is applied...!')
    x
  }
  
  
  
}
#----------

.get_Time_length <- function(x,y=NULL,unit='days') {
  # if y is NULL --> it is assumed that x is time-interval
  #------
  if (is.null(y)) {
    .s <- sapply(as.character(x),function(x) strsplit(x,'--')[[1]][1])
    .e <- sapply(as.character(x),function(x) strsplit(x,'--')[[1]][2])
    names(.s) <- names(.e) <-NULL
    as.numeric(difftime(as.POSIXct(.e),as.POSIXct(.s),units=unit))
  } else {
    as.numeric(difftime(as.POSIXct(x),as.POSIXct(y),units=unit))
  }
}
#----


.isZip <- function(x) {
  ex <- strsplit(basename(x),'\\.')[[1]]
  
  tolower(ex[length(ex)]) == 'zip'
}

#---
.isJson <- function(x) {
  # is the x a json filename?
  ex <- strsplit(basename(x),'\\.')[[1]]
  
  tolower(ex[length(ex)]) == 'json'
}
#---
.firstUpper <- function(x) {
  paste0(toupper(substr(x,1,1)), tolower(substr(x,2,nchar(x))))
}

############


#-----------
.loadPKG <- function(pkgs) {
  options(warn=-1)
  all(unlist(lapply(pkgs,function(p) {.require(p)})))
  options(warn=0)
}
#---------


# get Data/Time Format:
.getFormat <- function(x) {
  .dtFormats <- c("%Y-%m-%dT%H:%M:%OS","%Y-%m-%d %H:%M:%OS","%Y/%m/%dT%H:%M:%OS",
                  "%Y/%m/%d %H:%M:%OS","%Y-%m-%d %H:%M","%Y/%m/%d %H:%M","%Y-%m-%d","%Y/%m/%d")
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  
  o <- c()
  
  for (.f in .dtFormats) {
    o <- c(o,all(!is.na(as.POSIXct(x,format=.f))))
  }
  if (any(o)) {
    .dtFormats[which(o)[1]]
  } else NA
  
}
#----------

.bind_rows <- function(x) {
  o <- data.frame()
  
  .x <- sapply(x,ncol)
  if (length(unique(.x)) > 1) {
    .tmp <- a[[which.max(.x)]]
    .tmp[1,] <- NA
    for (i in seq_along(x)) {
      .o <- .tmp
      .o[1,colnames(x[[i]])] <- x[[i]]
      o <- rbind(o,.o)
    }
  } else {
    for (i in seq_along(x)) {
      o <- rbind(o,x[[i]])
    }
  }
  
  o
}
#------
.is.POSIXct <- function(x) {
  inherits(x,"POSIXct")
}
#--------
.get_match <- function(x, y, several=TRUE,case_sensitive=FALSE) {
  if (!case_sensitive) {
    .x <- tolower(x)
    .y <- tolower(y)
    .yy <- try(match.arg(.x,.y,several.ok = several),silent = TRUE)
    if (!inherits(.yy,'try-error')) {
      o <- c()
      for (n in .yy) {
        w <- which(.y == n) 
        o <- c(o,y[w])
      }
      o
    } else NA
    
  } else {
    xx <- try(match.arg(x,y,several.ok = several),silent = TRUE)
    if (!inherits(xx,'try-error')) xx
    else NA
  }
}
#--------

.file_info <- function(x) {
  
  if (basename(x) == x || dirname(x) == '.') {
    .dir <- '.'
  } else {
    .dir <- dirname(x)
    if (.dir == getwd()) .dir <- '.'
  }
  #-------
  w <- strsplit(basename(x),'\\.')[[1]]
  if (length(w) > 1) {
    .filename <- paste(w[-length(w)],collapse = '_')
    .extension <- w[length(w)]
  } else {
    .filename <- basename(x)
    .extension <- NA
  }
  #-----
  
  list(path=.dir,filename=.filename,extension=.extension)
  
}
#--------
.trim_chr <- function(x) trimws(as.character(x))
#------
.pick_col <- function(df, candidates) {
  if (is.null(df) || !is.data.frame(df)) return(NA_character_)
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) hit[1] else NA_character_
}
#----
.charN <- function(x,space=TRUE) {
  if (missing(x) || is.null(x)) return(NULL)
  x <- as.character(x)
  if (x %in% c(""," ","  ","   ","    ")) return(0)
  #-----
  x <- .trim(x)
  x <- strsplit(x,'')[[1]]
  if (space) length(x)
  else length(x[x != " "])
}
#-----
.wordN <- function(x) {
  if (missing(x) || is.null(x)) return(NULL)
  x <- as.character(x)
  
  if (length(x) > 1) return(sapply(1:length(x),function(i) .wordN(x[i])))
  
  if (x %in% c(""," ","  ","   ","    ")) return(0)
  #-----
  x <- .trim(x)
  #----
  if (.charN(x) > 0) {
    x <- strsplit(x,'')[[1]]
    j <- 1
    for (i in 1:length(x)) {
      if (x[i] == " ") j <- j + 1
    }
    return(j)
  }
  return(0)
}
#-----
.word <- function(x,start=NULL,end=NULL) {
  if (missing(x) || is.null(x)) return(NULL)
  x <- as.character(x)
  if (length (x) == 0 || x %in% c(""," ","  ","   ","    ")) return(NULL)
  #-----
  x <- .trim(x)
  #----
  
  .w <- c()
  x <- strsplit(x,'')[[1]]
  while (length(x) > 0) {
    i <- 1
    j <- TRUE
    while (j) {
      if (x[i] %in% c(" ",",",";",":",".") ) {
        .x <- x[1:(i-1)]
        .x <- .x[!.x %in% c(',',':',';',' ','.')]
        if (length(.x) > 0) {
          .w <- c(.w,paste(.x,collapse = ''))
        }
        x <- x[-c(1:(i-1))]
        while (length(x) > 0 && x[1] == ' ') x <- x[-1]
        i <- 1
      } else if (i == length(x)) {
        .w <- c(.w,paste(x,collapse = ''))
        x <- c()
        j <- FALSE
      }
      i <- i + 1
    }
  }
  #-------
  if (length(.w) > 0) {
    if (!is.null(start) && is.numeric(start) && start != 0) {
      if (start < 0) {
        start <- (-1 * as.integer(start)) 
        if (start > length(.w)) start <- length(.w)
        end <- length(.w)
        start <- length(.w) - start + 1
      } else {
        if (start <= length(.w)) {
          if (is.null(end)) end <- start
          else {
            if (is.numeric(end) && end <= length(.w)) {
              if (end < start) {
                warning('The "end" argument cannot be lower than "start"...!')
                end <- start
              }
            } else end <- start
          }
        } else {
          start <- 1
          end <- length(.w)
        }
      }
      
      
    } else {
      start <- 1
      end <- length(.w)
    }
    #----
    return(.w[start:end])
  }
  
  return(.w)
}
#------
# if a label has two parts with "_", it is replaced with space:
.pretty_label <- function(x) {
  x <- as.character(x)
  x <- gsub("_", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  
  # OPTIONAL: if you want to drop the word "wild"
  # x <- gsub("^wild\\s+", "", x)
  
  # "a, b, and c"
  if (length(x) == 1) return(x)
  if (length(x) == 2) return(paste(x, collapse = " and "))
  paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
}

