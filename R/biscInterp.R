
#' Make interpolated maps of Biscayne Bay
#'
#' @param inputData dataframe with water quality data. Function works best with output of \code{dfe.wq() or dfe.hydro()} using \code{wide = TRUE}.
#' @param paramCol character string, name of column containing the relevant water quality data
#' @param yearCol character string, name of column indicating relevant temporal divisions
#' @param year time period to be used for the plot (subset from \code{yearCol}, above)
#' @param returnRas TRUE/FALSE; should a raster layer be returned to the global environment
#' @param exportRaster TRUE/FALSE; should a raster layer be saved to disk?
#' @param fileName if a raster layer is exported, this argument sets the file address and name
#' @param BISCmap SpatialPolygonDataFrame layer with outline of Biscayne Bay (data included in SNFRC package)
#' @param exportPlot TRUE/FALSE; should the displayed plot be saved to disk?
#' @param plotName if the plot is exported, this argument sets the file address and name
#' @param plotWidth width of the plot
#' @param plotHeight height of the plot
#' @param plotRes resolution of the plot
#' @param minDataPoints minimum number of data points considered necessary for interpolation
#' @param plotZLims range for colors plotted in figure. This is useful for standardizing when multiple plots are being produced
#' @param vgModelType model type passed to \code{fit.variogram()} to interpolate data. See \code{?fit.variogram} for more details
#'
#' @return plot, raster layer, and/or raster data
#' @export
#'
#' @examples  
#' \dontrun{
#' fin2 <- reshape2::dcast(finDat[, -c(4, 7)], stn + date + year ~ param) # long to wide
#' agm <- plyr::ddply(fin2[, -c(2)], plyr::.(year, stn), plyr::numcolwise(geoMean))
#' names(agm) <- gsub(x = names(agm), pattern = " |,", replacement = "")
#' names(agm) <- gsub(x = names(agm), pattern = "-|[+]", replacement = ".")
#' finDat.coords <- plyr::join_all(list(agm, masterCoords), by = "stn")
#' finDat.coords <- finDat.coords[!is.na(finDat.coords$long), ]
#' coordinates(finDat.coords) <- c("long", "lat")
#' proj4string(finDat.coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#' 
#' sitesInBay <- sp::over(finDat.coords, bnp)
#' sitesInBay <- finDat.coords[complete.cases(sitesInBay), ]
#' 
#' biscInterp(inputData = sitesInBay,
#'     paramCol = "SALINITY", year = "2016")
#'     
#'     ### store raster layer in working environment
#' biscRas <- biscInterp(inputData = sitesInBay,
#'     paramCol = "SALINITY", year = "2016", returnRas = TRUE)
#'     }
#' 
#' 
#' @importFrom gstat gstat
#' @importFrom gstat variogram
#' @importFrom gstat fit.variogram
#' @importFrom raster predict
#' @importFrom raster raster
#' @importFrom raster writeRaster
#' @importFrom raster aggregate
#' @importFrom raster intersect
#' @importFrom raster plot
#' @importFrom dismo voronoi 
#' @importFrom methods as 
#' @importFrom grDevices terrain.colors
#' @importFrom grDevices colorRampPalette
#' @importFrom grDevices dev.off 
#' @importFrom grDevices png 
#' @importFrom graphics par 
#' @importFrom graphics plot
#' 
biscInterp <- function(inputData, # inputData = finDat.coords[(finDat.coords@data$stn %in% finalSites), ], 
                       paramCol     = "SALINITY", yearCol = "year", year      = "2016", 
                       returnRas    = FALSE, # option to return raster as an object
                       exportRaster = FALSE, fileName     = "NA.tif", BISCmap = SFNRC::bnp,
                       exportPlot   = FALSE, plotName     = "NA.png",
                       plotWidth    = 4,     plotHeight   = 5, plotRes = 200,
                       plotZLims    = "range", minDataPoints = 2,
                       vgModelType  = c("Exp", "Mat", "Gau", "Sph", "Ste")
) {
  ## make sure bnp and pts have same projections. Add bnp to package
  
  pts <- inputData[!is.na(inputData@data[, paramCol]) & (inputData@data[, yearCol] %in% year), ]
  cat(length(unique(pts$stn)), "stations with", paramCol, "data in", year, "       ")
  if (nrow(pts) > minDataPoints) { # only interpolate if there's more than x data points.
    
    if (plotZLims %in% "range") {
      plotZLims <- range(pts@data[, paramCol], na.rm = TRUE)
    }
    
        
      ### create color mapping for plot
      #Create a function to generate a continuous color palette
      numberOfLevels <- 10
      earthTones <- grDevices::terrain.colors(n = numberOfLevels, alpha = 1)
      rbPal      <- grDevices::colorRampPalette(c(earthTones[numberOfLevels], earthTones[1]))
      #This adds a column of color values
      # based on the y values
      
      # brks <- seq(plotZLims[1], plotZLims[2], by = 0.1) # this gets v complex given different scales of different params
      # nb <- length(brks)-1 
      # cols <- rev(terrain.colors(nb))
      
      pts$Col    <- rbPal(numberOfLevels)[as.numeric(cut(as.data.frame(pts[, paramCol])[, paramCol], breaks = numberOfLevels))] # not on the same scale as the plot...
      # pts$Col    <- round(as.data.frame(pts[, paramCol])[, paramCol], 1)
      ###
      
      # pts@data
      

      
      ### inverse distance weighted interpolation
      # http://rspatial.org/analysis/rst/4-interpolation.html
      gs  <- gstat::gstat(formula = get(paramCol) ~ 1, locations = pts, nmax = 5, set = list(idp = 0)) 
      v   <- gstat::variogram(gs) # generate variogram
      # fve <- gstat::fit.variogram(v, gstat::vgm(psill = max(v$gamma)*0.9, model = vgModelType, range = max(v$dist) / 2, nugget = 0))
      fve <- gstat::fit.variogram(v, gstat::vgm(vgModelType), fit.kappa = TRUE) # https://www.r-spatial.org/r/2016/02/14/gstat-variogram-fitting.html
      ### look into automap::autoKrige. coordinate system issues are obstacle. See also more generally: https://gis.stackexchange.com/questions/147660/strange-spatial-interpolation-results-from-ordinary-kriging
      ### TODO: report specs on interpolation
      ### function doesn't handle convergence errors well
      
      ### kriging using the variogram
      template.ras <- raster::raster(BISCmap, res = c(0.001, 0.001))
      blank.ras    <- methods::as(template.ras, "SpatialGrid") # sf::as may work
      
      ### predict values
      k  <- gstat::gstat(formula = get(paramCol) ~ 1, location = pts, model = fve)
      
      setTimeLimit(30) # set time limit to prevent this from running forever
      try(ras_pred <- raster::raster(raster::predict(k, blank.ras), layer = 1), silent = TRUE)
      ras_pred <- raster::mask(ras_pred, BISCmap)
      setTimeLimit()
      
      
      ### TODO: if kriging interpolation fails, use IDW or nearest neighbor interpolation considering nmax neighbors
      if (!exists("ras_pred") || sum(!is.na(ras_pred@data@values)) < 1) {
        # # ### Alternative: nearest neighbor. 
        # # ### Create nearest neighbor polygons
        # vlocs         <- dismo::voronoi(pts)
        # # plot(vlocs)
        # bnp_ag        <- raster::aggregate(BISCmap)
        # bnp_intsct    <- raster::intersect(vlocs, bnp_ag)
        # # spplot(bnp_intsct, paramCol, col.regions = rev(get_col_regions()))
        # 
        # ### rasterize
        # fl.ras       <- raster::raster(bnp, nrows = 1000, ncols = 1000)
        # ras_pred     <- raster::rasterize(bnp_intsct, fl.ras, paramCol)
        # # plot(ras_pred)
        # # plot(BISCmap, add = TRUE)
        # # raster::plot(pts, bg = pts$Col, add = TRUE, pch = 21, cex = 0.5, zlim = plotZLims)
        
        ### Alternative: use inverse distance weighted interpolation
        ras_pred <- raster::raster(raster::predict(gs, blank.ras), layer = 1)
        ras_pred <- raster::mask(ras_pred, BISCmap)
        # plot(ras_pred)
        # plot(BISCmap, add = TRUE)
        # raster::plot(pts, bg = pts$Col, add = TRUE, pch = 21, cex = 0.5, zlim = plotZLims)
      }
      
      
      
      #################
      ### plot/export
      #################
      if (exportPlot) {
        grDevices::png(filename = plotName, width = plotWidth, height = plotHeight, units = "in", res = plotRes)
      } 
      graphics::par(mar = c(4,4,1,0.5), fig = c(0,1,0,1))
      raster::plot(ras_pred, main = paste0(paramCol, " ", year), zlim = plotZLims, las = 1)
      raster::plot(BISCmap, add = TRUE)
      raster::plot(pts, bg = pts$Col, add = TRUE, pch = 21, cex = 0.5, zlim = plotZLims)
      if (exportPlot) {
        grDevices::dev.off()
      } 
      ### export raster
      if (exportRaster) {
        raster::writeRaster(ras_pred, fileName, overwrite = TRUE)
      }
      
      if (returnRas) {
        invisible(ras_pred)
      }
  }
}


