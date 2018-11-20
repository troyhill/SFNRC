### Script generates interpolated maps and rasters for Biscayne Bay parameters




# kriging annual values --------------------------------------------------------------

# http://rspatial.org/analysis/rst/4-interpolation.html ------------------
fin2 <- dcast(finDat[, -c(4, 7)], stn + date + year ~ param, fun.aggregate = mean) # long to wide
fin2 <- seas(fin2, timeCol = "date", wetSeas = c("May", "Sep"))
fin2$seas2  <- paste0(fin2$waterYr,"-", fin2$seas)

# Create interpolated maps of annual geometric means ----------------------
### Data are not finalized, so this is just laying out the conceptual approach. Interpolate for the whole bay and then clip for regions.
agm <- plyr::ddply(fin2[, -c(2)], plyr::.(year, stn), plyr::numcolwise(geoMean))
names(agm) <- gsub(x = names(agm), pattern = " |,", replacement = "")
names(agm) <- gsub(x = names(agm), pattern = "-|[+]", replacement = ".")


finDat.coords <- plyr::join_all(list(agm, as.data.frame(masterCoords)), by = "stn")
finDat.coords <- finDat.coords[!is.na(finDat.coords$long), ]

coordinates(finDat.coords) <- c("long", "lat")
proj4string(finDat.coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

sitesInBay <- sp::over(finDat.coords, polygonLayer)
sitesInBay <- finDat.coords[complete.cases(sitesInBay), ]




## re-run this loop when underlying data or shapefile change. otherwise, build data using csvs
for (j in 3:(length(names(sitesInBay)) - 1)) {
  for (i in 1:length(unique(sitesInBay$year))) {
    targYear  <- unique(sitesInBay$year)[i]
    targParam <- names(sitesInBay)[j] # "SALINITY"
    targDat   <- sitesInBay
    lims      <- as.numeric(quantile(data.frame(targDat@data[, targParam]), c(0, 1), na.rm = TRUE))
    rasterFileAddr  <- paste0(rasterFolder, "/", targParam, targYear, ".tif")
    csvMeanFileAddr <- paste0(rasterFolder, "/mean/", targParam, targYear)
    csvSDFileAddr   <- paste0(rasterFolder, "/sd/",   targParam, targYear)
    csvSub20FileAddr<- paste0(rasterFolder, "/sub20/",   targParam, targYear)
    # if (!rasterFileAddr %in% list.files(rasterFolder, full.names = TRUE)) {
          tryCatch({
      dat.l <- interp(inputData = targDat, paramCol = targParam, year = targYear, yearCol = "year", plotZLims = lims, 
                          returnRas = TRUE, exportRaster = TRUE, fileName = rasterFileAddr, minDataPoints = minPoints, mapLayer = polygonLayer,
                          interpMethod = interpMeth)
      write.csv(x = t(extract(dat.l , polygonLayer, fun = mean, na.rm = TRUE)), file = csvMeanFileAddr, row.names = FALSE)
      write.csv(x = t(extract(dat.l , polygonLayer, fun = sd, na.rm = TRUE)), file =  csvSDFileAddr, row.names = FALSE)
      if(targParam %in% "SALINITY") {
        write.csv(x = t(extract(dat.l, polygonLayer, fun = function(x, ...) ecdf(x)(20), na.rm = TRUE)), file =  csvSub20FileAddr, row.names = FALSE)
      }}, error = function(e) {
            cat("error in outer loop for ", targParam,
                targYear, ":", conditionMessage(e), "\n")
        })
  }
}




# Create interpolated maps of geometric means ----------------------
agm <- plyr::ddply(fin2[, -c(grep(x = names(fin2), pattern = "date|water"))], plyr::.(seas2, seas, stn), plyr::numcolwise(geoMean))
names(agm) <- gsub(x = names(agm), pattern = " |,", replacement = "")
names(agm) <- gsub(x = names(agm), pattern = "-|[+]", replacement = ".")


finDat.coords <- plyr::join_all(list(agm, as.data.frame(masterCoords)), by = "stn")
finDat.coords <- finDat.coords[!is.na(finDat.coords$long), ]

coordinates(finDat.coords) <- c("long", "lat")
proj4string(finDat.coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

sitesInBay <- sp::over(finDat.coords, polygonLayer)
sitesInBay <- finDat.coords[complete.cases(sitesInBay), ]

### re-run this loop when underlying data or shapefile change. otherwise, build data using csvs
for (j in 4:(length(names(sitesInBay)))) {
  for (i in 1:length(unique(sitesInBay$seas2))) {
    targYear  <- unique(sitesInBay$seas2)[i]
    targParam <- names(sitesInBay)[j] # "SALINITY"
    targDat   <- sitesInBay
    lims      <- as.numeric(quantile(data.frame(targDat@data[, targParam]), c(0, 1), na.rm = TRUE))

    rasterFileAddr   <- paste0(rasterFolder, "/seas/", targParam, targYear, ".tif")
    csvMeanFileAddr  <- paste0(rasterFolder, "/seas/mean/", targParam, targYear)
    csvSDFileAddr    <- paste0(rasterFolder, "/seas/sd/", targParam, targYear)
    csvsub20FileAddr <- paste0(rasterFolder, "/seas/sub20/", targParam, targYear)
    # if (!rasterFileAddr %in% list.files(paste0(rasterFolder, "seas/"),  full.names = TRUE)) { # if you don't want to re-process/overwrite files
          tryCatch({
      seas.l <- interp(inputData = targDat, paramCol = targParam, year = targYear, yearCol = "seas2", plotZLims = lims,
                       returnRas = TRUE, exportRaster = TRUE, fileName = rasterFileAddr, minDataPoints = minPoints, mapLayer = polygonLayer,
                       interpMethod = interpMeth)
      write.csv(x = t(extract( seas.l, polygonLayer, fun = mean, na.rm = TRUE)), file = csvMeanFileAddr, row.names = FALSE) # record results
      write.csv(x = t(extract( seas.l, polygonLayer, fun = sd, na.rm = TRUE)), file =  csvSDFileAddr, row.names = FALSE)
      if(targParam %in% "SALINITY") {
        write.csv(x = t(extract( seas.l, polygonLayer, fun = function(x, ...) ecdf(x)(20), na.rm = TRUE)), file =  csvsub20FileAddr, row.names = FALSE)
      }

      }, error = function(e) {
            cat("error in outer loop for ", targParam,
                targYear, ":", conditionMessage(e), "\n")
        })
  }}
