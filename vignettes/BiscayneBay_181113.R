## ----setup, include=FALSE, echo=FALSE------------------------------------

### TODO: amend bnpMod to include bay subregions used for regulatory water quality criteria, or include additional polygon files in package



if(!require(knitr)){
  install.packages("knitr", repos='http://cran.us.r-project.org')
}
if(!require(rmarkdown)){
  install.packages("rmarkdown", repos='http://cran.us.r-project.org')
}
if(!require(plyr)){
  install.packages("plyr", repos='http://cran.us.r-project.org')
}
if(!require(reshape2)){
  install.packages("reshape2", repos='http://cran.us.r-project.org')
}
if(!require(ggplot2)){
  install.packages("ggplot2", repos='http://cran.us.r-project.org')
}
if(!require(scales)){
  install.packages("scales", repos='http://cran.us.r-project.org')
}
if(!require(maptools)){ # this package may be unnecessary
  install.packages("scales", repos='http://cran.us.r-project.org')
}
if(!require(rgeos)){
  install.packages("rgeos", repos='http://cran.us.r-project.org')
}
if(!require(deldir)){
  install.packages("deldir", repos='http://cran.us.r-project.org')
}
if(!require(doMC)){
  install.packages("doMC", repos='http://cran.us.r-project.org')
}
if(!require(foreach)){
  install.packages("foreach", repos='http://cran.us.r-project.org')
}
if(!require(zoo)){
  install.packages("zoo", repos='http://cran.us.r-project.org')
}

# then load the package:
library(doMC)
library(foreach)
library(knitr)
library(rmarkdown)
library(plyr)
library(zoo)
library(reshape2)
library(ggplot2)
library(scales)
library(SFNRC)

library(maptools)
library(rgeos)
library(sp)
library(dismo)
library(deldir)
library(gstat)
library(rgdal)
library(raster)

knitr::opts_chunk$set(echo = TRUE, comment=NA)

## ---- echo = FALSE, include=FALSE----------------------------------------
# if the NitrogenUptake2016 package isn't installed, use devtools to do so:
# devtools::install_github("troyhill/NitrogenUptake2016")

# set some constants
rasterFolder <- "/opt/physical/troy/RDATA/output/WQrasters_NN"
todaysDate <- substr(as.character(Sys.time()), 1, 10)
pointSize <- 2 # for ggplot graphics
pd <- pd2 <- position_dodge(1.2)
pd3 <- position_dodge(0.8)
grayColor <- 0.55
fig2Col   <- "gray55"
minPoints <- 8 # minimum number of sampling points required for interpolation
polygonLayer <- bnpMod # shapefile to be used for subsetting data, interpolation, etc.
interpMeth <- "nearest neighbor"

compareParams <- c("AMMONIA-N", "NITRATE+NITRITE-N", "TEMP",
                   "SALINITY", "PH, FIELD", "CHLOROPHYLL-A", "TURBIDITY", "DISSOLVED OXYGEN", 
                   "PHOSPHATE, TOTAL AS P", "PHOSPHATE, ORTHO AS P")

## compiled water quality data - regenerate this.
summary(finDat)  # bay water quality data - merged with latest DERM data
summary(wqDat)   # canal water quality data (DERM data not likely to be relevant)

## hydrology data
summary(hydDat)  # canal inflows


registerDoMC(cores = 0) # register multicore parallel backend with the foreach package.
useMC = TRUE # sent to ddply calls, whether multiple cores should be used



### to re-do kriging:
# source("/home/thill/RDATA/git-repos/SFNRC/vignettes/BiscayneBay_generate_181113.R")


    ### summarize data after compiling rasters for each parameter
    # https://gis.stackexchange.com/questions/270988/raster-data-extract-to-polygon-rcode
    # possible approach would be to create a raster stack (https://gis.stackexchange.com/questions/29118/how-to-find-the-average-raster-value-of-an-area-defined-by-a-shapefile-using-r)
    # get mean and area <20 psu for bay subregions 
    ### example for entire raster:
    # test     <- ldply(dat.l, function(x) cellStats(x, "mean"))


### load & process csvs from folder
meanList        <- list.files(paste0(rasterFolder, "/mean"), full.names = TRUE)
adat       <- lapply(X = meanList, FUN = read.csv) # build a list with all rasters in the folder
adat       <- do.call("rbind", adat)
names(adat) <- gsub(x = names(adat), pattern = "V", replacement = "")
adat$".id" <- gsub(x = meanList, pattern = paste0(rasterFolder, "/mean/"), replacement = "")

sdList             <- list.files(paste0(rasterFolder, "/sd"), full.names = TRUE)
adat.sd       <- lapply(X = sdList, FUN = read.csv) # build a list with all rasters in the folder
adat.sd       <- do.call("rbind", adat.sd)
names(adat.sd) <- gsub(x = names(adat.sd), pattern = "V", replacement = "")
adat.sd$".id" <- gsub(x = sdList, pattern = paste0(rasterFolder, "/sd/"), replacement = "")
                              

sub20List          <- list.files(paste0(rasterFolder, "/sub20"), full.names = TRUE)
adat.20       <- lapply(X = sub20List, FUN = read.csv) # build a list with all rasters in the folder
adat.20       <- do.call("rbind", adat.20)
names(adat.20) <- gsub(x = names(adat.20), pattern = "V", replacement = "")
adat.20$".id" <- gsub(x = sub20List, pattern = paste0(rasterFolder, "/sub20/"), replacement = "")
 

### to make a long dataset with columns: param, year, loc, mean, sd, ecdf20
combd <- join_all(list(
  melt(adat,    id.vars = ".id", variable.name = "subRegion", value.name = "mean"),
  melt(adat.sd, id.vars = ".id", variable.name = "subRegion", value.name = "sd"),
  melt(adat.20, id.vars = ".id", variable.name = "subRegion", value.name = "propSub20")
))



combd$param <- substr(combd[, ".id"], 1, nchar(combd[, ".id"]) - 4)
combd$yr    <- as.numeric(substr(combd[, ".id"], nchar(combd[, ".id"]) - 3, nchar(combd[, ".id"])))

### change subregion from region number to BOX_CODE (can easily be replaced by another column)
combd$subRegion <- as.character(polygonLayer@data$BOX_CODE)[as.numeric(as.character(combd$subRegion))]



## ----Subregion map, fig.width = 4, fig.height = 4, message = FALSE, echo=FALSE----
par(mar = c(4,4,0.5,0.5))
cols.sub <- colors()[grep(x = colors(), pattern = "yellow|cyan|red|blue|green|brown")]
cols <- cols.sub[sample(x =  length(cols.sub), size = length(unique(polygonLayer@data$BOX_CODE)))]
cols <- cols.sub[sample(x =  length(cols.sub), size = length(unique(polygonLayer@data$BOX_CODE)))]

plot(polygonLayer, col = cols)
pointLabel(coordinates(polygonLayer), labels = polygonLayer$BOX_CODE)


## ----parameter trends, fig.width = 6, fig.height = 4, message = FALSE, echo=FALSE----
ggplot(combd, aes(y = mean, x = yr, col = subRegion)) + #geom_point(size  = 1, position = pd3) + 
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd), position = pd3, fatten = 1) + theme_classic() + facet_grid(param ~ ., scales = "free_y") + ylab("") 


## ----interpolated wet season data, echo = FALSE, include = FALSE, message = FALSE----
### same as above but for wet season.
### memory constraints may require saving rasters to disk or instantly summarizing & deleting them




# r <- raster("/opt/physical/troy/RDATA/output/WQrasters/seas/DISSOLVEDOXYGEN2017-wet.tif")
# persp(theta = 0, phi = 30, 2*r, border = "red", scale = FALSE)

### Load & process rasters from folder [this will change to a do.call/read.csv approach]
meanList        <- list.files(paste0(rasterFolder, "/seas/mean"), full.names = TRUE)
adat.seas       <- lapply(X = meanList, FUN = read.csv) # build a list with all rasters in the folder
adat.seas       <- do.call("rbind", adat.seas)
names(adat.seas) <- gsub(x = names(adat.seas), pattern = "V", replacement = "")
adat.seas$".id" <- gsub(x = meanList, pattern = paste0(rasterFolder, "/seas/mean/"), replacement = "")

sdList             <- list.files(paste0(rasterFolder, "/seas/sd"), full.names = TRUE)
adat.seas.sd       <- lapply(X = sdList, FUN = read.csv) # build a list with all rasters in the folder
adat.seas.sd       <- do.call("rbind", adat.seas.sd)
names(adat.seas.sd) <- gsub(x = names(adat.seas.sd), pattern = "V", replacement = "")
adat.seas.sd$".id" <- gsub(x = sdList, pattern = paste0(rasterFolder, "/seas/sd/"), replacement = "")
                              

sub20List          <- list.files(paste0(rasterFolder, "/seas/sub20"), full.names = TRUE)
adat.seas.20       <- lapply(X = sub20List, FUN = read.csv) # build a list with all rasters in the folder
adat.seas.20       <- do.call("rbind", adat.seas.20)
names(adat.seas.20) <- gsub(x = names(adat.seas.20), pattern = "V", replacement = "")
adat.seas.20$".id" <- gsub(x = sub20List, pattern = paste0(rasterFolder, "/seas/sub20/"), replacement = "")
                              


### to make a long dataset with columns: param, year, loc, mean, sd, ecdf20

combd.seas <- join_all(list(
  melt(adat.seas, id.vars = '.id', variable.name = "subRegion", value.name = "mean"),
  melt(adat.seas.sd, id.vars = '.id', variable.name = "subRegion", value.name = "sd") ,
  melt(adat.seas.20, id.vars = '.id', variable.name = "subRegion", value.name = "propSub20")
))

combd.seas$param <- substr(combd.seas[, '.id'], 1, nchar(combd.seas[, '.id']) - 8)
combd.seas$yr    <- as.numeric(substr(combd.seas[, 1], nchar(combd.seas[, 1]) - 7, nchar(combd.seas[, '.id']) - 4))
combd.seas$seas  <- substr(combd.seas[, '.id'], nchar(combd.seas[, '.id']) - 2, nchar(combd.seas[, '.id']))


### change subregion from region number to subRegion (can easily be replaced by another column)
combd.seas$subregion <- as.character(polygonLayer@data$BOX_CODE)[as.numeric(as.character(combd.seas$subRegion))]



## ----annual wet/dry season trends, fig.width = 7, fig.height = 4, message = FALSE, echo=FALSE----
ggplot(combd.seas, aes(y = mean, x = yr, col = subregion)) + #geom_point(size  = 0.25, position = pd3) + 
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd), position = pd3, fatten = 1) + theme_classic() + facet_grid(param ~ seas, scales = "free_y") + ylab("") 


## ----annual water inflows from canals, include=FALSE, echo=FALSE---------
# Prep canal water quality data for merging with flows --------------------------------
# wqCanal <- stats::reshape(wqDat[wqDat$param %in% compareParams, c("stn", "date", "param", "value")], idvar = c("stn", "date") ) # # limit to relevant params and reshape dataset

wqCanal <- stats::reshape(wqDat[wqDat$param %in% compareParams, c("stn", "date", "param", "value")], idvar = c("stn", "date"), 
            timevar = "param", direction = "wide")
names(wqCanal) <- gsub(x = names(wqCanal), pattern = "value.", 
            replacement = "")

# Comparison of water inflows and salinity --------------------------------
### TODO: assign structures to subregions based on proximity to polygons in bnp
# https://stackoverflow.com/questions/26308426/how-do-i-find-the-polygon-nearest-to-a-point-in-r
structs <- c("S22", "S25", "S25A", "S25B", "S26", # central
             "S27", "S28", "S29", # from SWIM plan - north biscayne bay
             "G58", "S700", "G93", # new structures since SWIM plan?
             "S123",  "S21A", "S21", "S20", "S20F", "S20G", "S197") # south bay "MOWRY C"?
biscHyd <- hydDat[hydDat$stn %in% structs, ]
### add coordinates for polygon assignment
biscHyd <- join_all(list(biscHyd, as.data.frame(masterCoords)), by = "stn")
biscHyd <- join_all(list(biscHyd, wqCanal), by = c("stn", "date"))



biscHyd$kacft <- biscHyd$flow * 2.29569e-5 / 1000 * 60 * 60 * 24 # convert cfs to thousand acre feet per day
biscHyd$m3d   <- biscHyd$flow * 2446.58 # convert cfs to cubic meters per day
biscHyd       <- seas(biscHyd, timeCol = "date", wetSeas = c("May", "Sep"))              # aggregate up to month or year

coordinates(biscHyd) <- c("long", "lat")
proj4string(biscHyd) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(bnp)
# plot(biscHyd, add = TRUE)
# 

## For each point, find name of nearest polygon (polygonLayer)
for (i in seq_along(unique(biscHyd$stn))) {
  biscHyd$subRegion[biscHyd$stn %in% unique(biscHyd$stn)[i]] <- suppressWarnings(polygonLayer$BOX_CODE[which.min(gDistance(masterCoords[masterCoords$stn == unique(biscHyd$stn)[i],], polygonLayer, byid=TRUE))])
  biscHyd$hectares[biscHyd$stn %in% unique(biscHyd$stn)[i]]  <- suppressWarnings(polygonLayer$HECTARES[which.min(gDistance(masterCoords[masterCoords$stn == unique(biscHyd$stn)[i],], polygonLayer, byid=TRUE))])
}

biscHyd.proj  <- biscHyd
biscHyd       <- as.data.frame(biscHyd)
biscHyd$m3dha <- biscHyd$m3d / biscHyd$hectares # m3 flow / day / hectare

###########################
#### back to water quality: interpolate linearly between samples. This is an area for improvement
###########################
biscHyd2 <- biscHyd 
biscHyd2[, grep(x = names(biscHyd2), pattern = "DISSOLVED"):grep(x = names(biscHyd2), pattern = "ORTHO.AS.P")] <- zoo::na.approx(biscHyd[,grep(x = names(biscHyd2), pattern = "DISSOLVED"):grep(x = names(biscHyd2), pattern = "ORTHO.AS.P")], na.rm = FALSE)


biscHyd2$salt.flux.kg <- biscHyd2$SALINITY * biscHyd2$m3d # kg salt/m3 * m3/day = kg salt/day
biscHyd2$N.flux.kg    <- (biscHyd2$NITRATE.NITRITE.N + biscHyd2$AMMONIA.N) * 1000  * biscHyd2$m3d # g/m3 * 1000g/kg * m3/day = kg N/day
biscHyd2$H.flux.kg    <- 10^-(biscHyd2$PH..FIELD) * 1.01 * biscHyd2$m3d  # (kmol/m3) *  (1.01 kg/kmol) * m3/d = kg/day
biscHyd2$P.flux.kg    <- biscHyd2$SALINITY * biscHyd2$m3d # kgP/m3 * m3/day = kg P/day
###########################



### plot structures and receiving basins in same color
polygonLayer$cols[polygonLayer$BOX_CODE %in% biscHyd.proj$subRegion] <- cols[as.numeric(as.factor(polygonLayer$BOX_CODE[polygonLayer$BOX_CODE %in% biscHyd.proj$subRegion]))]
biscHyd.proj$cols <- cols[as.numeric(as.factor(biscHyd.proj$subRegion))]


# plot(polygonLayer)
# plot(biscHyd.proj, add = TRUE)


plot(polygonLayer, col = polygonLayer$cols)
  # pointLabel(coordinates(polygonLayer), labels = polygonLayer$BOX_CODE)
plot(biscHyd.proj, add = TRUE, col = biscHyd.proj$cols)
  # pointLabel(coordinates(biscHyd.proj[1:length(biscHyd.proj$stn),]), labels = biscHyd.proj$stn) # can't figure out how to label points!
text(biscHyd.proj, labels = biscHyd.proj$stn, offset = 0.5, pos = 2)

### annual flux from each structure
# a <- ddply(biscHyd2, .(stn, year), summarise, kacft = sum(kacft, na.rm = TRUE), salt.flux.kg = sum(salt.flux.kg, na.rm = TRUE),             N.flux.kg = sum(N.flux.kg, na.rm = TRUE), H.flux.kg = sum(H.flux.kg, na.rm = TRUE),                   P.flux.kg = sum(P.flux.kg, na.rm = TRUE)) 
# ddply(a[a$year %in% 1980:1989, ], .(stn), summarise, mean(kacft, na.rm = TRUE)) # some values differ dramatically from table on page 49 of 1995 SWIM plan summary
# ddply(biscHyd2, .(subRegion, year), summarise, kacft = sum(kacft, na.rm = TRUE), salt.flux.kg = sum(salt.flux.kg, na.rm = TRUE),             N.flux.kg = sum(N.flux.kg, na.rm = TRUE), H.flux.kg = sum(H.flux.kg, na.rm = TRUE),                   P.flux.kg = sum(P.flux.kg, na.rm = TRUE)) 


### spatial scale: whole bay.
flow.mnth <- ddply(biscHyd2, .(year, mo), summarise, m3 = sum(flow, na.rm = TRUE), m3ha = sum(m3dha, na.rm = TRUE))
flow.mnth$yrMo <- paste0(flow.mnth$year,"-", flow.mnth$mo)
flow.mnth$date <- zoo::as.yearmon(flow.mnth$yrMo, "%Y-%m")

flow.yr   <- ddply(biscHyd2, .(year), summarise, m3 = sum(flow, na.rm = TRUE), m3ha = sum(m3dha, na.rm = TRUE),
                   salt.flux.kg = sum(salt.flux.kg, na.rm = TRUE),             N.flux.kg = sum(N.flux.kg, na.rm = TRUE),
                   H.flux.kg = sum(H.flux.kg, na.rm = TRUE),                   P.flux.kg = sum(P.flux.kg, na.rm = TRUE))
flow.yr$year <- as.numeric(as.character(flow.yr$year))
flow.seas <- ddply(biscHyd2, .(seas, waterYr), summarise, m3 = sum(flow, na.rm = TRUE), m3ha = sum(m3dha, na.rm = TRUE),
                   salt.flux.kg = sum(salt.flux.kg, na.rm = TRUE),             N.flux.kg = sum(N.flux.kg, na.rm = TRUE),
                   H.flux.kg = sum(H.flux.kg, na.rm = TRUE),                   P.flux.kg = sum(P.flux.kg, na.rm = TRUE))
flow.seas$waterYr <- as.numeric(as.character(flow.seas$waterYr))


# 
# ggplot(flow.mnth, aes(y = m3 / 1e6, x = date)) + geom_line() + theme_classic() + xlab("") + ylab("inflow to bay (1e6 m3 per month)")
# ggplot(flow.yr, aes(y = m3 / 1e6, x = year)) + geom_line() + theme_classic() + xlab("calendar year") + ylab("inflow to bay (1e6 m3 per year)")
# ggplot(flow.seas, aes(y = m3 / 1e6, x = waterYr)) + geom_line() + theme_classic() + facet_grid(seas ~ .) + xlab("water year") + ylab("inflow to bay (1e6 m3 per season)")


### spatial scale: bay subregions
flow.mnth <- ddply(biscHyd2, .(year, mo, subRegion), summarise, m3 = sum(flow, na.rm = TRUE), ha = mean(hectares, na.rm = TRUE))
flow.mnth$yrMo <- paste0(flow.mnth$year,"-", flow.mnth$mo)
flow.mnth$date <- zoo::as.yearmon(flow.mnth$yrMo, "%Y-%m")

flow.yr   <- ddply(biscHyd2, .(year, subRegion), summarise, m3 = sum(flow, na.rm = TRUE), ha = mean(hectares, na.rm = TRUE),
                   salt.flux.kg = sum(salt.flux.kg, na.rm = TRUE),             N.flux.kg = sum(N.flux.kg, na.rm = TRUE),
                   H.flux.kg = sum(H.flux.kg, na.rm = TRUE),                   P.flux.kg = sum(P.flux.kg, na.rm = TRUE))
flow.yr$year <- as.numeric(as.character(flow.yr$year))
flow.seas <- ddply(biscHyd2, .(seas, waterYr, subRegion), summarise, m3 = sum(flow, na.rm = TRUE), ha = mean(hectares, na.rm = TRUE),
                   salt.flux.kg = sum(salt.flux.kg, na.rm = TRUE),             N.flux.kg = sum(N.flux.kg, na.rm = TRUE),
                   H.flux.kg = sum(H.flux.kg, na.rm = TRUE),                   P.flux.kg = sum(P.flux.kg, na.rm = TRUE))
flow.seas$waterYr <- as.numeric(as.character(flow.seas$waterYr))


# 
# ggplot(flow.mnth, aes(y = m3 / 1e6, x = date, col = subRegion)) + geom_line() + theme_classic() + facet_grid(. ~ subRegion) + xlab("") + ylab("inflow to bay (1e6 m3 per month)")
# ggplot(flow.mnth, aes(y = m3ha / 1e6, x = date, col = subRegion)) + geom_line() + theme_classic() + facet_grid(. ~ subRegion) + xlab("") + ylab("inflow to bay (1e6 m3 per ha per month)")
# 
# ggplot(flow.yr, aes(y = m3 / 1e6, x = year, col = subRegion)) + geom_line() + theme_classic() + facet_grid(. ~ subRegion) + xlab("calendar year") + ylab("inflow to bay (1e6 m3 per year)")
# ggplot(flow.yr, aes(y = m3ha / 1e6, x = year, col = subRegion)) + geom_line() + theme_classic() + facet_grid(. ~ subRegion) + xlab("calendar year") + ylab("inflow to bay (1e6 m3 per ha per year)")
# 
# ggplot(flow.seas, aes(y = m3 / 1e6, x = waterYr, col = subRegion)) + geom_line() + theme_classic() + facet_grid(seas ~ subRegion) + xlab("water year") + ylab("inflow to bay (1e6 m3 per season)")
# ggplot(flow.seas, aes(y = m3ha / 1e6, x = waterYr, col = subRegion)) + geom_line() + theme_classic() + facet_grid(seas ~ subRegion) + xlab("water year") + ylab("inflow to bay (1e6 m3 per ha per season)")
# 




## ----Figure - flow to bay, fig.width = 6, fig.height = 4, message = FALSE, include=FALSE, echo=FALSE----

ggplot(flow.seas, aes(y = m3 / 1e6, x = waterYr, col = subRegion)) + geom_line() + theme_classic() + facet_grid(seas ~ subRegion) + xlab("water year") + ylab("inflow to bay (1e6 m3 per season)")
ggplot(flow.seas, aes(y = m3 / ha, x = waterYr, col = subRegion)) + geom_line() + theme_classic() + facet_grid(seas ~ subRegion) + xlab("water year") + ylab("inflow to bay (m3 per ha per season)")

## ----annual solute delivery from canals, include=FALSE, echo=FALSE-------
soluteDF <- melt(flow.seas, id.vars = c("seas", "waterYr", "subRegion", "ha"))

ggplot(soluteDF, aes(y = value, x = waterYr, col = subRegion)) + geom_line() + theme_classic() + facet_grid(variable ~ seas, scales = "free") + xlab("water year") + ylab("flux to bay (per season)")
ggplot(soluteDF, aes(y = value / ha, x = waterYr, col = subRegion)) + geom_line() + theme_classic() + facet_grid(variable ~ seas, scales = "free") + xlab("water year") + ylab("flux to bay (per ha per season)")



## ----solute delivery vs subregion concentrations, include=FALSE, echo=FALSE----
### combine wide datasets
aves   <- dcast(combd.seas, seas + yr + subregion ~ param, value.var = "mean")
sds    <- dcast(combd.seas, seas + yr + subregion ~ param, value.var = "sd")
prop20 <- dcast(combd.seas, seas + yr + subregion ~ param, value.var = "propSub20")

names(aves)[c(2:3,4:ncol(aves))]     <- c("waterYr", "subRegion", paste0(names(aves)[c(4:ncol(aves))], ".mean"))
names(sds)[c(2:3,4:ncol(sds))]       <- c("waterYr", "subRegion", paste0(names(sds)[c(4:ncol(sds))], ".sd"))
names(prop20)[c(2:3,4:ncol(prop20))] <- c("waterYr", "subRegion", paste0(names(prop20)[c(4:ncol(prop20))], ".sub20"))



df <- join_all(list(flow.seas, aves, sds, prop20), by = c("seas", "waterYr", "subRegion"))

plot(df[, c(4, 6:11)])

ggplot(df, aes(y = SALINITY.sub20, x = m3, col = subRegion)) + geom_point() + theme_classic() 
ggplot(df[df$SALINITY.sub20 > 0.001, ], aes(y = SALINITY.sub20, x = m3, col = subRegion)) + geom_point() + theme_classic() + geom_smooth(method = "lm")


