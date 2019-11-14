library(SFNRC)
library(EGRET)
library(dataRetrieval)

### USGS 02319000 WITHLACOOCHEE RIVER NEAR PINETTA, FLA.. 1931-2019
usgsStn <- read.delim(file = "/home/thill/RDATA/data_usgs02319000_20190507", skip = 15)
head(usgsStn)
plot(X12s.3 ~ X12s.2, data = usgsStn, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
     xlim = c(5, 15), ylim = c(0, 6000), pch = 19, cex = 0.6, las = 1, main = "USGS 02319000 (1931-2019)")

# targetStns <- c("S20", "S20F", "S20G",
# "S21A", "S21", "S22", "S25", "S25A", "S25B", "S26", "S27", "S28",
# "G58", "S700", "G93", "S123", "S197",
# "S333", "S12A", "S12B", "S12C", "S12D", "S151")

targetStns <- c("S333", "S12A", "S12B", "S12C", "S12D", "S151")

### requires linux and SFNRC access
dat <- getHydro(stns = targetStns, # c(as.character(dfrInBay$stn), structs),
parameter_list = c("flow", "head_water", "salinity", "temperature", "tail_water",
"stage", "rainfall", "precipitation", "ppt"), data_shape = "wide")

head(dat)


plot(flow ~ head_water, data = dat[dat$stn %in% "S12D", ], cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
     xlim = c(5, 15), ylim = c(0, 6000))
plot(flow ~ head_water, data = dat[dat$stn %in% "S333", ], cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
     xlim = c(5, 15), ylim = c(0, 6000))
plot(flow ~ head_water, data = dat[dat$stn %in% "S151", ], cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
     xlim = c(5, 15), ylim = c(0, 6000))
plot(flow ~ head_water, data = dat[dat$stn %in% "S197", ], cex = 0.5, las = 1, pch = 19)


### temporal coherence in S12D
plot(flow ~ head_water, data = dat[dat$stn %in% "S12D", ], cex = 0.5, las = 1, pch = 19, 
     ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", main = "S12D (1963-2019)",
     xlim = c(5, 15), ylim = c(0, 6000))
plot(flow ~ head_water, cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
     xlim = c(5, 15), ylim = c(0, 2500), main = "S12D (2016)",
     data = dat[(dat$stn %in% "S12D") & (dat$year == "2016"), ])
plot(flow ~ head_water, cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
     xlim = c(5, 15), ylim = c(0, 2500), main = "S12D (2017)",
     data = dat[(dat$stn %in% "S12D") & (dat$year == "2017"), ])


# Pull hydro data from DBHYDRO --------------------------------------------
getDBkey(stn = "s333", type = "FLOW") 
getDBkey(stn = "s12d", type = "FLOW")
db.dat <- do.call(rbind, lapply(c("91487", "01310"), getDBHYDROhydro))
head(db.dat)




# water quality data  -----------------------------------------------------
### pull from DBHydro
phosDat <- do.call(rbind, lapply(c("s333", "S12d"), getDBHYDRO, parameters = c("PHOSPHATE, TOTAL AS P|TURBIDITY"))) # using parameter filter
head(phosDat)

tp <- getDBHYDRO(stn = "S333")
head(tp)
head(dat)
mrgd <- join_all(list(dat, tp), by = c("stn", "date", "year", "mo", "day"))
head(mrgd)

plot(flow ~ date, data = mrgd)
plot(PHOSPHATETOTALASP ~ date, data = mrgd)

# Implement WRTDS ---------------------------------------------------------
### using DBHydro
wqTemp <- replaceBDLs(data = wqDat[(wqDat$param %in% targAnalyte), ])

cl <- makePSOCKcluster(nCores)
registerDoParallel(cl)
tp <- lapply(targStns, function(stnSelect) 
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                                 wq_data = wqTemp, flow_data = hydDat), run.parallel = TRUE))
stopCluster(cl)

### set water year (default is 10, 12)
tp <- lapply(tp, setPA, paStart = WaterYearStart, paLong = 12)

# diagnostics
lapply(tp, plotConcPred)
