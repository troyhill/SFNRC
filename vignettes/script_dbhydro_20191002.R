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


# DataForEver workflow ----------------------------------------------------

### requires linux and SFNRC access
dat <- getHydro(stns = targetStns, # c(as.character(dfrInBay$stn), structs),
          parameter_list = c("flow", "head_water", "salinity", "temperature", "tail_water",
          "stage", "rainfall", "precipitation", "ppt"), data_shape = "wide")
head(dat)

targParams    <- c("PHOSPHATE, TOTAL AS P", "TURBIDITY")
phosDat <- getWQ(stns = targetStns, target_analytes = paste(targParams, collapse = "|"))
head(phosDat)


# Implement WRTDS ---------------------------------------------------------
### using DBHydro
cl <- parallel::makePSOCKcluster(detectCores(logical = FALSE) - 2)
parallel::registerDoParallel(cl)
targAnalyte <- "PHOSPHATE, TOTAL AS P"
tp.dfe <- lapply(c("S333", "S12D"), function(stnSelect) 
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                                 wq_data = phosDat[phosDat$date > "2000-01-01",], flow_data = dat[dat$date > "2000-01-01", ])))
parallel::stopCluster(cl)

### set water year (default is 10, 12)
tp.dfe <- lapply(tp.dfe, setPA, paStart = 10, paLong = 12)

# diagnostics
lapply(tp.dfe, plotConcPred)

# 
# 
# plot(flow ~ head_water, data = dat[dat$stn %in% "S12D", ], cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
#      xlim = c(5, 15), ylim = c(0, 6000))
# plot(flow ~ head_water, data = dat[dat$stn %in% "S333", ], cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
#      xlim = c(5, 15), ylim = c(0, 6000))
# plot(flow ~ head_water, data = dat[dat$stn %in% "S151", ], cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
#      xlim = c(5, 15), ylim = c(0, 6000))
# plot(flow ~ head_water, data = dat[dat$stn %in% "S197", ], cex = 0.5, las = 1, pch = 19)
# 
# 
# ### temporal coherence in S12D
# plot(flow ~ head_water, data = dat[dat$stn %in% "S12D", ], cex = 0.5, las = 1, pch = 19, 
#      ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", main = "S12D (1963-2019)",
#      xlim = c(5, 15), ylim = c(0, 6000))
# plot(flow ~ head_water, cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
#      xlim = c(5, 15), ylim = c(0, 2500), main = "S12D (2016)",
#      data = dat[(dat$stn %in% "S12D") & (dat$year == "2016"), ])
# plot(flow ~ head_water, cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
#      xlim = c(5, 15), ylim = c(0, 2500), main = "S12D (2017)",
#      data = dat[(dat$stn %in% "S12D") & (dat$year == "2017"), ])




# DBHYDRO workflow --------------------------------------------------------

# Pull hydro data from DBHYDRO --------------------------------------------
getDBkey(stn = "s333", type = "FLOW") 
getDBkey(stn = "s12d", type = "FLOW")
db.dat <- do.call(rbind, lapply(c("91487", "01310"), getDBHYDROhydro))
head(db.dat)
names(db.dat)[names(db.dat) %in% "value"]  <- "flow"



# water quality data  -----------------------------------------------------
### pull from DBHydro
phosDat <- do.call(rbind, lapply(c("s333", "S12d"), getDBHYDRO, parameters = c("PHOSPHATE, TOTAL AS P|TURBIDITY"))) # using parameter filter
head(phosDat)

tp.long <- reshape2::melt(phosDat[, c("stn", "date", "PHOSPHATETOTALASP", "TURBIDITY")], id.vars = c("stn", "date"), variable.name = "param")
head(tp.long)
tp.long$units <- "units" # need to set units


# Implement WRTDS ---------------------------------------------------------
### using DBHydro
cl <- parallel::makePSOCKcluster(detectCores(logical = FALSE) - 2)
parallel::registerDoParallel(cl)
targAnalyte <- "PHOSPHATE, TOTAL AS P"
tp <- lapply(c("S333", "S12D"), function(stnSelect) 
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                                 wq_data = tp.long[tp.long$date > "2000-01-01", ], flow_data = db.dat[db.dat$date > "2000-01-01",])))
parallel::stopCluster(cl)

### set water year (default is 10, 12)
tp <- lapply(tp, setPA, paStart = 10, paLong = 12)

# diagnostics
lapply(tp, plotConcPred)

