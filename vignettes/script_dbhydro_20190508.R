library(SFNRC)
library(EGRET)
library(dataRetrieval)

usgsStn <- read.delim(file = "/home/thill/RDATA/data_usgs02319000_20190507", skip = 15)
head(usgsStn)
plot(X12s.3 ~ X12s.2, data = usgsStn, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)")

targetStns <- c("S20", "S20F", "S20G",
"S21A", "S21", "S22", "S25", "S25A", "S25B", "S26", "S27", "S28",
"G58", "S700", "G93", "S123", "S197",
"S333", "S12A", "S12B", "S12C", "S12D", "S151")

dat <- getHydro(stns = targetStns, # c(as.character(dfrInBay$stn), structs),
parameter_list = c("flow", "head_water", "salinity", "temperature", "tail_water",
"stage", "rainfall", "precipitation", "ppt"), data_shape = "wide")

head(dat)


plot(flow ~ head_water, data = dat[dat$stn %in% "S12D", ], cex = 0.5, las = 1, pch = 19)
plot(flow ~ head_water, data = dat[dat$stn %in% "S333", ], cex = 0.5, las = 1, pch = 19)
plot(flow ~ head_water, data = dat[dat$stn %in% "S151", ], cex = 0.5, las = 1, pch = 19)
plot(flow ~ head_water, data = dat[dat$stn %in% "S197", ], cex = 0.5, las = 1, pch = 19)



# water quality data  -----------------------------------------------------
### pull from DBHydro
tp <- getDBHYDRO(stn = "S333")
head(tp)
head(dat)
mrgd <- join_all(list(tp, dat), by = c("stn", "date", "year", "mo", "day"))
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
