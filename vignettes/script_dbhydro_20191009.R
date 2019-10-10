library(SFNRC)
library(ggplot2)

### USGS 02319000 WITHLACOOCHEE RIVER NEAR PINETTA, FLA.. 1931-2019
usgsStn <- read.delim(file = "/home/thill/RDATA/data_usgs02319000_20190507", skip = 15)
head(usgsStn)
plot(X12s.3 ~ X12s.2, data = usgsStn, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
     xlim = c(5, 15), ylim = c(0, 6000), pch = 19, cex = 0.6, las = 1, main = "USGS 02319000 (1931-2019)")

# targetStns <- c("S20", "S20F", "S20G",
# "S21A", "S21", "S22", "S25", "S25A", "S25B", "S26", "S27", "S28",
# "G58", "S700", "G93", "S123", "S197",
# "S333", "S12A", "S12B", "S12C", "S12D", "S151")

targetStns <- c("S333", "S12D", "S151", 
                # no/inadequate water quality data: "S20F", "S21A", "S21", "S123", "S22", "S24", "S336", "S344", "S194", "S120", "S121", "S334", "S179",  "G93", "S380", "S26", "S30", 
                # inadequate flow data: "S332", "S177",  "S8", "S32","G56", 
               "S18C", "S7", "S39",
               "S12C", "S179"
                )


# DataForEver workflow ----------------------------------------------------

### requires linux and SFNRC access
dat.dfe.long <- getHydro(stns = targetStns, # c(as.character(dfrInBay$stn), structs),
                    parameter_list = c("flow", "head_water" #, "salinity", "temperature", "tail_water", "stage", "rainfall", "precipitation", "ppt"
                                       ), data_shape = "long")
dat.dfe <- getHydro(stns = targetStns, # c(as.character(dfrInBay$stn), structs),
                    parameter_list = c("flow", "head_water" #, "salinity", "temperature", "tail_water", "stage", "rainfall", "precipitation", "ppt"
                    ), data_shape = "wide")
head(dat.dfe)

targParams    <- c("PHOSPHATE, TOTAL AS P", "KJELDAHL NITROGEN, TOTAL",  "AMMONIA-N", "NITRATE", "SODIUM", "TURBIDITY", "HARDNESS AS CACO3")
phos.dfe <- getWQ(stns = targetStns, target_analytes = paste(targParams, collapse = "|"), 
                  matricesToExclude = "analyte_free_water|unknown")
phos.dfe <- phos.dfe[phos.dfe$matrix == "surface water", ] # drops 11000 rows
head(phos.dfe)
 
### Data assessment
ggplot(dat.dfe, aes(y = flow, x = date)) + geom_point(size = 0.5) + theme_classic() + facet_grid( stn ~ .)
ggplot(phos.dfe, aes(y = value, x = date)) + geom_point(size = 0.5) + theme_classic() + facet_grid(param ~ stn, scales = "free_y")

### number of observations per parameter
### stations with <200 obs: S334 (all params except TP); S151 (Ca, Na)
startDate <- "1980-01-01"

wq.summary <- ddply(phos.dfe[phos.dfe$date >= startDate,], .(param, stn), summarise, 
      firstObs = head(datetime, 1),
      lastObs  = tail(datetime, 1),
      count    = sum(!is.na(value)))
wq.qualified <- wq.summary[(wq.summary$count >= 200), ]
wq.excluded  <- wq.summary[(wq.summary$count < 200) , ]

flow.summary <- ddply(dat.dfe.long[dat.dfe.long$param %in% "flow", ], .(param, stn), summarise, 
      firstObs = head(date, 1),
      lastObs  = tail(date, 1),
      count    = sum(!is.na(value)),
      days     = round(lastObs - firstObs) + 1)
flow.qualified <- flow.summary[flow.summary$firstObs <= startDate, ]
flow.excluded  <- flow.summary[flow.summary$firstObs > startDate, ]



merged.data <- join_all(list(dat.dfe[(dat.dfe$date >= startDate), ], phos.dfe[phos.dfe$date >= startDate,]), by = c("stn", "date"))
noFlowData <- merged.data[(merged.data$param %in% "PHOSPHATE, TOTAL AS P") & is.na(merged.data$flow), ]
nrow(noFlowData)
unique(noFlowData$stn)

noFlowData.all <- merged.data[is.na(merged.data$flow), ]

### replace NAs with zeroes for relevant data
# dat.dfe$flow[paste0(dat.dfe$stn, dat.dfe$date) %in% paste0(noFlowData.all$stn, noFlowData.all$date)] <- 0
# or, remove sample data from dates with no flow data
phos.dfe <- phos.dfe[!paste0(phos.dfe$stn, phos.dfe$date) %in% paste0(noFlowData.all$stn, noFlowData.all$date), ]

# Implement WRTDS ---------------------------------------------------------
### using DBHydro
# cl <- parallel::makePSOCKcluster(detectCores(logical = FALSE) - 2)
# parallel::registerDoParallel(cl)
targAnalyte <- "PHOSPHATE, TOTAL AS P"
tp.dfe <- lapply(wq.qualified$stn[wq.qualified$param %in% targAnalyte], function(stnSelect)
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                                 wq_data = phos.dfe[phos.dfe$date >= startDate,], flow_data = dat.dfe[dat.dfe$date >= startDate, ])))
# parallel::stopCluster(cl)

# tp151 <- phos.dfe[(phos.dfe$date >= startDate) & (phos.dfe$stn %in% "S151") & (phos.dfe$param %in% targAnalyte),]
# flow151 <- dat.dfe[(dat.dfe$date >= startDate) & (dat.dfe$stn %in% "S151"), ]
# a <- modelEstimation(convertToEgret(stn = "S151", target_analyte = targAnalyte, 
#                                wq_data = tp151, flow_data = flow151))

### set water year (default is 10, 12)
tp.dfe <- lapply(tp.dfe, setPA, paStart = 10, paLong = 12)
# save("tp.dfe", "phos.dfe", "dat.dfe", file = "tpBackup_20191010.RData")


# diagnostics
lapply(tp.dfe, plotConcPred)
lapply(tp.dfe, plotFluxPred)
lapply(tp.dfe, plotResidPred)
lapply(tp.dfe, plotResidQ)

# figure 1
lapply(tp.dfe, plotConcHist, concMax = 0.12, yearStart = 1978)
lapply(tp.dfe, plotFluxHist, fluxMax = 0.16, yearStart = 1978)

plotConcHist(tp.dfe[[2]])

s151 <- tp.dfe[[2]]
head(s151$Sample)

ggplot(data = dat.dfe[(dat.dfe$stn %in% "S12D") & (dat.dfe$year == "2017"), ], aes(x = head_water, y = flow, col = mo)) + theme_classic() + geom_point() + labs(y = "flow (cfs)", x = "stage (ft NGVD29)", title = "S12D (2017)")

### Try using stage instead of flow
targAnalyte <- "PHOSPHATE, TOTAL AS P"
flowDat <- dat.dfe[dat.dfe$date > "2000-01-01", ]
names(flowDat)[3:4] <- c("actual", "flow")
flowDat$flow <- exp(flowDat$flow)
tp.dfe <- lapply(c("S333", "S12D"), function(stnSelect) 
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                                 wq_data = phos.dfe[phos.dfe$date > "2000-01-01",], flow_data = flowDat)))
lapply(tp.dfe, plotConcPred)


# 
# 
# plot(flow ~ head_water, data = dat.dfe[dat.dfe$stn %in% "S12D", ], cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
#      xlim = c(5, 15), ylim = c(0, 6000))
# plot(flow ~ head_water, data = dat.dfe[dat.dfe$stn %in% "S333", ], cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
#      xlim = c(5, 15), ylim = c(0, 6000))
# plot(flow ~ head_water, data = dat.dfe[dat.dfe$stn %in% "S151", ], cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
#      xlim = c(5, 15), ylim = c(0, 6000))
# plot(flow ~ head_water, data = dat.dfe[dat.dfe$stn %in% "S197", ], cex = 0.5, las = 1, pch = 19)
# 
# 
# ### temporal coherence in S12D
# plot(flow ~ head_water, data = dat.dfe[dat.dfe$stn %in% "S12D", ], cex = 0.5, las = 1, pch = 19, 
#      ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", main = "S12D (1963-2019)",
#      xlim = c(5, 15), ylim = c(0, 6000))
# plot(flow ~ head_water, cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
#      xlim = c(5, 15), ylim = c(0, 2500), main = "S12D (2016)",
#      data = dat.dfe[(dat.dfe$stn %in% "S12D") & (dat.dfe$year == "2016"), ])
# plot(flow ~ head_water, cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
#      xlim = c(5, 15), ylim = c(0, 2500), main = "S12D (2017)",
#      data = dat.dfe[(dat.dfe$stn %in% "S12D") & (dat.dfe$year == "2017"), ])




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
### Tons of messages: "Removing Sample data that does not have corresponding flow data"
# cl <- parallel::makePSOCKcluster(detectCores(logical = FALSE) - 2)
# parallel::registerDoParallel(cl)
targAnalyte <- "PHOSPHATE, TOTAL AS P"
tp <- lapply(c("S333", "S12D"), function(stnSelect) 
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                                 wq_data = tp.long[tp.long$date > "2000-01-01", ], flow_data = db.dat[db.dat$date > "2000-01-01",])))
# parallel::stopCluster(cl)



### set water year (default is 10, 12)
tp <- lapply(tp, setPA, paStart = 10, paLong = 12)

# diagnostics
lapply(tp, plotConcPred)



# Comparison of DBHYDRO and DFE data --------------------------------------
### Problem: DBHYDRO data submitted to ModelEstimation(convertToEgret) gives a barrage of warnings: "Removing Sample data that does not have corresponding flow data"
### only difference detected is that dataforever has some duplicated dates in TP dataset

allDat.dfe <- join_all(list(dat.dfe[(dat.dfe$date > "2000-01-01") & (dat.dfe$stn == "S333"), ], phos.dfe[(phos.dfe$date > "2000-01-01") & (phos.dfe$stn == "S333") & (phos.dfe$param == targAnalyte),]))
nrow(allDat.dfe)
head(allDat.dfe)
nrow(allDat.dfe) - sum(duplicated(allDat.dfe$date))

allDat.db <- join_all(list(db.dat[(db.dat$date > "2000-01-01") & (db.dat$stn == "S333"),], tp.long[(tp.long$date > "2000-01-01") & (tp.long$stn == "S333") & (tp.long$param == "PHOSPHATETOTALASP"), ]), type = "left") 
nrow(allDat.db)
head(allDat.db)
sum(duplicated(allDat.db$date))
allDat.db$date[!allDat.db$date %in% allDat.dfe$date] # most recent date is only one missing from DataForEver. 


### compare convertToEgret output
db.egret <- convertToEgret(stn = "S333", target_analyte = targAnalyte, 
               wq_data = tp.long[tp.long$date > "2000-01-01", ], flow_data = db.dat[db.dat$date > "2000-01-01",])
# Some Sample dates do not have corresponding flow data. Not all EGRET functions will work correctly. Appears due to "2014-12-31 Q = NA" but this also appears in DFE data except there are no TP data for that date in DFE
newTP <- tp.long[(tp.long$date > "2000-01-01") & (tp.long$stn == "S333") & (tp.long$param == "PHOSPHATETOTALASP"), ]
newFlow <- db.dat[(db.dat$date > "2000-01-01") & (db.dat$stn == "S333"), ]
newTP$date[!newTP$date %in% newFlow$date]


dfe.egret <- convertToEgret(stn = "S333", target_analyte = targAnalyte, 
               wq_data = phos.dfe[phos.dfe$date > "2000-01-01",], flow_data = dat.dfe[dat.dfe$date > "2000-01-01", ])
newTP.dfe   <- phos.dfe[(phos.dfe$date > "2000-01-01") & (phos.dfe$stn == "S333") & (phos.dfe$param == "PHOSPHATETOTALASP"), ]
newFlow.dfe <- dat.dfe[(dat.dfe$date > "2000-01-01") & (dat.dfe$stn == "S333"), ]

# There are 4658 duplicated Sample dates. sum(duplicated(allDat.dfe$date))


### DBHYDRO has fewer TP samples than DataForEver?
nrow(db.egret$Sample)
#[1] 4869
nrow(dfe.egret$Sample)
# 5506

dfe.egret$Sample$Date[!dfe.egret$Sample$Date %in% db.egret$Sample$Date] # all DFE dates appear in DBHydro
db.egret$Sample$Date[! db.egret$Sample$Date %in% dfe.egret$Sample$Date] # dates that appear in DFE but not DBHYDRO dataset. possibly autosampler data?
# c("2003-11-17", "2006-01-23", "2006-01-24", "2006-01-25" )
duplicated(dfe.egret$Sample$Date)

newTP[newTP$date %in% c("2003-11-17", "2006-01-23", "2006-01-24", "2006-01-25" ), ]



### look for missing water quality Data
dbkey <- "91487"
startDate <- "20060121"
endDate <-  "20060125"
  urlDL <- paste0("http://my.sfwmd.gov/dbhydroplsql/web_io.report_process?v_period=uspec&v_start_date=", 
                  startDate, "&v_end_date=", endDate, "&v_report_type=format6&v_target_code=file_csv&v_run_mode=onLine&v_js_flag=Y&v_db_request_id=5603897&v_where_clause=&v_dbkey=", 
                  dbkey, "&v_os_code=Unix&v_interval_count=5&v_cutover_datum=1")
  
  
s333.dat <- getDBHYDRO(stn = "S333", parameters = c("PHOSPHATE, TOTAL AS P"))
