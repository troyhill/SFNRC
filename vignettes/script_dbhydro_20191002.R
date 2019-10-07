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
dat.d4e <- getHydro(stns = targetStns, # c(as.character(dfrInBay$stn), structs),
          parameter_list = c("flow", "head_water", "salinity", "temperature", "tail_water",
          "stage", "rainfall", "precipitation", "ppt"), data_shape = "wide")
head(dat.d4e)

targParams    <- c("PHOSPHATE, TOTAL AS P", "TURBIDITY")
phos.d4e <- getWQ(stns = targetStns, target_analytes = paste(targParams, collapse = "|"))
head(phos.d4e)


# Implement WRTDS ---------------------------------------------------------
### using DBHydro
# cl <- parallel::makePSOCKcluster(detectCores(logical = FALSE) - 2)
# parallel::registerDoParallel(cl)
targAnalyte <- "PHOSPHATE, TOTAL AS P"
tp.dfe <- lapply(c("S333", "S12D"), function(stnSelect) 
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                                 wq_data = phos.d4e[phos.d4e$date > "2000-01-01",], flow_data = dat.d4e[dat.d4e$date > "2000-01-01", ])))
# parallel::stopCluster(cl)

### set water year (default is 10, 12)
tp.dfe <- lapply(tp.dfe, setPA, paStart = 10, paLong = 12)

# diagnostics
lapply(tp.dfe, plotConcPred)

ggplot(data = dat.d4e[(dat.d4e$stn %in% "S12D") & (dat.d4e$year == "2017"), ], aes(x = head_water, y = flow, col = mo)) + theme_classic() + geom_point() + labs(y = "flow (cfs)", x = "stage (ft NGVD29)", title = "S12D (2017)")



# 
# 
# plot(flow ~ head_water, data = dat.d4e[dat.d4e$stn %in% "S12D", ], cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
#      xlim = c(5, 15), ylim = c(0, 6000))
# plot(flow ~ head_water, data = dat.d4e[dat.d4e$stn %in% "S333", ], cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
#      xlim = c(5, 15), ylim = c(0, 6000))
# plot(flow ~ head_water, data = dat.d4e[dat.d4e$stn %in% "S151", ], cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
#      xlim = c(5, 15), ylim = c(0, 6000))
# plot(flow ~ head_water, data = dat.d4e[dat.d4e$stn %in% "S197", ], cex = 0.5, las = 1, pch = 19)
# 
# 
# ### temporal coherence in S12D
# plot(flow ~ head_water, data = dat.d4e[dat.d4e$stn %in% "S12D", ], cex = 0.5, las = 1, pch = 19, 
#      ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", main = "S12D (1963-2019)",
#      xlim = c(5, 15), ylim = c(0, 6000))
# plot(flow ~ head_water, cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
#      xlim = c(5, 15), ylim = c(0, 2500), main = "S12D (2016)",
#      data = dat.d4e[(dat.d4e$stn %in% "S12D") & (dat.d4e$year == "2016"), ])
# plot(flow ~ head_water, cex = 0.5, las = 1, pch = 19, ylab = "flow (cfs)", xlab = "stage (ft NGVD29)", 
#      xlim = c(5, 15), ylim = c(0, 2500), main = "S12D (2017)",
#      data = dat.d4e[(dat.d4e$stn %in% "S12D") & (dat.d4e$year == "2017"), ])




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

allDat.d4e <- join_all(list(dat.d4e[(dat.d4e$date > "2000-01-01") & (dat.d4e$stn == "S333"), ], phos.d4e[(phos.d4e$date > "2000-01-01") & (phos.d4e$stn == "S333") & (phos.d4e$param == targAnalyte),]))
nrow(allDat.d4e)
head(allDat.d4e)
nrow(allDat.d4e) - sum(duplicated(allDat.d4e$date))

allDat.db <- join_all(list(db.dat[(db.dat$date > "2000-01-01") & (db.dat$stn == "S333"),], tp.long[(tp.long$date > "2000-01-01") & (tp.long$stn == "S333") & (tp.long$param == "PHOSPHATETOTALASP"), ]), type = "left") 
nrow(allDat.db)
head(allDat.db)
sum(duplicated(allDat.db$date))
allDat.db$date[!allDat.db$date %in% allDat.d4e$date] # most recent date is only one missing from DataForEver. 


### compare convertToEgret output
db.egret <- convertToEgret(stn = "S333", target_analyte = targAnalyte, 
               wq_data = tp.long[tp.long$date > "2000-01-01", ], flow_data = db.dat[db.dat$date > "2000-01-01",])
# Some Sample dates do not have corresponding flow data. Not all EGRET functions will work correctly. Appears due to "2014-12-31 Q = NA" but this also appears in DFE data except there are no TP data for that date in DFE
newTP <- tp.long[(tp.long$date > "2000-01-01") & (tp.long$stn == "S333") & (tp.long$param == "PHOSPHATETOTALASP"), ]
newFlow <- db.dat[(db.dat$date > "2000-01-01") & (db.dat$stn == "S333"), ]
newTP$date[!newTP$date %in% newFlow$date]


dfe.egret <- convertToEgret(stn = "S333", target_analyte = targAnalyte, 
               wq_data = phos.d4e[phos.d4e$date > "2000-01-01",], flow_data = dat.d4e[dat.d4e$date > "2000-01-01", ])
newTP.dfe   <- phos.d4e[(phos.d4e$date > "2000-01-01") & (phos.d4e$stn == "S333") & (phos.d4e$param == "PHOSPHATETOTALASP"), ]
newFlow.dfe <- dat.d4e[(dat.d4e$date > "2000-01-01") & (dat.d4e$stn == "S333"), ]

# There are 4658 duplicated Sample dates. sum(duplicated(allDat.d4e$date))


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
