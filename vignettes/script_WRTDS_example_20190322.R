### Weighted regression using time, discharge, and season
### Application of EGRET tools to DataForEver data
library(plyr)
library(SFNRC)

# Meta-analysis -----------------------------------------------------------

targStns    <- c("S333", "S12A", "S12B", "S12C", "S12D", "S151") #, "S343", "S332")
targAnalyte <- "PHOSPHATE, TOTAL AS P"


test <- lapply(targStns, function(stnSelect) 
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                 wq_data = wqDat, flow_data = hydDat)))
### set water year (default is 10, 12)
test <- lapply(test, setPA, paStart = 04, paLong = 12)
plotConcHist(eList)

# figure 1
lapply(test, plotConcHist, concMax = 0.04)
lapply(test, plotFluxHist, fluxMax = 0.016)



# Change in concentration at different discharge ------------------------


yearEnd <- 2000
yearStart <- 1970
q1 <- 3  # 100 cfs
q2 <- 14 # 500 cfs
q3 <- 28 # 1000 cfs
plotConcTimeSmooth(test[[1]], q1, q2, q3, centerDate, yearStart, yearEnd, legendTop = 0.01, legendLeft = 1980)

lapply(test, plotConcTimeSmooth, q1, q2, q3, centerDate, yearStart, yearEnd, legendTop = 0.01, legendLeft = 1980)

### this looks great
lapply(test, plotConcTimeSmooth, q1, q2, q3, centerDate = "01-01", yearStart, yearEnd, 
       legendTop = 0.06, legendLeft = yearStart, concMax = 0.06)
lapply(test, plotConcTimeSmooth, q1, q2, q3, centerDate = "05-01", yearStart, yearEnd, 
       legendTop = 0.16, legendLeft = yearStart, concMax = 0.06)
lapply(test, plotConcTimeSmooth, q1, q2, q3, centerDate = "07-01", yearStart, yearEnd, 
       legendTop = 0.16, legendLeft = yearStart, concMax = 0.06)
lapply(test, plotConcTimeSmooth, q1, q2, q3, centerDate = "10-01", yearStart, yearEnd, 
       legendTop = 0.16, legendLeft = yearStart, concMax = 0.06)



# Contour plots -----------------------------------------------------------
# mdiff     <- 0.04
# yearStart <- 1985
# yearEnd   <- 2015
# 
# lapply(test, plotDiffContours, yearStart, yearEnd, 
#        qBottom = 0.1, qTop = 2000, qUnit = 1, maxDiff = mdiff)

# plotConcTimeSmooth(eList, q1, q2, q3, centerDate = centerDate, yearStart = yearStart, yearEnd = yearEnd)
clevel    <- seq(0, 0.15, 0.01)
clevel2    <- seq(0, 0.05, 0.005)
maxDiff   <- 0.1
yearStart1 <- 2009
yearEnd1   <- 2019

qBottom   <- 0.1
qTop      <- 2500

lapply(test[-2], plotContours, 
       yearStart = yearStart1, yearEnd = yearEnd1, qBottom = qBottom, qTop =qTop, 
       contourLevels = clevel2, qUnit=1)
yearStart2 <- 1979
yearEnd2   <- 1989
lapply(test[-2], plotContours, 
       yearStart = yearStart2, yearEnd = yearEnd2, qBottom = qBottom, qTop =qTop, 
       contourLevels = clevel, qUnit=1)


lapply(test[-2], plotContours, 
       yearStart = yearStart2, yearEnd = yearEnd2, qBottom = qBottom, qTop =qTop, 
       contourLevels = clevel2, qUnit=1)
lapply(test[-2], plotContours, 
       yearStart = yearStart1, yearEnd = yearEnd2, qBottom = qBottom, qTop =qTop, 
       contourLevels = clevel2, qUnit=1)

lapply(test[-2], plotDiffContours, 
       year0 = yearStart1, year1 = yearEnd1, qBottom = qBottom, qTop =qTop, 
       maxDiff = maxDiff, qUnit=1)

# plots difference between two years
lapply(test[-2], plotDiffContours, 
       year0 = 1980, year1 = yearEnd1, qBottom = qBottom, qTop =qTop, 
       maxDiff = maxDiff, qUnit=1)




# lapply(test, fluxBiasMulti)
# lapply(test, plotConcPred)
# lapply(test, plotFluxPred)
# lapply(test, boxResidMonth)
# lapply(test, multiPlotDataOverview)
# lapply(test, tableChange)

annualSeries <- lapply(test, makeAnnualSeries)
monthlyResults <- lapply(test, calculateMonthlyResults)


plot15(test[[4]], yearStart = yearStart, yearEnd = yearEnd)
tableResults(test[[1]])
tableFlowChange(test[[1]])
# plotDiffContours(test[[1]], yearStart, yearEnd, qBottom, qTop, qUnit = 1, maxDiff = mdiff)
lapply(test, tableFlowChange)


# Discharge-concentration relationships -----------------------------------

date1 <- "1985-09-01"
date2 <- "1997-09-01"
date3 <- "2010-09-01"
qBottom <- 0.2
qTop    <- 28
lapply(test, plotConcQSmooth, date1, date2, date3, qBottom, qTop, 
                concMax= 0.06,legendTop = 0.85)

# Discharge-time plots ----------------------------------------------------

# Multi-line plots:
q1 <- 100
q2 <- 500
q3 <- 1000
centerDate <- "08-01"
yearStart <- 1980
yearEnd   <- 2017
lapply(test, plotConcTimeSmooth, q1, q2, q3, centerDate = centerDate, legendTop = 0.06, legendLeft = 1970,
       yearStart = yearStart, yearEnd = yearEnd, concMax = 0.06, concMin = 0)




# geogenic solute ---------------------------------------------------------


targAnalyte <- "HARDNESS AS CACO3"
Ca <- lapply(targStns, function(stnSelect) 
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                                 wq_data = wqDat, flow_data = hydDat)))

level_Ca    <- seq(0, 0.15, 0.01)
maxDiff_Ca  <- 150

lapply(Ca, plotDiffContours, 
       year0 = yearStart1, year1 = yearEnd1, qBottom = qBottom, qTop =qTop, 
       maxDiff = maxDiff_Ca, qUnit=1)


targAnalyte <- "TURBIDITY"
ntu <- lapply(targStns, function(stnSelect) 
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                                 wq_data = wqDat, flow_data = hydDat)))

maxDiff_ntu  <- 150

lapply(ntu, plotDiffContours, 
       year0 = yearStart1, year1 = yearEnd1, qBottom = qBottom, qTop =qTop, 
       maxDiff = maxDiff_ntu, qUnit=1)
