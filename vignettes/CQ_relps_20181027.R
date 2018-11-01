#' ---
#' title: ''
#' output:
#'   pdf_document: default
#'   html_document: default
#' ---
#' 
#' 



library(SFNRC)
library(segmented)
library(multcomp)
library(pwr)
library(zoo)
library(reshape2)
library(raster)
library(scales)
library(ggplot2)
library(gridExtra)
library(ggmap)
library(ggrepel)
library(sp)
library(rgdal)
library(plyr)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(quantmod)
library(ggsn)



todaysDate <- as.character(Sys.Date())
stn.targets <- c("S333", paste0("S12", toupper(letters[1:4])))

# station coordinate file

# Load SFNRC water quality data ---------------------------------
head(wqDat)
target_analytes <- c("PHOSPHATE|NITROGEN|AMMONI|SUSPENDED|DISSOLVED OXYGEN|CALCIUM|POTASSIUM|HARDNESS|SODIUM|CHLORIDE|TEMP|CONDUCTIVITY, FIELD|SILICA|LEAD, TOTAL|MAGNESIUM|TURBIDITY|CHLOROPHYLL|MERCURY, TOTAL|SULFATE|ZINC, TOTAL|CHLORDANE|MALATHION|CARBOPHENOTHION|PH, FIELD")


# https://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r
wq.df <- wqDat
wq.df$bdl <- 0 # dummy variable indicating whether value estimated from MDL
wq.df <- wq.df[c(!((wq.df$stn %in% "G377D") & (wq.df[, 'value'] %in% c(614.000, 236.190, 69.299)))), ] # values to exclude


a <- ddply(wq.df, .(param, stn), summarise, nonZeros = sum(!is.na(value)))
# write.csv(a, "/home/thill/RDATA/params_by_stn_180814.csv")

### replace NAs with method detection limit, where MDL is stated
head(wq.df[is.na(wq.df$value), ])
summary(wq.df[is.na(wq.df$value), ])
wq.df[(wq.df$mdl < 0) & !is.na(wq.df$mdl), ]
wq.df[(wq.df$mdl > 20) & !is.na(wq.df$mdl), ]
wq.df[(wq.df$mdl > 20) & !is.na(wq.df$mdl) & (is.na(wq.df$value)), ]

pb <- txtProgressBar(style = 3, min = 0, max = nrow(wq.df))
for (i in 1:nrow(wq.df)) {
  if (is.na(wq.df$value[i])) {
    if ((wq.df$mdl[i] > 0) & (wq.df$mdl[i] < 50)) { # idk why, but there's a -5 and a 100 in the mdl data
      wq.df$bdl[i]   <- 1
      wq.df$value[i] <- wq.df$mdl[i]
      
    }
  }
  setTxtProgressBar(pb, i)
}

summary(wq.df[wq.df$bdl == 1, ])
summary(wq.df[wq.df$bdl == 0, ])


###
### Process hydro data - stage and flow

hydDat <- hydDat[c(!((hydDat$stn %in% "S356") & (hydDat$date < "2015-01-01"))), ] # remove 15-minute data from the S356 testing phase (summed to get those wildly high values)
hydDat <- hydDat[c(!hydDat$stn %in% "S178"), ] # remove S178 - odd effects from backflow
hydDat <- seas(hydDat, timeCol = "date")

############################## added 20181029
hydDat$flow[is.na(hydDat$flow)] <- 0
##############################


# make WQ dataset wide (one row per date)
wq2  <- dcast(wq.df[grep(wq.df$param, pattern = target_analytes), ], date * stn ~ param, mean, na.rm = TRUE)
bdls <- dcast(wq.df[grep(wq.df$param, pattern = target_analytes), ], date * stn * param ~ bdl, mean, na.rm = TRUE)
sum(!is.na(bdls$'1')) # 8147 BDL values
sum(is.na(bdls$'0'))  # of which 6409 remain as NAs, suggesting only 8147 - 6409 = 1738 were replaced

# set dates and merge wq and flow data
wq2$naca          <- (wq2$SODIUM / 22.9898) / ((wq2$CALCIUM / 40.078)) # mol:mol; possibly indicative of water source, 0.94 mol/mol is EAA surface water ratio (Chen et al. 2006)
wq2               <- join_all(list(hydDat, wq2), by = c("stn", "date"))
wq2               <- seas(wq2, timeCol = "date")


summary(wq2$`PHOSPHATE, DISSOLVED AS P`)
sum(!is.na(wq2$`PHOSPHATE, DISSOLVED AS P`)) # 215 samples
summary(wq2$`PHOSPHATE, TOTAL AS P`)
sum(!is.na(wq2$'PHOSPHATE, TOTAL AS P')) # 6034
sum(!is.na(wq2$'PHOSPHATE, TOTAL AS P'[wq2$flow > 0])) # 15561 with flows > 0


wq.df[(wq.df$stn %in% "G377D") & (wq.df[, 'value'] %in% c(614.000, 236.190, 69.299)), ]

# 233175 G377D 2007-05-21     CARBON, TOTAL ORGANIC 614.000  mg/L 10.000
# 233176 G377D 2007-05-21  KJELDAHL NITROGEN, TOTAL 236.190  mg/L  2.500
# 233177 G377D 2007-05-21     PHOSPHATE, TOTAL AS P  69.299  mg/L  0.400

### possible concentration issue with G377D - CVC is 6.5

### Add flow to TAMBR stations in object wq2
# source("/home/thill/RDATA/script_TAMBR_proc_20180912.R")
###

dd.wq  <- ddply(wq2, .(stn), numcolwise(function(x) length(x))) # total no. of samples
dd.wq2 <- ddply(wq2, .(stn), numcolwise(function(x) sum(is.na(x)))) # no. of samples below MDL (those below appear as NAs)

dd.wq2$NAratio <- dd.wq2$`PHOSPHATE, TOTAL AS P` / dd.wq$`PHOSPHATE, TOTAL AS P` 
dd.wq2$NAratio[dd.wq2$NAratio == 0] <- NA
dd.wq2[, c("stn", "NAratio")]


stns.wo.NAs <- unique(wq2$stn[!is.na(wq2$flow)])


#' 
#' 
#' 
#' 
## ----stats!, include = FALSE---------------------------------------------

### check for normality - stations pooled
# par(mfcol = c(1, 3)) 
# x <- log(wq2[, "PHOSPHATE, DISSOLVED AS P"]); qqnorm(x); qqline(x, col = "red")
# x <- log(wq2[, "PHOSPHATE, ORTHO AS P"]); qqnorm(x); qqline(x, col = "red")
# x <- log(wq2[, "PHOSPHATE, TOTAL AS P"]); qqnorm(x); qqline(x, col = "red")
# 
# par(mfcol = c(1,1)) 

### Takeaway: 
wq2$st.fac <- factor(wq2$stn, levels = stn.targets)

testType <- "Tukey"
aov1 <- aov(log(wq2[, "PHOSPHATE, DISSOLVED AS P"]) ~ st.fac, data = wq2)
summary(glht(aov1, linfct = mcp(st.fac=testType))) # no differences (Dunnett or Tukey)

aov1 <- aov(log(wq2[, "PHOSPHATE, ORTHO AS P"]) ~ st.fac, data = wq2)
summary(glht(aov1, linfct = mcp(st.fac=testType))) # Tukey: S12D different from all stations; S-12B higher than S-333

aov1 <- aov(log(wq2[, "PHOSPHATE, TOTAL AS P"]) ~ st.fac, data = wq2)
summary(glht(aov1, linfct = mcp(st.fac=testType))) # Dunnett: complex. S12A and S333 share letter, S12C and B share a letter
# Tukey: Only insignificant differences are S12B-C and S12A-S333

# aov1 <- aov(log(wq2[, "TOTAL SUSPENDED SOLIDS"]) ~ st.fac, data = wq2)
# summary(glht(aov1, linfct = mcp(st.fac=testType))) # Dunnett, Tukey: S333-S12D



#' 
#' 
#' 
#' 
#' 
#' 
#' # Phosphorus concentrations and solute behavior at the Everglades northern boundary
#' 
#' The question examined in this analysis is whether phosphorus (P) concentrations and solute behavior at S-333 differ from the nearby S-12 structures. Two aspects are explored: the magnitude of any difference in P concentrations, and possible explanations based on ancillary data. This analysis is motivated by S-333's reputation for being relatively enriched in P, a broadly shared perception that may not be consistent with water quality data (e.g., Hanlon et al. 2010).
#' 
#' P concentrations are of direct relevance to attainment of water quality objectives at the northern boundary of Everglades National Park. Solute behavior (i.e., the relationship between concentration and flow) is relevant for estimating nutrient fluxes to downstream areas and, to the extent that concentrations are flow-dependent, solute behavior can inform water management regimes designed to minimize P exceedences.
#' 
#' 
#' ## Data sources and analysis
#' 
#' The water quality and flow data described below were extracted from DataForEver. Concentrations falling below the MDL were assumed to be equal to the MDL when the MDL was provided for a given parameter and sampling date. When MDLs were not reported, non-detects were left as null values. This approach differs from typical treatment, which replaces non-detects with a value equal to half of the MDL. My more conservative approach results in slightly higher replacement concentrations. After correcting non-detects using MDLs, null data represented small proportions of the total dataset. For example, null TP values comprised a maximum of 0.6% of an individual station's data.
#' 
#' Departures from normality were evaluated graphically. Data violating normality assumptions were transformed as necessary to improve normality.
#' 
#' Two metrics are used to gain insight into solute behavior. The flow-dependence of concentrations was inferred from the slope (*b*) of regression lines relating ln-transformed concentrations and ln-transformed flows. A negative *b* indicates the dominance of dilutive processes, *b* near zero indicates flow independence (chemostatic behavior), and positive *b* suggests dominance by accretive processes (e.g., flushing, changing contribution areas). In addition to *b*, the ratio of coefficients of variation for concentration and discharge (CV~C~/CV~Q~) was used to identify chemostatic behavior attributable to flow-independence from apparent chemostasis resulting from noisy data (Musolff et al. 2017).
#' 
#' # Results
#' 
#' ## Phosphorous concentrations
#' 
#' 
#' Splitting the TP concentration data into categories based on whether the structure is open shows that S-333 is anomalous in that it does not have a strong difference in P concentrations during flow and non-flow sampling events (Fig. \ref{fig:P-boxplots}). The S-12 structures all have elevated concentrations during no-flow periods, whereas concentrations at S-333 are independent of whether the structure is closed or open.
#' 
#' 
#' As Fig. \ref{fig:P-boxplots} indicates, the distribution of concentrations for three P species at the S-12 structures varies depending on what data are considered. Because the pattern of higher TP during flow events is not observed at S-333, it is possible that this distinction could affect comparisons between stations. 
#' 
#' When all sampling events are considered, the data do not suggest systematic differences between S-333 and the S-12 structures (Fig. \ref{fig:boxplots1}, top row). Stations show no differences in dissolved P. Ortho-P is elevated at S-12D compared with the other stations, and higher at S-12B than at S-333. Total P at S-12A and S-333 is significantly higher than S-12B and S-12C. 
#' 
#' When data are limited to periods of active flow (Fig. \ref{fig:boxplots1}, bottom row), S-333 does have significantly higher TP than the S-12 structures; a mean of 14 ppb, compared with 9-12 ppb at the S-12s. This offers support for the contention that S-333 is relatively enriched in P, at least during the periods of active flow that are most relevant to the Everglades.
#' 
#' 
#' 
#' 
## ----flow-P-boxplots, fig.cap = "\\label{fig:P-boxplots}Comparison of total P concentrations during flow and no-flow conditions at the S-333 and S-12 structures. Raw data shown as jittered points behind boxplots. For each station, significant differences between flow and no-flow concentrations are indicated by asterisks (all stations except S-333). Data were log-transformed to improve normality."----

### flow/no-flow comparison - boxplots
### make flow bins
dat2 <-  transform(wq2, group = cut(flow,
                                    breaks=c(-Inf, 0, Inf),
                                    labels=c('no flow', 'flow')))

### stats - within-station pairwise comparisons of flow and no-flow conditions
### check for normality 
ttests <- dlply(dat2[dat2$stn %in% stn.targets, ], "stn", function(df)
  t.test(log(df$`PHOSPHATE..TOTAL.AS.P`[df$group %in% "no flow"]), log(df$`PHOSPHATE..TOTAL.AS.P`[df$group %in% "flow"]))) # S-12s all sig at P < 0.001; no difference at S-333!

### some plots
ann_textTP <- data.frame(stn = c("S333", paste0("S12", toupper(letters[1:4]))), 
                         variable = rep(c("PHOSPHATE..TOTAL.AS.P"), times = 5),
                         lab = c("B", "A", "B", "B", "B"),
                         value = rep(0.50, times = 5))

FlowVsNoWpts <- ggplot(dat2[(dat2$stn %in% stn.targets) & (!is.na(dat2[, "PHOSPHATE..TOTAL.AS.P"])), ], 
       aes(x = group, y = `PHOSPHATE..TOTAL.AS.P`)) + 
  geom_jitter(width = 0.1, size = 0.3, col = "darkgray") +
  geom_boxplot(outlier.shape = NA) + 
  theme_classic() + facet_grid(. ~ stn) + scale_y_log10()+
  theme(legend.position="top",axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + annotate("text", x = 1.5, y = 0.35, label = c("*", "*", "*", "*", ""), size = 12) +
  ylab (expression("Total P (mg P" %.%"L"^-1*")")) + xlab("") # +   geom_text(data = ann_textOrtho, label = ann_textOrtho$lab)

FlowVsNo <- ggplot(dat2[(dat2$stn %in% stn.targets) & (!is.na(dat2[, "PHOSPHATE..TOTAL.AS.P"])), ], 
                       aes(x = group, y = `PHOSPHATE..TOTAL.AS.P`)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_classic() + facet_grid(. ~ stn) + scale_y_log10(limits = c(0.01, 0.12))+
  theme(legend.position="top",axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = 1.5, y = 0.105, label = c("*", "*", "*", "*", ""), size = 12) +
  ylab (expression("Total P (mg P" %.%"L"^-1*")")) + xlab("") 




### main thesis is that there's seasonality in P and flows, and that at S333 the P regime is similar, 
### but that flow regimes are profoundly different, leading to homogenization of water quality rather than strong flow/no-flow disparities.
### 

# Other seasonal parameters - flow vs no-flow -----------------------------------------------
### chloride diff at S12A and C
### "CHLOROPHYLL.A" is good example - no diff only at S333
### "TOTAL.NITROGEN" is a good example too, but no data for S12D

targParam <- "CHLOROPHYLL.A"
ttests <- dlply(dat2[dat2$stn %in% stn.targets, ], "stn", function(df)
  t.test(log(df[df$group %in% "no flow", targParam]), log(df[df$group %in% "flow", targParam]))) # S-12s all sig at P < 0.001; no difference at S-333!

ggplot(dat2[(dat2$stn %in% stn.targets) & (!is.na(dat2[, targParam])), ], 
       aes(x = group, y = get(targParam))) + 
  geom_jitter(width = 0.1, size = 0.3, col = "darkgray") +
  geom_boxplot(outlier.shape = NA) + ylim(0, 10) + 
  theme_classic() + facet_grid(. ~ stn) + #scale_y_log10()+
  theme(legend.position="top",axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + 
  # annotate("text", x = 1.5, y = 0.35, label = c("*", "*", "*", "*", ""), size = 12) +
  ylab(expression("(mg" %.%"L"^-1*")")) + xlab("") 



#' 
## ----boxplots, fig.height = 4, fig.width = 6.5, fig.cap = "\\label{fig:boxplots1}Phosphorus concentrations at S-12A:D (1976-2018) and S-333 (1978-2018). Top row includes all concentration data; bottom row shows concentrations only during times with active flow. Different letters indicate significant differences (P < 0.05) determined by ANOVA with Tukey's HSD post-hoc test. Data were ln-transformed to improve normality."----

### differences in entire dataset?
# ggplot(wq2[!is.na(wq2[, "PHOSPHATE, TOTAL AS P"]), ], aes(x = stn, y = `PHOSPHATE, TOTAL AS P`)) + 
#   geom_boxplot(outlier.alpha=0) + geom_jitter(width = 0.1, size = 0.3) + 
#   theme_classic() + geom_hline(yintercept = 0.010, lty = 2) +
#   theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=15), plot.title = element_text(hjust = 0.5)) +  
#   ylab ("Total P (mg/L)") + xlab("")

a.means <- ddply(wq2, .(stn), numcolwise(mean, na.rm = TRUE))
a.se <- ddply(wq2, .(stn), numcolwise(se))
a.count <- ddply(wq2, .(stn), numcolwise(function(x) sum(!is.na(x))))

# Only insignificant difference is S12B-C and S12A-S333
### panel plot
wq.melt <- melt(wq2[wq2$stn %in% stn.targets, ], id.vars = "stn", measure.vars = c("PHOSPHATE, DISSOLVED AS P", "PHOSPHATE, ORTHO AS P", "PHOSPHATE, TOTAL AS P")) #, "TOTAL SUSPENDED SOLIDS"))
ann_textTP <- data.frame(stn = stn.targets, 
                         variable = rep(c("PHOSPHATE, TOTAL AS P"), times = 5),
                         lab = c("A", "A", "B", "B", "C"),
                         value = rep(0.50, times = 5))
# ann_textTSS <- data.frame(stn = c("S333", paste0("S12", toupper(letters[1:4]))), 
#                        variable = rep(c("TOTAL SUSPENDED SOLIDS"), times = 5),
#                        lab = c("B", "AB", "AB", "AB", "A"),
#                        value = rep(50, times = 5))
#  
ann_textOrtho <- data.frame(stn = stn.targets, 
                            variable = rep(c("PHOSPHATE, ORTHO AS P"), times = 5),
                            lab = c("B", "AB", "A", "AB", "C"),
                            value = rep(0.12, times = 5))

all.data <- ggplot(wq.melt, aes(x = stn, y = value)) + 
  geom_jitter(width = 0.1, size = 0.3, col = "darkgray") + geom_boxplot(outlier.alpha=0) + 
  theme_classic() + 
  theme(legend.position="none", 
        axis.text.x = element_text(angle = 0, hjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) +  facet_wrap(. ~ variable, scales = "free_y") + xlab("") + ylab("mg/L (all data)") + geom_text(data = ann_textTP, label = ann_textTP$lab) +
  geom_text(data = ann_textOrtho, label = ann_textOrtho$lab)

TP.DP.withPts <- ggplot(wq.melt[!wq.melt$variable %in% "PHOSPHATE, ORTHO AS P", ], aes(x = stn, y = value)) + 
  geom_jitter(width = 0.1, size = 0.3, col = "darkgray") + 
  geom_boxplot(outlier.alpha=0, alpha = 0.4) + theme_classic() + theme(legend.position="none",
        axis.text.x = element_text(angle = 0, hjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) +  facet_wrap(. ~ variable) + xlab("") + 
  ylab(expression("mg P" %.%"L"^-1*" (all data)")) + geom_text(data = ann_textTP, label = ann_textTP$lab) 

ann_textTP2 <- ann_textTP
ann_textTP2$value <- 0.1
TP.DP <- ggplot(wq.melt[!wq.melt$variable %in% "PHOSPHATE, ORTHO AS P", ], aes(x = stn, y = value)) + 
  geom_boxplot(outlier.alpha=0, alpha = 0.4) + theme_classic() + theme(legend.position="none", 
        axis.text.x = element_text(angle = 0, hjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) +  facet_wrap(. ~ variable) + xlab("") + 
  ylab(expression("mg P" %.%"L"^-1*" (all data)")) + geom_text(data = ann_textTP2, label = ann_textTP2$lab) +
  ylim(0.002, 0.12)

TP.DP


### differences in periods with flow
### stats
wq2$st.fac <- factor(wq2$stn, levels = stn.targets)

subDat.flow <- wq2[wq2$flow > 0, ]
testType <- "Tukey"
aov1 <- aov(log(subDat.flow[, "PHOSPHATE, DISSOLVED AS P"]) ~ st.fac, data = subDat.flow)
summary(glht(aov1, linfct = mcp(st.fac=testType))) # no differences

aov1 <- aov(log(subDat.flow[, "PHOSPHATE, ORTHO AS P"]) ~ st.fac, data = subDat.flow)
summary(glht(aov1, linfct = mcp(st.fac=testType))) # Tukey: S12D different from all stations; S-12A higher than S-333

aov1 <- aov(log(subDat.flow[, "PHOSPHATE, TOTAL AS P"]) ~ st.fac, data = subDat.flow)
summary(glht(aov1, linfct = mcp(st.fac=testType))) # Tukey: only shared letter is S12C-S12A


a.means.flow <- ddply(subDat.flow[subDat.flow$stn %in% stn.targets, ], .(stn), numcolwise(mean, na.rm = TRUE))
a.se.flow <- ddply(subDat.flow, .(stn), numcolwise(se))
a.count.flow <- ddply(subDat.flow, .(stn), numcolwise(function(x) sum(!is.na(x))))


### panel plot
flow.only.melt <- melt(wq2[(wq2$flow > 0) & (wq2$stn %in% stn.targets), ], id.vars = "stn", measure.vars = c("PHOSPHATE, DISSOLVED AS P", "PHOSPHATE, ORTHO AS P", "PHOSPHATE, TOTAL AS P")) #, "TOTAL SUSPENDED SOLIDS"))
flow.only.melt <- flow.only.melt[!is.na(flow.only.melt$stn), ]
# ann_textTSS <- data.frame(stn = c("S333", paste0("S12", toupper(letters[1:4]))), 
#                        variable = rep(c("TOTAL SUSPENDED SOLIDS"), times = 5),
#                        lab = c("B", "AB", "AB", "AB", "A"),
#                        value = rep(50, times = 5))
#  
ann_textOrtho <- data.frame(stn = stn.targets, 
                            variable = rep(c("PHOSPHATE, ORTHO AS P"), times = 5),
                            lab = c("B", "A", "AB", "AB", "C"),
                            value = rep(0.12, times = 5))
ann_textTP <- ann_textTP2 <- data.frame(stn = stn.targets, 
                         variable = rep(c("PHOSPHATE, TOTAL AS P"), times = 5),
                         lab = c("D", "A", "B", "A", "C"),
                         value = rep(0.2, times = 5))
ann_textTP2$value <- 0.09

flow.onlyWpoints <- ggplot(flow.only.melt[!flow.only.melt$variable %in% "PHOSPHATE, ORTHO AS P", ], aes(x = stn, y = value)) + 
  geom_jitter(width = 0.1, size = 0.3, col = "darkgray") + geom_boxplot(outlier.alpha=0) + theme_classic() + theme(legend.position="none", 
        axis.text.x = element_text(angle = 0, hjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) +  facet_wrap(. ~ variable, scales = "free_y") + xlab("") + 
  ylab(expression("mg P" %.%"L"^-1~" (flow > 0)")) + 
  geom_text(data = ann_textTP, label = ann_textTP$lab) + ylim(0, 0.035)

flow.only <- ggplot(flow.only.melt[!flow.only.melt$variable %in% "PHOSPHATE, ORTHO AS P", ], aes(x = stn, y = value)) + 
  geom_boxplot(outlier.alpha=0) + theme_classic() + theme(legend.position="none", 
        axis.text.x = element_text(angle = 0, hjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) +  facet_wrap(. ~ variable) + xlab("") + 
  ylab(expression("mg P" %.%"L"^-1~" (flow > 0)")) + 
  geom_text(data = ann_textTP2, label = ann_textTP2$lab)  + ylim(0, 0.035)

grid.arrange(TP.DP, flow.only, nrow = 2)
plt <- arrangeGrob(TP.DP, flow.only, nrow = 2)
# ggsave(plt, file = "/opt/physical/troy/RDATA/output/DP_TP.png", width = 5, height = 5, units = "in", dpi = 200)



# Simulation - modify flow regimes and simulate effect on P dynamics -------
### approach 1 - use TP data from each station, but binary flow/no-flow data from S12D (then redo with S333 flow regimes). Does this all assume that TP and flow are independent?

dat2.sub <- dat2[dat2$stn %in% stn.targets, c(1:3, 44, 64)]

# dat2.sub.m <- dcast(dat2.sub, stn + date ~ numbers, value.var = PHOSPHATE..TOTAL.AS.P)

pb <- txtProgressBar(style = 3, min = 0, max = nrow(dat2.sub))
for (i in 1:nrow(dat2.sub)) {
  ifelse(length(dat2.sub$group[(dat2.sub$stn %in% "S333") & (dat2.sub$date == dat2.sub$date[i])]) == 1, 
    dat2.sub$group.S333[i] <- dat2.sub$group[(dat2.sub$stn %in% "S333") & (dat2.sub$date == dat2.sub$date[i])],
    dat2.sub$group.S333[i] <- NA
    )
  ifelse(length(dat2.sub$group[(dat2.sub$stn %in% "S12D") & (dat2.sub$date == dat2.sub$date[i])]) == 1, 
        dat2.sub$group.S12D[i] <- dat2.sub$group[(dat2.sub$stn %in% "S12D") & (dat2.sub$date == dat2.sub$date[i])],
        dat2.sub$group.S12D[i] <- NA
    )
  setTxtProgressBar(pb, i)
}
beepr::beep(4)

tail(factor(dat2.sub$group.S12D, levels = c(1, 2), labels = c("no flow", "flow")))
     
dat2.sub$group.S12D <- factor(dat2.sub$group.S12D, levels = c(1, 2), labels = c("no flow", "flow"))
dat2.sub$group.S333 <- factor(dat2.sub$group.S333, levels = c(1, 2), labels = c("no flow", "flow"))
dat2.sub$st.fac     <- factor(dat2.sub$stn, levels = stn.targets)

ddply(dat2.sub[dat2.sub$stn %in% stn.targets, ], .(stn, group.S333), numcolwise(mean, na.rm = TRUE))
ddply(dat2.sub[dat2.sub$stn %in% stn.targets, ], .(stn, group.S12D), numcolwise(mean, na.rm = TRUE))

### base case
ttests <- dlply(dat2.sub, "stn", function(df) 
  t.test(log(df[df$group %in% "no flow", "PHOSPHATE..TOTAL.AS.P"]), log(df[df$group %in% "flow", "PHOSPHATE..TOTAL.AS.P"]))) # S-12s all sig at P < 0.001; no difference at S-333!
aov1 <- aov(log(PHOSPHATE..TOTAL.AS.P) ~ st.fac, data = dat2.sub[dat2.sub$group %in% "flow", ])
summary(glht(aov1, linfct = mcp(st.fac=testType))) # Tukey: only shared letter is S12C-S12A

### Using S333 flows
ttests.s333 <- dlply(dat2.sub[!is.na(dat2.sub$group.S333), ], "stn", function(df) # s12B remains different (flow vs no-flow)
  t.test(log(df[df$group.S333 %in% "no flow", "PHOSPHATE..TOTAL.AS.P"]), log(df[df$group.S333 %in% "flow", "PHOSPHATE..TOTAL.AS.P"]))) 
aov1 <- aov(log(PHOSPHATE..TOTAL.AS.P) ~ st.fac, data = dat2.sub[dat2.sub$group.S333 %in% "flow", ])
summary(glht(aov1, linfct = mcp(st.fac=testType))) # Tukey: only shared letter is S12C-S12A

### Using S12D flows
ttests.s12d <- dlply(dat2.sub[!is.na(dat2.sub$group.S12D), ], "stn", function(df) # all are significantly different
  t.test(log(df[df$group.S12D %in% "no flow", "PHOSPHATE..TOTAL.AS.P"]), log(df[df$group.S12D %in% "flow", "PHOSPHATE..TOTAL.AS.P"]))) 
aov1 <- aov(log(PHOSPHATE..TOTAL.AS.P) ~ st.fac, data = dat2.sub[dat2.sub$group.S12D %in% "flow", ])
summary(glht(aov1, linfct = mcp(st.fac=testType))) # Tukey: only shared letter is S12C-S12A





S333flows <- ggplot(dat2.sub[!is.na(dat2.sub$group.S333), ], 
                   aes(x = group.S333, y = `PHOSPHATE..TOTAL.AS.P`)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_classic() + facet_grid(. ~ stn) + scale_y_log10(limits = c(0.01, 0.12))+
  theme(legend.position="top",axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = 1.5, y = 0.105, label = c("", "*", "", "", ""), size = 12) +
  ylab (expression("Total P (mg P" %.%"L"^-1*"; using S333 flow data)")) + xlab("") 


S12Dflows <- ggplot(dat2.sub[!is.na(dat2.sub$group.S12D), ], 
                    aes(x = group.S12D, y = `PHOSPHATE..TOTAL.AS.P`)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_classic() + facet_grid(. ~ stn) + scale_y_log10(limits = c(0.01, 0.12))+
  theme(legend.position="top",axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = 1.5, y = 0.105, label = c("*", "*", "*", "*", "*"), size = 12) +
  ylab (expression("Total P (mg P" %.%"L"^-1*"; using S12D flow data)")) + xlab("") 

FlowVsNo # base case - homogenization at S333
# ggsave(file = "/opt/physical/troy/RDATA/output/flowVsNoFlow_base.png", width = 5, height = 3, units = "in", dpi = 200)
S12Dflows # 
# ggsave(file = "/opt/physical/troy/RDATA/output/flowVsNoFlow_S12DflowRegime.png", width = 5, height = 3, units = "in", dpi = 200)
S333flows # if the S12s had S333's flow regime, how would that affect the difference between P concentrations in flow vs no-flow conditions?
# ggsave(file = "/opt/physical/troy/RDATA/output/flowVsNoFlow_S333flowRegime.png", width = 5, height = 3, units = "in", dpi = 200)

### approach 2 - can I recreate observed data by resampling from totality of a month's values?







#' ## The effect of discharge on total P concentrations
#' 
#' 
#' Focusing just on periods with active flow through the structures, the behavior of P at S-333 remains distinct in showing negligble variation as a function of discharge (Fig. \ref{fig:P-Q}). P at S-333 varies little in response to changes in flow, contrasting with P behavior at the S-12 structures, where concentrations decline as discharge increases consistent with dilution and possible source limitation.
#' 
#' 
#' CV~C~/CV~Q~ ratios were all above 1, indicating that TP concentrations had greater relative variation than flows (Fig. \ref{fig:b-CV}). The S-12 structures all had very similar ln(C)-ln(Q) relationships, but showed more variation in CV~C~/CV~Q~ ratios, which increased moving eastward along L-29. This ratio is driven by an east-west gradient of variation in concentrations, which could partly reflect that the main source of P is water carried down the L-67N. Proximity to that relatively stable source of nutrients could contribute to reduced variation in measured concentrations. 
#' 
#' 
## ----flow-solute relationships - linear models, fig.cap = "\\label{fig:P-Q}Comparison of total P concentrations during flow and no-flow conditions at the S-333 and S-12 structures. For each station, significant differences between flow and no-flow concentrations are indicated by asterisks (all stations except S-333)."----

lm.mod <- function(df, param){
  m1<-lm(log(param) ~ log(flow), data = df)
}

### "TOTAL.NITROGEN" "AMMONIA.N"    "SP.CONDUCTIVITY..FIELD" "PHOSPHATE..DISSOLVED.AS.P"   "PHOSPHATE..TOTAL.AS.P"   "TOTAL.SUSPENDED.SOLIDS"
targParam <- "PHOSPHATE..TOTAL.AS.P" # "PHOSPHATE..TOTAL.AS.P"   "CALCIUM"    "SODIUM"   "POTASSIUM" "MAGNESIUM" "SILICA"  "CHLOROPHYLL.A..CORRECTED" "CHLOROPHYLL.A"

dat2$logC[!is.na(dat2[, targParam]) & (dat2$group %in% "flow")]  <- log(dat2[!is.na(dat2[, targParam]) & (dat2$group %in% "flow"), targParam])
dat2$logQ[!is.na(dat2[, targParam]) & (dat2$group %in% "flow")]  <- log(dat2[!is.na(dat2[, targParam]) & (dat2$group %in% "flow"), "flow"])

ggplot(dat2[!is.na(dat2[, targParam]) & (dat2$group %in% "flow") & (dat2$stn %in% stn.targets), ], 
       aes(x = logQ, y = logC)) + 
  geom_point(alpha = 0.6, size = 0.6) + geom_smooth(method = "lm") + 
  theme_classic() + facet_wrap(~ stn, scales = "free_y") + #scale_y_log10()  +  scale_x_log10() +
  theme(legend.position="top", axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), 
        text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + #annotate("text", x = 1.5, y = 0.35, label = c("*", "*", "*", "*", ""), size = 12) +
  ylab (paste0(targParam, " (mg/L; log scale)")) + xlab("Discharge (cfs; log scale)")





#  C-Q relps by season, rising/falling water level ----------------

ggplot(dat2[!is.na(dat2[, "PHOSPHATE..TOTAL.AS.P"]) & (dat2$stn %in% stn.targets), ], 
       aes(x = head_water, y = log(PHOSPHATE..TOTAL.AS.P))) + 
  geom_point(alpha = 0.6, size = 0.6) + geom_smooth(method = "lm") + 
  theme_classic() + facet_grid( ~ stn, scales = "free_y") + #scale_y_log10()  +  scale_x_log10() +
  theme(legend.position="top", axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), 
        text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + #annotate("text", x = 1.5, y = 0.35, label = c("*", "*", "*", "*", ""), size = 12) +
  ylab (paste0("TP (mg/L; log scale)")) + xlab("headwater (ft)")


ggplot(dat2[!is.na(dat2[, "PHOSPHATE..TOTAL.AS.P"]) & (dat2$group %in% "flow") & (dat2$stn %in% stn.targets), ], 
       aes(x = log(flow), y = log(PHOSPHATE..TOTAL.AS.P))) + 
  geom_point(alpha = 0.6, size = 0.6) + geom_smooth(method = "lm") + 
  theme_classic() + facet_grid(seas ~ stn, scales = "free_y") + #scale_y_log10()  +  scale_x_log10() +
  theme(legend.position="top", axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), 
        text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + #annotate("text", x = 1.5, y = 0.35, label = c("*", "*", "*", "*", ""), size = 12) +
  ylab (paste0("TP (mg/L; log scale)")) + xlab("Discharge (cfs; log scale)")


ggplot(dat2[!is.na(dat2[, "PHOSPHATE..TOTAL.AS.P"]) & (dat2$group %in% "flow") & (dat2$stn %in% stn.targets), ], 
       aes(x = flow, y = PHOSPHATE..TOTAL.AS.P)) + 
  geom_point(alpha = 0.6, size = 0.6) + geom_smooth(method = "lm") + 
  theme_classic() + facet_grid(seas ~ stn, scales = "free_y") + #scale_y_log10()  +  scale_x_log10() +
  theme(legend.position="top", axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), 
        text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + #annotate("text", x = 1.5, y = 0.35, label = c("*", "*", "*", "*", ""), size = 12) +
  ylab (paste0("TP (mg/L; log scale)")) + xlab("Discharge (cfs; log scale)")


### identify periods of rising stage
dat3 <- dat2[dat2$stn %in% stn.targets, ] %>% arrange(stn, head_water) %>%
  group_by(stn) %>% 
  mutate(rank = rank(head_water, ties.method = "first"))

dat3 <- dat2[dat2$stn %in% stn.targets, ] %>% arrange(stn, flow) %>%
  group_by(stn) %>% 
  mutate(qtile = cut(flow, 
    breaks=4, labels = FALSE,
    include.lowest=TRUE))

ggplot(dat3[!is.na(dat3[, "PHOSPHATE..TOTAL.AS.P"]) & (dat3$group %in% "flow"), ], 
       aes(x = log(flow), y = log(PHOSPHATE..TOTAL.AS.P))) + 
  geom_point(alpha = 0.6, size = 0.6) + geom_smooth(method = "lm") + 
  theme_classic() + facet_grid(stn ~ mo, scales = "free_y") + #scale_y_log10()  +  scale_x_log10() +
  theme(legend.position="top", axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), 
        text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + #annotate("text", x = 1.5, y = 0.35, label = c("*", "*", "*", "*", ""), size = 12) +
  ylab (paste0("TP (mg/L; log scale)")) + xlab("Discharge (cfs; log scale)")


ggplot(dat3[!is.na(dat3[, "PHOSPHATE..TOTAL.AS.P"]) & (dat3$group %in% "flow"), ], 
       aes(x = log(flow), y = log(PHOSPHATE..TOTAL.AS.P), col = head_water)) + 
  geom_point(alpha = 0.6, size = 0.6) + geom_smooth(method = "lm") + 
  theme_classic() + facet_grid(stn ~ mo, scales = "free_y") + #scale_y_log10()  +  scale_x_log10() +
  theme(legend.position="top", axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), 
        text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + #annotate("text", x = 1.5, y = 0.35, label = c("*", "*", "*", "*", ""), size = 12) +
  ylab (paste0("TP (mg/L; log scale)")) + xlab("Discharge (cfs; log scale)") + scale_colour_distiller(palette = "Spectral")


ggplot(dat3[!is.na(dat3[, "PHOSPHATE..TOTAL.AS.P"]) & (dat3$group %in% "flow"), ], 
       aes(x = head_water, y = log(PHOSPHATE..TOTAL.AS.P))) + 
  geom_point(alpha = 0.6, size = 0.6) + geom_smooth(method = "lm") + 
  theme_classic() + facet_grid(stn ~ mo, scales = "free_y") + #scale_y_log10()  +  scale_x_log10() +
  theme(legend.position="top", axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), 
        text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + #annotate("text", x = 1.5, y = 0.35, label = c("*", "*", "*", "*", ""), size = 12) +
  ylab (paste0("TP (mg/L; log scale)")) + xlab("headwater (ft NGVD29?)")

#
#  by(dat2[!is.na(dat2[, "PHOSPHATE..TOTAL.AS.P"]) & (dat2$flow > 0), ], "stn",                function(x) geom_smooth(data=x, method = lm, formula = lm.mod(x)))

#' 
#' 
## ----flow-solute relationships - synthesis, fig.height = 3, fig.width = 4, fig.cap = "\\label{fig:b-CV}Comparison of TP flux regimes. Y-axis is the slope of the ln(C) ln(Q) relationship, and x-axis is the ratio of coefficients of variation of TP concentration and flow."----
### plot of CV/CQ

### pull out b from regression models
lm.mod <- function(df){
  lm(log(`PHOSPHATE..TOTAL.AS.P`) ~ log(flow), data = df)
}

mods <- dlply(dat2[!is.na(dat2[, "PHOSPHATE..TOTAL.AS.P"]) & (dat2$group %in% "flow"), ], "stn", function(df)
  lm(log(`PHOSPHATE..TOTAL.AS.P`) ~ log(flow), data = df))
# test <- l_ply(mods, summary, .print = TRUE) # tough to print the summaries! slope for S-332 isn't significant
modResults1  <- ldply(mods, coef)
modResults2 <- ldply(mods, function(x) summary(x)$coefficients[c(4,7:8)]) # p values for intercept and slope (in that order)
names(modResults2)[2:4] <- c("slope.se", "pval.int", "pval.slope")
modResults3 <- ldply(mods, function(x) summary(x)$r.squared) # r2 for model
names(modResults3)[2] <- c("rsq")



### CV-C / CV-Q
cvDat <- ddply(dat2, .(stn), summarise, #  & (dat2$group %in% "flow") # flow-criterion removed 20180918
               CVC = sd(`PHOSPHATE..TOTAL.AS.P`, na.rm = TRUE) / mean(`PHOSPHATE..TOTAL.AS.P`, na.rm = TRUE),
               naca = mean(naca, na.rm = TRUE))

### hydDat doesn't include TAMBR stations
cq <- ddply(hydDat, .(stn), summarise,
            CVQ = sd(flow, na.rm = TRUE) / mean(flow, na.rm = TRUE))


modResults  <- join_all(list(modResults1, modResults2, modResults3, cvDat, cq), by = "stn")
modResults$ratio <- modResults$CVC / modResults$CVQ


ggplot(modResults, aes(x = ratio, y = `log(flow)`, label = stn, col = naca)) + scale_color_gradientn(colours = terrain.colors(7)) + 
  geom_point(alpha = 0, size = 0.6) + geom_pointrange(aes(ymin = `log(flow)` - slope.se, ymax =  `log(flow)` + slope.se)) + 
  theme_classic() + ylab("slope of ln(C)-ln(Q)") + xlab("CV-C / CV-Q") + geom_text(label.padding = unit(0.5, "lines"), nudge_x = 0.05) + 
  geom_hline(aes(yintercept = 0), linetype = 2) + geom_vline(aes(xintercept = 1), linetype = 2)

plot(`log(flow)` ~ naca, data = modResults, pch = 19, cex = 0.4)

out.lm <- lm(`log(flow)` ~ naca, data = modResults[!is.na(modResults$naca) & !is.na(modResults$`log(flow)`), ])
o <- segmented(out.lm, seg.Z = ~naca, psi = list(naca = c(2)), # very insensitive to psi
               control = seg.control(display = FALSE)
)
dat2 <- data.frame(naca = modResults$naca[!is.na(modResults$naca) & !is.na(modResults$`log(flow)`)], `log(flow)` = broken.line(o)$fit)
names(dat2)[2] <- "log(flow)"

ggplot(modResults, aes(x = naca, y = `log(flow)`)) + theme_classic() + geom_point(alpha = 0, size = 0.1) + geom_pointrange(aes(ymin = `log(flow)` - slope.se, ymax =  `log(flow)` + slope.se)) +
  geom_line(data = dat2, color = 'blue')





# flow timing -------------------------------------------------------------

q.mo <- ddply(wq2[wq2$stn %in% stns.wo.NAs, ], .(stn, mo), summarise,
              headwtr    = mean(head_water, na.rm = TRUE),
              headwtr.se = se(head_water),
              Mm3.day = mean(flow, na.rm = TRUE) * 0.0283168466 * 60 * 60 * 24 / 1e6,
              Mm3.day.se = se(flow) * 0.0283168466 * 60 * 60 * 24 / 1e6,
              P          = mean(`PHOSPHATE, TOTAL AS P`, na.rm = TRUE),
              P.se       = se(`PHOSPHATE, TOTAL AS P`),
              chl        = mean(`CHLOROPHYLL-A`, na.rm = TRUE),
              chl.se     = se(`CHLOROPHYLL-A`),
              flow.binary= sum(flow > 0.001, na.rm = TRUE)
) # m3/day


ggplot(q.mo[q.mo$stn %in% stn.targets, ], aes(x = mo, y = Mm3.day)) + 
  geom_point() + geom_errorbar(aes(ymin = Mm3.day - Mm3.day.se, ymax = Mm3.day + Mm3.day.se), width = 0) +
  theme_classic() + facet_grid(stn ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + xlab("Month") + 
  ylab(expression("Mean daily flow (1e"^6~"m"^3%.%"day"^-1~")"))
ggplot(q.mo[q.mo$stn %in% stn.targets, ], aes(x = mo, y = headwtr)) + 
  geom_point() + geom_errorbar(aes(ymin = headwtr - headwtr.se, ymax = headwtr + headwtr.se), width = 0) +
  theme_classic() + facet_grid(stn ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + xlab("Month") + 
  ylab(expression("Mean headwater (ft NGVD29?)"))


flowSeasS12s <- ggplot(q.mo[q.mo$stn %in% stn.targets, ], aes(x = mo, y = Mm3.day)) + 
  geom_point() + geom_errorbar(aes(ymin = Mm3.day - Mm3.day.se, ymax = Mm3.day + Mm3.day.se), width = 0) +
  theme_classic() + facet_grid(stn ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + xlab("Month") + 
  ylab(expression("Mean daily flow (1e"^6~"m"^3%.%"day"^-1~")"))
flowSeasS12s
# ggsave(paste0("/opt/physical/troy/RDATA/flowVsMo-", todaysDate, ".png"), height = 7, width = 7)
TPSeasS12s <- ggplot(q.mo[q.mo$stn %in% stn.targets, ], aes(x = mo, y = P)) + 
  geom_point() + geom_errorbar(aes(ymin = P - P.se, ymax = P + P.se), width = 0) +
  theme_classic() + facet_grid(stn ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + xlab("Month") + 
  ylab(expression("Total P (mg P"%.%"L"^-1*")"))
TPSeasS12s 

chlSeasS12s <- ggplot(q.mo[q.mo$stn %in% stn.targets, ], aes(x = mo, y = chl)) + 
  geom_point() + geom_errorbar(aes(ymin = chl - chl.se, ymax = chl + chl.se), width = 0) +
  theme_classic() + facet_grid(stn ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + xlab("Month") + 
  ylab(expression("Chlorophyll a (mg"%.%"L"^-1*")"))
chlSeasS12s

grid.arrange(flowSeasS12s, TPSeasS12s, chlSeasS12s, ncol = 3)





### 




### ratio of flow during wet:dry season
# q.mo$seas <- "wet"
# q.mo$seas[(q.mo$seas > 10) & (q.mo$seas < 5)] <- "dry"

q.seas <- ddply(hydDat[hydDat$stn %in% stns.wo.NAs, ], .(stn), summarise,
                Mm3.day.wet = mean(flow[seas %in% "wet"], na.rm = TRUE) * 0.0283168466 * 60 * 60 * 24 / 1e6,
                #Mm3.day.se = se(flow) * 0.0283168466 * 60 * 60 * 24 / 1e6,
                Mm3.day.dry = mean(flow[seas %in% "dry"], na.rm = TRUE) * 0.0283168466 * 60 * 60 * 24 / 1e6,
                wetDry  = Mm3.day.wet / Mm3.day.dry
) # m3/day

modResults <- join_all(list(modResults, q.seas), by = "stn")

plot(modResults[, c(3, 7:11, 14)])
plot(modResults[!modResults$stn %in% grep("S356|S344", modResults$stn, value = TRUE), c(3, 7:11, 14)])


plot(ratio ~ wetDry, data = modResults, cex = 0.5, pch = 19)
text(x = modResults$wetDry, y = modResults$ratio, labels = modResults$stn)

plot(log(ratio) ~ log(wetDry), data = modResults, cex = 0.5, pch = 19)
text(x = log(modResults$wetDry), y = log(modResults$ratio), labels = modResults$stn)


#' 
#' 
#' 
#' 
#' 
#' 
#' \pagebreak
#' 
#' ## Explanations for P behavior at S-333
#' 
#' 
#' A number of mechanisms could produce the anomalous, chemostatic behavior of P at S-333. Several of these hypotheses can be coarsely evaluated with existing water quality data. The hypotheses explored here propose that the distinct P behavior at S-333 is due to: 
#' 
#' 1. Biological processes
#' 
#'     + Chlorophyll
#' 
#'     + Vertebrate activity (not testable with present dataset)
#' 
#' 
#' 2. Sorption dynamics
#' 
#'     + pH
#' 
#'     + Calcium
#' 
#' 
#' 3. Different source contributions
#' 
#'     + groundwater/EAA tracers (Na:Ca ratios, specific conductivity)
#' 
#'     + agricultural tracers (carbophenothion, endosulfan sulfate, malathion, chlordane - limited and inconclusive; data not show)
#' 
#'     + sediment availability (proximity of S-333 to the L-67N)
#' 
#' 
#' 
#' 
#' ### Biological processes
#' 
#' Biological activity could be an explanation for higher P at S-333. Chlorophyll a data are the sole biological activity indicator used in this assessment, ignoring vertebrate activity that also merits attention. 
#' 
#' Chlorophyll a concentrations were significantly higher at S-333 than at the three westernmost S-12 structures (S-12B-D) and show a strong relationship with TP concentrations (*R^2^* = 0.55, *P* < 0.001; Fig. \ref{fig:bioProc}). Chlorophyll a is difficult to interpret as an indicator of the source of P because abundant phytoplankton are likely to be both a cause and an effect of elevated total P. The strong relationship between TP and chlorophyll a is thus interesting but inconclusive; the increased chlorophyll a at S-333 may be contributing to elevated P at that station, or elevated TP may be contributing to phytoplankton abundance.
#' 
#' 
## ----bioProc, echo = FALSE, warning = FALSE, fig.height = 2, fig.width = 5, fig.cap = "\\label{fig:bioProc}Left panel: Chlorophyll A at each station during sampling events with positive flow. Right side: Relationship between chlorophyll A and  TP (all data). In the left panel, different letters indicate significant differences at P < 0.05 (Tukey post-hoc test). Data ln-transformed for normality."----

### idea - look at S151 to get upstream

### boxplots
subDat <- wq2[wq2$flow > 0, ]
# testType <- "Tukey"
# aov1 <- aov(I(log(subDat[, "CHLOROPHYLL-A, CORRECTED"])) ~ st.fac, data = subDat)
# summary(glht(aov1, linfct = mcp(st.fac=testType))) # S-333 diff from all but S12A

wq.melt2 <- melt(wq2[wq2$flow > 0, ], id.vars = "stn", measure.vars = c("CHLOROPHYLL-A, CORRECTED"))
ann_cond <- data.frame(stn = c("S333", paste0("S12", toupper(letters[1:4]))), 
                       variable = rep(c("CHLOROPHYLL-A, CORRECTED"), times = 5),
                       lab = c("B", "AB", "A", "A", "A"),
                       value = rep(20, times = 5))

b.plots <- ggplot(wq.melt2, aes(x = stn, y = value)) + 
  geom_jitter(width = 0.1, size = 0.3, col = "darkgray") + geom_boxplot(outlier.alpha=0) + 
  theme_classic() + # facet_grid(. ~ variable, scales = "free_y") +
  theme(legend.position="none", #strip.text.x = element_text(size = 8),
        axis.text.x = element_text(angle = 0, hjust = 0.5), text = element_text(size=9), plot.title = element_text(hjust = 0.5)) + 
  xlab("") + ylab("Chlorophyll a, corrected (mg/L)") + 
  scale_y_log10() + 
  geom_text(data = ann_cond, label = ann_cond$lab)




# ### C-P relationships - split by station
# ggplot(dat2[!is.na(dat2$naca) & !is.na(dat2[, "PHOSPHATE..TOTAL.AS.P"]), ], 
#        aes(x = dat2[!is.na(dat2$naca) & !is.na(dat2[, "PHOSPHATE..TOTAL.AS.P"]), "naca"], y = dat2[!is.na(dat2$naca)& !is.na(dat2[, "PHOSPHATE..TOTAL.AS.P"]), "PHOSPHATE..TOTAL.AS.P"], colour = stn)) + 
#   coord_cartesian(ylim=c(0, 0.06)) +
#   geom_point(alpha = 0.6, size = 0.6) + geom_smooth(method = "lm") + 
#   theme_classic() + #facet_grid(. ~ stn) + 
#   theme(legend.position="top", axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), 
#         text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + # annotate("text", x = 1.5, y = 0.35, label = c("*", "*", "*", "*", ""), size = 12) +
#   ylab ("Na:Ca (molar ratio)") + xlab("TP (mg/L)") # + 


# ### C-Q relationships - all data! not just flow > 1
a <- summary(lm(log(wq2[, "CHLOROPHYLL-A, CORRECTED"]) ~ log(wq2[, "PHOSPHATE, TOTAL AS P"])))


relpToP <- ggplot(wq2,
                  aes(y = log(wq2[, "PHOSPHATE, TOTAL AS P"]), x = log(wq2[, "CHLOROPHYLL-A, CORRECTED"]))) +
  geom_point(alpha = 0.6, size = 0.6) + geom_smooth(method = "lm") +
  theme_classic() +# facet_grid(. ~ stn) + # scale_y_log10()  +  scale_x_log10() +
  theme(legend.position="top", axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        text = element_text(size=9), plot.title = element_text(hjust = 0.5)) + # annotate("text", x = 1.5, y = 0.35, label = c("*", "*", "*", "*", ""), size = 12) +
  xlab ("Chl. A (mg/L; log scale)") + ylab("Total P (mg/L; log scale)") # +


grid.arrange(b.plots, relpToP, ncol = 2)


#' 
#' 
#' 
#' 
#' 
#' 
#' 
## ----seasonality, echo = FALSE, include = FALSE--------------------------

### Differences in seasonality?
ggplot(wq2[!is.na(wq2[, "PHOSPHATE, TOTAL AS P"]), ], aes(x = stn, y = `PHOSPHATE, TOTAL AS P`)) + 
  geom_boxplot(outlier.alpha=0) + geom_jitter(width = 0.1, size = 0.3) + 
  theme_classic() + geom_hline(yintercept = 0.010, lty = 2) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) +  
  ylab ("Total P (mg/L)") + xlab("") + facet_grid(seas ~ ., scales = "fixed")



#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' ### Sorption dynamics
#' 
#' Sorption processes provide a second tier of potential explanations for the P dynamics at S-333. In freshwater systems, pH below ~8 leads to maximum P sorption (Bai et al. 2017) and scavenging by particles, potentially reducing water column P. Dissolved calcium (Ca^2+^), though not measured at S-12D, is also included in this section because of the potential for formation and precipitation of calcium phosphate. 
#' 
#' The range of pH values in our data (Fig. \ref{fig:sorption}) fall within the range of optimal and stable sorption identified by Bai et al. (2017) for freshwater wetland soils, and could explain why no relationship between the two variables is apparent iskmaklmfaslkmslksdlksd;mlkfsdl;kmds;lkjd.
#' 
#' Lower water column Ca at S-333 and S-12A (Fig. \ref{fig:sorption}) seems consistent with higher P concentrations at those stations; less Ca available for forming complexes and removing P from the water column. Diaz et al. (1994) characterized P solubility in response to gradients of Ca and pH, finding increased precipitation with increasing Ca levels (over the range 25-110 mg Ca L^-1^). At Ca levels similar to those we observe, Diaz et al. (1994) found minimal effects of pH < 9. There is potential for reductions in pH to dissolve CaP.
#' 
#' However, we actually see a positive association between Ca and TP concentrations (Fig. \ref{fig:sorption}), contrary to what would be suggested by their theoretical relationship and the general trends in the two solutes among stations. 
#' 
#' 
#' 
#' 
## ----sorption, echo = FALSE, warning = FALSE, fig.height = 4, fig.width = 5, fig.cap = "\\label{fig:sorption}Left panels: pH and calcium concentrations at each station during sampling events with positive flow. Different letters for each station indicate that all pairwise comparisons are significant at P < 0.001 (Tukey post-hoc test). Right panels: relationships between TP concentrations and pH (top) and Ca (bottom), with regression line shown if the relationship was significant."----

### idea - look at S151 to get upstream

### boxplots
subDat <- wq2[wq2$flow > 0, ]
# testType <- "Tukey"
# aov1 <- aov(I(subDat[, "PH, FIELD"]) ~ st.fac, data = subDat)
# summary(glht(aov1, linfct = mcp(st.fac=testType))) # S333 different from 12B/C; 12A diff from B/C; S12D diff from B/C
# 
# summary(glht(aov(I(wq2$CALCIUM) ~ st.fac, data = wq2), linfct = mcp(st.fac=testType)))
# testType <- "Tukey"
# aov1 <- aov(I(log(subDat[, "CALCIUM"])) ~ st.fac, data = subDat)
# summary(glht(aov1, linfct = mcp(st.fac=testType))) # all differences significant except S12C/B

wq.melt2 <- melt(subDat, id.vars = "stn", measure.vars = c("PH, FIELD", "CALCIUM"))

ann_ph <- data.frame(stn = c("S333", paste0("S12", toupper(letters[1:4]))), 
                     variable = rep(c("PH, FIELD"), times = 5),
                     lab = c("B", "AB", "C", "C", "B"),
                     value = rep(9, times = 5))
ann_ca <- data.frame(stn = c("S333", paste0("S12", toupper(letters[1:4]))), 
                     variable = rep(c("CALCIUM"), times = 5),
                     lab = c("C", "A", "B", "B", ""),
                     value = rep(85, times = 5))

sorption.boxplots <- ggplot(wq.melt2, aes(x = stn, y = value)) + 
  geom_jitter(width = 0.1, size = 0.3, col = "darkgray") + geom_boxplot(outlier.alpha=0) + 
  theme_classic() + facet_grid(variable ~ ., scales = "free") +
  theme(legend.position="none", #strip.text.x = element_text(size = 8),
        axis.text.x = element_text(angle = 0, hjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + xlab("") + ylab("") +
  #+ scale_y_log10() + 
  geom_text(data = ann_ph, label = ann_ph$lab) +   geom_text(data = ann_ca, label = ann_ca$lab)


### pH/Ca - phosphorous scatterplots
# summary(lm(wq2[, "PHOSPHATE, TOTAL AS P"] ~ wq2[, "PH, FIELD"])) # pH-P relp not significant
pHPlot <- ggplot(wq2,
                 aes(y = wq2[, "PHOSPHATE, TOTAL AS P"], x = wq2[, "PH, FIELD"])) +
  geom_point(alpha = 0.6, size = 0.3) + #geom_smooth(method = "lm") + 
  theme_classic() + #facet_grid(variable ~ ., scales = "free") +
  theme(legend.position="none", #strip.text.x = element_text(size = 8),
        axis.text.x = element_text(angle = 0, hjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + xlab("pH") + ylab("Total P (mg/L)")

# summary(lm(wq2[, "PHOSPHATE, TOTAL AS P"] ~ wq2[, "CALCIUM"])) # Ca-P relp significant, slope 6.4e-5, r2 = 0.01
CaPlot <- ggplot(wq2,
                 aes(y = wq2[, "PHOSPHATE, TOTAL AS P"], x = wq2[, "CALCIUM"])) +
  geom_point(alpha = 0.6, size = 0.3) + geom_smooth(method = "lm") + 
  theme_classic() + #facet_grid(variable ~ ., scales = "free") +
  theme(legend.position="none", #strip.text.x = element_text(size = 8),
        axis.text.x = element_text(angle = 0, hjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + xlab("Ca (mg/L)") + ylab("Total P (mg/L)") + ylim(0, 0.25)


grid.arrange(sorption.boxplots, pHPlot, CaPlot, layout_matrix = cbind(c(1, 1), c(2, 3)))


#' 
#' 
#' 
#' 
#' 
#' ### Source contributions
#' 
#' It is possible that the differences in solute behavior are the result of different sources of water predominantly contributing to S-333. For example, the close proximity of S-333 to the L-67N could produce a stronger agricultural signal at that station. This hypothesis has two parts: that differences in source contributions exist, and that the water source predominating at S-333 shares the solute-discharge characteristics observed at S-333. I use water quality data to draw inferences about the source of water at the five structures and evaluate the first part only.
#' 
#' Molar ratios of Na:Ca have been considered to provide a signature for the source of water, with higher ratios indicating agricultural sources and lower ratios treated as indicative of sheetflow over WCA 3A. 
#' 
#' Ratios of Na:Ca are not available for S-12D, but stations otherwise show decreasing relative influence of agricultural inputs moving west from S-333 to S-12A (Fig. \ref{fig:NaCa}). This pattern also appears in the specific conductivity data, which are available for all stations.
#' 
#' 
#' 
#' 
## ----agricultureContribution, echo = FALSE, warning = FALSE, fig.height = 3, fig.width = 5, fig.cap = "\\label{fig:NaCa}Molar ratios of Na+:Ca2+ (left panel) and specific conuctivity (right panel) during sampling events with positive flow. Different letters for each station indicate that all pairwise comparisons are significant at P < 0.001 (Tukey post-hoc test). Data ln-transformed for normality."----

### idea - look at S151 to get upstream

### boxplots
subDat <- wq2[wq2$flow > 0, ]
# testType <- "Tukey"
# aov1 <- aov(I(subDat[, "naca"]) ~ st.fac, data = subDat)
# summary(glht(aov1, linfct = mcp(st.fac=testType))) # all differences significant (Tukey; log-transformed or not)
# 
# summary(glht(aov(I(wq2$CALCIUM) ~ st.fac, data = wq2), linfct = mcp(st.fac=testType)))
# testType <- "Tukey"
# aov1 <- aov(I(subDat[, "SP CONDUCTIVITY, FIELD"]) ~ st.fac, data = subDat)
# summary(glht(aov1, linfct = mcp(st.fac=testType))) # all differences significant (Tukey; log-transformed or not)

wq.melt2 <- melt(wq2[wq2$flow > 0, ], id.vars = "stn", measure.vars = c("naca", "SP CONDUCTIVITY, FIELD"))
ann_naca <- data.frame(stn = c("S333", paste0("S12", toupper(letters[1:4]))), 
                       variable = rep(c("naca"), times = 5),
                       lab = c("D", "A", "B", "C", ""),
                       value = rep(0.12, times = 5))
ann_cond <- data.frame(stn = c("S333", paste0("S12", toupper(letters[1:4]))), 
                       variable = rep(c("SP CONDUCTIVITY, FIELD"), times = 5),
                       lab = c("E", "A", "B", "C", "D"),
                       value = rep(100, times = 5))

ggplot(wq.melt2, aes(x = stn, y = value)) + 
  geom_jitter(width = 0.1, size = 0.3, col = "darkgray") + geom_boxplot(outlier.alpha=0) + 
  theme_classic() + facet_grid(.~ variable) +
  theme(legend.position="none", #strip.text.x = element_text(size = 8),
        axis.text.x = element_text(angle = 0, hjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + xlab("") + ylab("") + 
  scale_y_log10() + 
  geom_text(data = ann_naca, label = ann_naca$lab) +   geom_text(data = ann_cond, label = ann_cond$lab)




# ### C-P relationships - split by station
# ggplot(dat2[!is.na(dat2$naca) & !is.na(dat2[, "PHOSPHATE..TOTAL.AS.P"]), ], 
#        aes(x = dat2[!is.na(dat2$naca) & !is.na(dat2[, "PHOSPHATE..TOTAL.AS.P"]), "naca"], y = dat2[!is.na(dat2$naca)& !is.na(dat2[, "PHOSPHATE..TOTAL.AS.P"]), "PHOSPHATE..TOTAL.AS.P"], colour = stn)) + 
#   coord_cartesian(ylim=c(0, 0.06)) +
#   geom_point(alpha = 0.6, size = 0.6) + geom_smooth(method = "lm") + 
#   theme_classic() + #facet_grid(. ~ stn) + 
#   theme(legend.position="top", axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), 
#         text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + # annotate("text", x = 1.5, y = 0.35, label = c("*", "*", "*", "*", ""), size = 12) +
#   ylab ("Na:Ca (molar ratio)") + xlab("TP (mg/L)") # + 


# ### C-Q relationships
# ggplot(dat2[!is.na(dat2$naca) & (dat2$flow > 0), ], 
#        aes(x = log(dat2[!is.na(dat2$naca) & (dat2$flow > 0), "naca"]), y = log(dat2[!is.na(dat2$naca) & (dat2$flow > 0), "flow"]))) + 
#   geom_point(alpha = 0.6, size = 0.6) + geom_smooth(method = "lm") + 
#   theme_classic() + facet_grid(. ~ stn) + # scale_y_log10()  +  scale_x_log10() +
#   theme(legend.position="top", axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), 
#         text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + # annotate("text", x = 1.5, y = 0.35, label = c("*", "*", "*", "*", ""), size = 12) +
#   ylab ("Na:Ca (molar; log scale)") + xlab("Discharge (cfs; log scale)") # + 
#   



#' 
#' 
#' 
#' 
#' 
## ----driver, include = FALSE, echo = FALSE, warning = FALSE, fig.height = 3, fig.width = 3, fig.cap = "\\label{fig:tss}The relationship between suspended sediment and total P concentrations. ANCOVA suggested no difference in slope between stations, so data are pooled in this figure. Slope from regression line suggests a 1 mg/L increase in suspended solids increases total P by 0.5+-0.1 ppb (but with low explanatory power: R2 = 0.025; P < 0.001)."----

# summary(lm(wq2[, 'PHOSPHATE, TOTAL AS P'] ~ wq2[, 'TOTAL SUSPENDED SOLIDS'])) # r2 only 0.045

# ANCOVA: no difference in slope between stations
# summary(lm(wq2[, 'PHOSPHATE, TOTAL AS P'] ~ wq2[, 'TOTAL SUSPENDED SOLIDS'] * wq2$stn))

ggplot(wq2, aes(x = wq2[, 'TOTAL SUSPENDED SOLIDS'], y = wq2[, 'PHOSPHATE, TOTAL AS P'])) + 
  geom_point() + 
  theme_classic() + #geom_hline(yintercept = 0.010, lty = 2) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + xlab("Suspended solids (mg/L)") + ylab("Total P (mg/L)") + geom_smooth(method = "lm")






#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' # References
#' 
#' Bai et al. 2017. doi: 10.1016/j.chemosphere.2017.08.117
#' 
#' Hanlon et al. 2010. doi: 10.2134/jeq2009.0488
#' 
#' Diamond and Cohen 2017. doi: 10.1002/hyp.11424
#' 
#' Musolff et al. 2015. doi: 10.1016/j.advwatres.2015.09.026
#' 
