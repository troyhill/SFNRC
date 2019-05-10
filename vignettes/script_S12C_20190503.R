### script to download and summarize DataForEver hydro data for a set of stations

### install.packages("devtools") # install devtools package, if it's not installed already
devtools::install_github("troyhill/SFNRC")

library(SFNRC)
library(plyr)
library(ggplot2)

### stations can be defined manually
stns <- c("S333", "S12D", "S12C", "S332")

dat <- getHydro(stns = stns, data_shape = "wide")
head(dat)


### summarize, e.g., mean annual flow
dd.dat <- ddply(dat, .(stn, year), summarise,
                Q = mean(flow, na.rm = TRUE),
                Q.se = se(flow))

ggplot(dd.dat, aes(x = year, y = Q)) + theme_classic() + facet_grid(stn ~ .) +
  geom_pointrange(aes(ymin = Q - Q.se, ymax = Q + Q.se), size = 0.3)





# Analysis of S12C --------------------------------------------------------



a <- dbhydro.stn(stations = "S12D", import = TRUE)
names(a)[2] <- "stn"
head(a)

a$date <- NA 
for (i in 1:nrow(a)) {
  a$date[i] <- ifelse(grepl(x = a$First.Trigger.Date[i], pattern = "$^") | is.na(a$First.Trigger.Date[i]), a$Collection_Date[i], a$First.Trigger.Date[i])
}

gsub("([a-zA-Z]{2}-.*)", "\\L\\1", a$date[1], perl=TRUE) # really tough to convert all-cap month into title case month inside of a string.

a$date <- gsub(pattern = "([a-zA-Z]{2}-.*)", replacement = "\\L\\1", x = a$date, perl=TRUE)
a$date <- as.POSIXct(strptime(a$date, format = "%d-%b-%Y %H:%M"))

tempDat <- a[a$Test.Name %in% c("PHOSPHATE, TOTAL AS P", "TURBIDITY"), c("stn", "date", "Test.Name", "Value")]

### average measurements on same day
tempDat <- ddply(tempDat, .(stn, date, Test.Name), summarise,
                 value = mean(Value, na.rm = TRUE))

### reshape dataset to one column per parameter 
wideDat  <- stats::reshape(tempDat,
                           idvar = c("stn", "date"),
                           timevar = "Test.Name", direction = "wide")
names(wideDat) <- gsub(x = names(wideDat), pattern = "value.| |,", replacement = "")

tail(wideDat)

plot()