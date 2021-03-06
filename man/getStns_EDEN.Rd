% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getStnsEDEN.R
\name{getStns_EDEN}
\alias{getStns_EDEN}
\title{Retrieve station coordinates from EDEN}
\usage{
getStns_EDEN(url = "https://sofia.usgs.gov/eden/stationlist-area.php?area=WCA3")
}
\arguments{
\item{url}{character string. Web address with EDEN stations (e.g., https://sofia.usgs.gov/eden/stationlist-area.php?area=WCA3).}
}
\value{
dataframe \code{getCoords_EDEN} returns a dataframe with EDEN stations in the requested region.
}
\description{
Scrapes EDEN station coordinate data.
}
\examples{
\dontrun{
tst <- getStns_EDEN()
### generate list of all stations
areas <- c("WCA1", "WCA2", "WCA3", "BCNP", "ENP", "Pennsuco", "FLBay", "GOM")
area.urls <- c(paste0("https://sofia.usgs.gov/eden/stationlist-area.php?area=", areas))
stn.list <- lapply(X = area.urls, getStns_EDEN)
stns     <- do.call(c, stn.list)
}

}
