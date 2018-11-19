% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dfe_hydro.R
\name{getHydro}
\alias{getHydro}
\title{DataForEver hydrology database API}
\usage{
getHydro(stns, parameter_list = c("flow", "tail_water", "head_water", "stage"),
data_shape = "long", getWaterQuality = FALSE, ...
)
}
\arguments{
\item{stns}{a single-column dataframe of DataForEver station names.}

\item{parameter_list}{vector of desired parameters.}

\item{data_shape}{shape of output dataframe. Default is \code{long} (one row per date-station-param) but can also be \code{wide} (one row per date-station) or \code{really_wide} (one row per date)}

\item{getWaterQuality}{if \code{TRUE}, water quality data are also downloaded and combined into a single dataframe. If \code{getWaterQuality = TRUE}, then \code{data_shape} is automatically set to \code{wide}.}

\item{...}{additional arguments sent to \code{dfe.wq}}
}
\value{
dataframe \code{getHydro} returns a dataframe with water quality measurements from each station identified in \code{stns}.
}
\description{
Downloads and compiles DataForEver hydrology data. This function Works only on linux machines on the SFNRC network with access to the opt/physical drive. Code issues system commands, runs shell scripts, and modifies files in a temp folder on the local hard drive.
}
\examples{
\dontrun{
stations <- c("S333", "S12A", "S12B", "S12C", "S12D")

  # usage examples: 
  hydro.test  <- getHydro(stns = stations)
  hydro.test2 <- getHydro(stns = stations, data_shape = "wide")
  hydro.test3 <- getHydro(stns = stations, data_shape = "really_wide")
  
  # simultaneously grab water quality data:
  hydro.test4 <- getHydro(stns = stations, data_shape = "wide", getWaterQuality = TRUE, 
       parameter_list = c("flow", "head_water"), target_analytes = "PHOSPHATE, TOTAL AS P")
       
 plot(PHOSPHATETOTALASP ~ flow, data = hydro.test4[hydro.test4$flow > 0, ], pch = 19, las = 1)
 plot(PHOSPHATETOTALASP ~ head_water, data = hydro.test4[hydro.test4$flow > 0, ], pch = 19, las = 1)
  
  
  ### to generate the hydDat dataframe included in package:
  targetStns <- c("AC01", "BB02", "BB04", "BB05A", "BB06", "BB09", "BB10", "BB11", 
"BB14", "BB15", "BB16", "BB17", "BB19", "BB22", "BB23", "BB24", 
"BB25", "BB26", "BB27", "BB28", "BB29", "BB31", "BB32", "BB34", 
"BB35", "BB36", "BB37", "BB38", "BB39A", "BB41", "BB42", "BB43", 
"BB44", "BB45", "BB47", "BB48", "BB52", "BB53", "BISC101", "BISC102", 
"BISC103", "BISC104", "BISC105", "BISC106", "BISC107", "BISC108", 
"BISC109", "BISC110", "BISC111", "BISC112", "BISC113", "BISC114", 
"BISC115", "BISC116", "BISC117", "BISC119", "BISC120", "BISC121", 
"BISC122", "BISC123", "BISC124", "BISC125", "BISC126", "BISC127", 
"BISC128", "BISC129", "BISC130", "BISC131", "BISC132", "BISC133", 
"BISC134", "BISC135", "BL01", "BS01", "LR01", "MI01", "TPBBSW-1", 
"TPBBSW-10", "TPBBSW-14", "TPBBSW-1B", "TPBBSW-1T", "TPBBSW-2", 
"TPBBSW-2B", "TPBBSW-3", "TPBBSW-3B", "TPBBSW-4", "TPBBSW-4B", 
"TPBBSW-4T", "TPBBSW-5", "TPBBSW-5B", "TPBBSW-5T", "TPGW-11D", 
"TPGW-11M", "TPGW-11S", "TPGW-14D", "TPGW-14M", "TPGW-14S", "UPKEYS201", 
"UPKEYS202", "UPKEYS203", "UPKEYS204", "UPKEYS205", "UPKEYS206", 
"UPKEYS207", "UPKEYS208", "UPKEYS210", "S20", "S20F", "S20G", 
"S21A", "S21", "S22", "S25", "S25A", "S25B", "S26", "S27", "S28", 
"G58", "S700", "G93", "S123", "S197")

hyd.df <- getHydro(stns = targetStns, parameter_list = c("flow", "head_water", "salinity", 
     "temperature", "tail_water", "stage", "rainfall", "precipitation", "ppt"), 
     data_shape = "wide")
}

}
\seealso{
\code{\link{getWQ}}
}