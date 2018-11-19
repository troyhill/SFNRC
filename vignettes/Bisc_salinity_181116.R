### script explores salinity data from Biscayne Bay
### monthly spatial interpolations and analysis of area-under-20-psu
### relationship with canal inflow


# identify stations (may be missing some)
bsc.stns <- dfe.data.types(stn = "BISC|BB")
unique(bsc.stns$parameter)
stns <- as.character(unique(bsc.stns[bsc.stns$parameter %in% "salinity", "stn"]))

# get data
paramsToGet <- c("salinity", "surface_temperature", "bottom_temperature", "oxygen_solubility")
bsc      <- dfe.hydro(stns = stns, parameter_list = paramsToGet)
bsc.wide <- dfe.hydro(stns = stns, parameter_list = paramsToGet, data_shape = "wide")
beep(2)

