#' Convert units
#'
#' @param data.source can be "dss" or ...
#' @param station.type station type
#' @param units units desired
#' @param graph_type graph produced
#' @param digits
#'
#' @return value
#'
#' 
#' @importFrom stats sd
#' @export
unitConversion = function(
  #data.type          = 'stage',   # stage, flow, vector
  data.source        = 'dss',
  #variable           = 'ComputedHead',
  station.type       = 'transect',
  units              = 'kaf',
  graph_type         = 'barchart',
  #aggTEST            = 'sum',
  digits.precision   = 3, 
  DEBUG              = FALSE
) {
  # in dss transect flows ft^/day; structures cfs
  #cf=1.0            #only for ${data.source} == netCDF
  #if [[ ${units} == cfs ]]; then cf=0.5041593143 
  cf <- 1
  if ( tolower(units) == 'k-af'  | tolower(units) == 'kac-ft' ) {
    units <- 'kaf'  
  }   
  
  if ( tolower(station.type) == 'transect' ) {
    
    if ( tolower(units) == 'cfs' ) { 
      
      if ( tolower(data.source) == 'netcdf' ) {
        cf <- 504.15931434  # may need to change as R code is updated
      }  else {
        cf <- 0.00001157
        #print('the units are cfs')
      }
    } 
    
    if ( tolower(units) == 'cms' ) { 
      
      if ( tolower(data.source) == 'netcdf' ) {
        cf <- 504.15931434 * 0.0283168  # may need to change as R code is updated
      }  else {
        cf <- 0.00001157 * 0.0283168
        #print('the units are cfs')
      }
    } 
    
    if ( tolower(units) == 'kaf' ) { 
      
      if ( tolower(data.source) == 'netcdf' ) {
        cf <- 1.0  # may need to change as R code is updated
      }  else {
        cf <- 0.000000023
        #print('the units are kaf')
      }
    } 
    
    if ( tolower(units) == 'af' ) { 
      
      if ( tolower(data.source) == 'netcdf' ) {
        cf <- 1000  # may need to change as R code is updated
      }  else {
        cf <- 0.000023
        #print('the units are af')
      }
    } 
    
    #   } else if ( tolower(units) == 'cfs' & tolower(station.type) != 'transect' ) {           # cfs-days to cfs-days
    #      cf <- 1.0
    
    
  } else if ( tolower(units) == 'af' & tolower(station.type) != 'transect' ) {      # cfs-days to af
    cf <- 1.9835
    
  } else if ( tolower(units) == 'kaf' & tolower(station.type) != 'transect' ) {   # cfs-days to kaf
    cf <- 0.0019835
    
  } else if ( tolower(units) == 'cms' & tolower(station.type) != 'transect' ) {   # cfs to cms
    cf <- 0.0283168
    
  } else if ( tolower(units) == 'mm' ) {
    cf=304.8
    
  } else if ( tolower(units) == 'cm' ) {
    cf <- 30.48
    
  } else if ( tolower(units) == 'm' | tolower(units) == 'meters' ) {
    cf <- 0.3048
    
  } else if ( tolower(units) == 'inch' | tolower(units) == 'in' | tolower(units) == 'inches' ) {
    cf <- 12.0
    
  } else {
    print("reported units are same as model output ")
    cf <- 1.0
    
  }
  
  print("")
  print(paste("the station.type is ",station.type, sep=''))
  print(paste("the units are ",units, sep=''))
  print(paste("the conversion factor is ",cf, sep=''))
  print(paste("the statFlag is ",statFlag, sep=''))
  print("")
  
  #return(list(cf,statFlag))
  return(cf)
  
}
