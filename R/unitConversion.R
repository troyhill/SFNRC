#' Convert units
#'
#' @param data.source can be "dss" or ...
#' @param station.type station type. options: transect
#' @param units units desired. options: kaf, cfs, cms
#' @param graph_type graph produced
#' @param digits
#'
#' @return value
#'
#' 
#' @importFrom stats sd
#' @export
#' 
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
  
  ### change arguments to lowercase
  units        <- tolower(units)
  station.type <- tolower(station.type)
  graph_type   <- tolower(graph_type)
  data.source  <- tolower(data.source)
  
  ### argument checking
  unit_options <- c("kaf", "kac-ft", "k-af", "cfs", "cms", "af", "mm", "cm", "m", "meters", "inch", "in", "inches")
  if (!units %in% unit_options) stop(paste0("Units argument is not recognized. Acceptable inputs: ", paste(unit_options, collapse = ", ")))
  
  if ( units == 'k-af'  | units == 'kac-ft' ) {
    units <- 'kaf'  
  }   
  
  if ( station.type == 'transect' ) {
    
    if ( units == 'cfs' ) { 
      
      if ( data.source == 'netcdf' ) {
        cf <- 504.15931434  # may need to change as R code is updated
      }  else {
        cf <- 0.00001157
        #print('the units are cfs')
      }
    } 
    
    if ( units == 'cms' ) { 
      
      if ( data.source == 'netcdf' ) {
        cf <- 504.15931434 * 0.0283168  # may need to change as R code is updated
      }  else {
        cf <- 0.00001157 * 0.0283168
        #print('the units are cfs')
      }
    } 
    
    if ( units == 'kaf' ) { 
      
      if ( data.source == 'netcdf' ) {
        cf <- 1.0  # may need to change as R code is updated
      }  else {
        cf <- 0.000000023
        #print('the units are kaf')
      }
    } 
    
    if ( units == 'af' ) { 
      
      if ( data.source == 'netcdf' ) {
        cf <- 1000  # may need to change as R code is updated
      }  else {
        cf <- 0.000023
        #print('the units are af')
      }
    } 
    
    #   } else if ( units == 'cfs' & station.type != 'transect' ) {           # cfs-days to cfs-days
    #      cf <- 1.0
    
    
  } else if ( units == 'af' & station.type != 'transect' ) {      # cfs-days to af
    cf <- 1.9835
    
  } else if ( units == 'kaf' & station.type != 'transect' ) {   # cfs-days to kaf
    cf <- 0.0019835
    
  } else if ( units == 'cms' & station.type != 'transect' ) {   # cfs to cms
    cf <- 0.0283168
    
  } else if ( units == 'mm' ) {
    cf=304.8
    
  } else if ( units == 'cm' ) {
    cf <- 30.48
    
  } else if ( units == 'm' | units == 'meters' ) {
    cf <- 0.3048
    
  } else if ( units == 'inch' | units == 'in' | units == 'inches' ) {
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
