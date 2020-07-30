### Function fills missing values in time series data using the empirical data distribution 

# functions ---------------------------------------------------------------

fillValues <- function(data, stationColumns = grep(x = names(data), pattern = "value", value = TRUE), # wide dataset - one row per date 
                       dateColumn = "date") {
  ### function fills missing vaues by randomly sampling observed values from a given date
  date.lt <- as.POSIXlt(data[, dateColumn] )
  
  # brute force... create yearday samples of available data
  days      <- seq( 1, 365 )
  data.yday <- list()
  
  for ( day in days ) {
    i.yday <- which( date.lt$yday == day )
    
    all.yday.data <- c()
    
    for ( station in stationColumns ) {
      yday.data <- data[ i.yday, station ]
      i.na      <- which( is.na( yday.data ) )
      yday.data <- yday.data[ -i.na ]
      
      all.yday.data = c( all.yday.data, yday.data )
    }
    
    data.yday[[ day ]] <- all.yday.data
  }
  
  # For each station sample the distribution for missing data
  for ( station in stationColumns ) {
    
    
    i.na <- which( is.na( data[ , station ] ) )
    
    print( paste0("station ", station, " had ", length(i.na), " NA values (", round(length(i.na) / length(data[, station])*100),  "% of data)"))
    
    for ( i in i.na ) {
      yday <- date.lt[ i ] $ yday
      
      if ( yday == 0 ) { yday <- 1 }
      
      # Choose randomly from the available data for this yearday
      value <- max( sample( data.yday[[ yday ]], size = 2, replace = TRUE ), na.rm = TRUE)
      # Assing to the missing data
      data[ i, station ] <- value
    }
  }
  invisible(data)
}


