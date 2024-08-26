#' @title Function summarizes National Weather Service precipitation geotiff files by polygon
#'
#' @description Summarize spatial data using a polygon layer to return raw values for each date and aggregate measures by month.
#'
#' @param tif_addresses character vector with full file names for tiff files to be summarized (including entire directory and file extension). Can be created with `list.files('C:/directory', full.names = TRUE)`.
#' @param parameter_pattern character element used in a grep query to identify which layer to use. Note that this must select the desired layer from both Stage 3 and Stage 4 data (so the query should likely be an OR statement, e.g., '_1|Observed' for observed data). Layer names for Stage 3 data appear to be `Observed`, `Normal`, `Departure from Normal`, and `Percent of Normal`. Layer names for Stage 4 data are the filename and a numeric value between 1 and 4.
#' @param input_polygon Spatvector with polygons used to summarize precipitation data. Load with `terra::vect`. Example polygon data: https://catalog.data.gov/dataset/tiger-line-shapefile-2019-state-georgia-current-county-subdivision-state-based
#' @param polygon_names character. Name of column in `input_polygon` containing feature names/IDs. If left as `NULL`, features are numbered by their row in the polygon object.
#' @param create_plots logical. Optional visualization produced for each layer. Setting this to `TRUE` will dramatically slow down the run time.
#' @param func function. Used to summarize the monthly data from each polygon.
#' @param date_correction numeric. Optional correction for data date. If no correction is desired, set to 0.
#'
#' @return list \code{summarizeNWS} containing (1) `summary_table`, a summary dataframe showing the result of `func` applied to monthly values for Jan-Dec. More complex operations can be performed on the raw values, and (2) `all_data`, a dataframe containing the raw values from each polygon and each layer.
#' 
#' @importFrom terra rast
#' @importFrom terra extract
#' @importFrom terra plot
#' @importFrom terra project
#' @importFrom terra crs
#' @importFrom terra crop
#' @importFrom terra extract
#' @importFrom terra values
#' @importFrom stats aggregate
#' @importFrom stats reshape
#' 
#' @export
#'
#' @examples
#' 2+2
summarizeNWS <- function(tif_addresses,
                         parameter_pattern = '_1|Observed',
                         input_polygon, # shapefile, e.g., 
                         polygon_names = NULL, # optional; name of the column in the polygon object with feature IDs/names
                         create_plots = FALSE,
                         func = median, # value to report in the final summary table (max, min, mean, etc.)
                         date_correction = -1 # optional correction for data date. 
) {
  
  if (!is.null(polygon_names)) {
    ### confirm polygon names are present
    if (!polygon_names %in% names(input_polygon)) {
      polygon_names <- NULL
    }
  }
  if (is.null(polygon_names)) {
    polygon_names <- 1:nrow(input_polygon)
  }
  
  for (i in 1:length(tifs)) {
    tif_address <- tifs[i]
    ### get date
    split_file_name <- strsplit(x = gsub(x = basename(tif_address), pattern = 'conus|pr|ak|\\.tif', replacement = ''), split = '_')[[1]]
    date_value      <- split_file_name[length(split_file_name)]
    
    ### get data
    dat1 <- terra::rast(tif_address)
    input_polygon <- terra::project(input_polygon, terra::crs(dat1, proj = TRUE))
    
    dat1 <- dat1[[grep(x = names(dat1), pattern = parameter_pattern)]]
    dat1 <- terra::crop(dat1, input_polygon, mask = TRUE)
    
    if(create_plots) {
      terra::plot(dat1, main = paste0('Pixel-level rainfall:\n', as.character(as.Date(date_value, format = '%Y%m%d') - 1)))
      terra::plot(input_polygon, add = TRUE)
    }
    
    ### get pixel values for each county
    extracted.int <- sapply(dat1, function(x) {
      x.df <- data.frame(t(terra::extract(x, y = input_polygon, 
                                          fun = mean, na.rm = TRUE)))
      x.df <- x.df[!grepl(x = row.names(x.df), pattern = "ID"), 
      ]
    })
    extracted.int           <- data.frame(extracted.int)
    names(extracted.int)[1] <- paste0('precip', # split_file_name[length(split_file_name) - 2],
                                      '_', split_file_name[length(split_file_name) - 1])
    extracted.int$monthYear <- as.Date(date_value, format = '%Y%m%d') + date_correction # using -1 is useful for full-month rainfall; changes date to last day of prior month. Assuming that e.g., June 1st reflects May rainfall totals
    extracted.int$month     <- format(extracted.int$monthYear, '%b')
    extracted.int$name      <- terra::values(input_polygon[, polygon_names])[,1]
    
    if(create_plots) {
      input_polygon$mtd <- extracted.int[,1]
      terra::plot(input_polygon, 'mtd', main = paste0('Polygon-level rainfall:\n', as.character(as.Date(date_value, format = '%Y%m%d') - 1)))
    }
    if (i == 1) {
      finDat <- extracted.int
    } else {
      finDat <- rbind(finDat, extracted.int)
    }
  }
  
  ### now create a table
  tbl1 <- stats::aggregate(unlist(get(names(finDat)[1])) ~ month + name, data = finDat, 
                           FUN = function(x) func(x, na.rm = TRUE))
  names(tbl1)[3] <- names(finDat)[1]
  tbl2 <- stats::reshape(tbl1, idvar = "name", timevar = "month", direction = "wide")
  names(tbl2)[2:ncol(tbl2)] <- substr(names(tbl2)[2:ncol(tbl2)], nchar(names(tbl2)[2:ncol(tbl2)])-2, nchar(names(tbl2)[2:ncol(tbl2)]))
  tbl2 <- tbl2[, c('name', month.abb[month.abb %in% names(tbl2)[2:ncol(tbl2)]])]
  head(tbl2)
  row.names(tbl2) <- 1:nrow(tbl2)
  
  ### return the table
  return(list(summary_table = tbl2,  # monthly means
              all_data      = finDat # raw data, long format
  ))
}
