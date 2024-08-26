#' @title Function bulk downloads National Weather Service precipitation geotiff files
#'
#' @description Useful for automating download of large numbers of NWS spatial precipitation layers, and easily download new data. Stage 3 and Stage 4 data can be found at https://water.noaa.gov/resources/downloads/precip/
#'
#' @param data_folder parent directory for data to be deposited. If this does not exist, it will be created.
#' @param overwrite_data TRUE/FALSE value indicating whether existing files should be overwritten. If TRUE, all data are downloaded fresh.
#' @param stage type of data to download. Acceptable values include `stage3` and/or `stage4`.
#' @param stage3_filename example of a stage 3 filename. YYYYMMDD is a token that will be replaced with actual dates. Adjusting this can accommodate odd filenames (e.g., 2017)
#' @param stage4_filename example of a stage 4 filename. YYYYMMDD is a token that will be replaced with actual dates.
#' @param day_range numeric range of days to download from each month. Default only downloads data from the first of the month.
#'
#' @return NULL \code{downloadNWS} downloads files to the provided `data_folder` directory. 
#' 
#' @importFrom terra rast
#' @importFrom terra writeRaster
#' 
#' @export
#'
#' @examples
#' 2+2
downloadNWS <- function(data_folder,
                         overwrite_data = FALSE, # if TRUE, all data is downloaded fresh
                         stage = c('stage3', 'stage4'), # type of data to download
                         stage3_filename = 'nws_precip_mtd_YYYYMMDD.tif', # example of a stage 3 filename. YYYYMMDD is a token that will be replaced with actual dates.
                         stage4_filename = 'nws_precip_mtd_YYYYMMDD_conus.tif',
                         day_range       = 1 # range of days to download from each month. Default only downloads data from the first of the month
) {
  filename <- c(stage3_filename, # stage 3 filename pattern
                stage4_filename) # stage 4 filename pattern
  
  if (any(grepl(x = stage, pattern = '3'))) {
    if (!dir.exists(file.path(data_folder, 'stage3'))) {
      dir.create(file.path(data_folder, 'stage3'), recursive = TRUE)
    }
    ### create list of dates, then urls
    years <- as.character(2005:2017)
    months<- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
    days  <- as.character(day_range)
    days[nchar(days) < 2] <- paste0('0', days[nchar(days) < 2])
    targetDates <- expand.grid(years, months, days)
    targetDates$ymd  <- paste0(targetDates$Var1, targetDates$Var2, targetDates$Var3)
    targetDates      <- targetDates[!grepl(x = targetDates$ymd, pattern = '20050101'), ]
    targetDates$ymd2 <- paste(targetDates$Var1, targetDates$Var2, targetDates$Var3, sep = '/')
    targetDates$url  <- paste0('https://water.noaa.gov/resources/downloads/precip/stageIII/', targetDates$ymd2, '/', mapply(gsub, filename[1], pattern = 'YYYYMMDD', replacement = targetDates$ymd, USE.NAMES = FALSE))
    targetDates$basename <- basename(targetDates$url)
    targetDates$download_location <- file.path(data_folder, 'stage3', targetDates$basename)
    existing_files <- list.files(file.path(data_folder, 'stage3'), full.names = FALSE)
    targetDates$exists_in_directory <- ifelse(targetDates$basename %in% existing_files, 1, 0)
    
    if (overwrite_data == FALSE) {
      targetDates <- targetDates[targetDates$exists_in_directory == 0, ] # remove existing data from list of impending downloads
    }
    
    ### now download and save
    for (i in 1:nrow(targetDates)) {
      message('Downloading Stage 3 data for ', targetDates$ymd2[i])
      tryCatch(
        tmp <- suppressWarnings(terra::rast(targetDates$url[i])), 
        error = function(cond) {
          message(paste("URL does not seem to exist:", targetDates$url[i]))
        }
      )
      if (exists('tmp')) {
        terra::writeRaster(x = tmp, filename = targetDates$download_location[i], overwrite = TRUE)
        rm(tmp)
      }
    }
    message('Stage 3 data download complete!')
    # tst <- terra::rast(targetDates$download_location[i])  
  }
  if (any(grepl(x = stage, pattern = '4'))) {
    if (!dir.exists(file.path(data_folder, 'stage4'))) {
      dir.create(file.path(data_folder, 'stage4'), recursive = TRUE)
    }
    ### create list of dates, then urls. July 2016 is the first month that follows this pattern. Jan-Jun 2016 can be downloaded manually.
    years <- as.character(2016:as.numeric(format(Sys.Date(), format = '%Y')))
    months<- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
    days  <- as.character(day_range)
    days[nchar(days) < 2] <- paste0('0', days[nchar(days) < 2])
    targetDates <- expand.grid(years, months, days)
    targetDates$ymd  <- paste0(targetDates$Var1, targetDates$Var2, targetDates$Var3)
    ### remove first part of 2016
    # targetDates      <- targetDates[!(targetDates$ymd %in% paste0('20160',1:6, '01')), ]
    ### remove future months from current year
    current_mo       <- format(Sys.Date(), format = '%m')
    available_months <- 1:as.numeric(current_mo)
    months_to_remove <- as.character(c(1:12)[!(c(1:12) %in% available_months)])
    months_to_remove[nchar(months_to_remove) == 1] <- paste0('0', months_to_remove[nchar(months_to_remove) == 1])
    dates_to_remove  <- paste0(format(Sys.Date(), format = '%Y'), months_to_remove, '01')
    targetDates      <- targetDates[!(targetDates$ymd %in% dates_to_remove), ]
    
    targetDates$ymd2 <- paste(targetDates$Var1, targetDates$Var2, targetDates$Var3, sep = '/')
    targetDates$url  <- paste0('https://water.noaa.gov/resources/downloads/precip/stageIV/', targetDates$ymd2, "/", mapply(gsub, filename[2], pattern = 'YYYYMMDD', replacement = targetDates$ymd, USE.NAMES = FALSE))
    targetDates$basename <- basename(targetDates$url)
    targetDates$download_location <- file.path(data_folder, 'stage4', targetDates$basename)
    existing_files   <- list.files(file.path(data_folder, 'stage4'), full.names = FALSE)
    targetDates$exists_in_directory <- ifelse(targetDates$basename %in% existing_files, 1, 0)
    
    if (overwrite_data == FALSE) {
      targetDates <- targetDates[targetDates$exists_in_directory == 0, ] # remove existing data from list of impending downloads
    }
    
    ### now download and save
    for (i in 1:nrow(targetDates)) {
      message('Downloading Stage 4 data for ', targetDates$ymd2[i])
      tryCatch(
        tmp <- suppressWarnings(terra::rast(targetDates$url[i])), 
        error = function(cond) {
          message(paste("URL does not seem to exist:", targetDates$url[i]))
        }
      )
      if (exists('tmp')) {
        terra::writeRaster(x = tmp, filename = targetDates$download_location[i], overwrite = TRUE)
        rm(tmp)
      }
    }
    message('Stage 4 data download complete!')
  }
  invisible()
}