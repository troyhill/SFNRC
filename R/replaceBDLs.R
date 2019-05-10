#' @title Replace BDLs
#'
#' @description Replaces values below a detection limit with a method of the analyst's choosing
#' 
#' @usage replaceBDLs(data, valueCol = "value", mdlCol = "mdl", replacement = "mdl")
#' 
#' @param data dataframe
#' @param valueCol name of column with sample values
#' @param mdlCol name of column with detection limit associated with each value
#' @param replacement how should values belwo the detection limit be treated? "mdl" (default)
#' replaces values with the detection limit. Numeric multipliers are also allowed; "0.5" would 
#' set values at half of the detection limit. 
#' 
#' @return list \code{replaceBDLs} returns a list the same length as list1 and list2
#' 
#' 
#'  @importFrom utils setTxtProgressBar
#'  @importFrom utils txtProgressBar
#' @export



replaceBDLs <- function(data, valueCol = "value", mdlCol = "mdl", 
                        replacement = "mdl" # can be "mdl" or a numeric multiplier of the mdl (e.g., 0.5 for replacing values with half of the mdl)
) {
  # replaces values below MDLs
  data$bdl <- 0 
  pb <- txtProgressBar(style = 3, min = 0, max = nrow(data))
  for (i in 1:nrow(data)) {
    if (is.na(data[, valueCol][i]) && !is.na(data[, mdlCol][i])) {
      if ((data[, mdlCol][i] > 0) & (data[, mdlCol][i] < 50)) { # idk why, but there's a -5 and a 100 in the mdl data
        data$bdl[i]   <- 1
        if (replacement == "mdl") {
          data[, valueCol][i] <- data[, mdlCol][i]
        } else if (is.numeric(replacement)) {
          data[, valueCol][i] <- data[, mdlCol][i] * replacement
        }
        
      }
    }
    utils::setTxtProgressBar(pb, i)
  }
  # cat(summary(data[data$bdl == 1, ]))
  # cat(summary(data[data$bdl == 0, ]))
  
  invisible(data)
}
