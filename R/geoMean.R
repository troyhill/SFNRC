#' @title Calculate geometric mean
#'
#' @description Calculates the geometric mean of a vector of values.
#'
#' @param x vector of values from which geometric mean will be calculated
#' @param nas TRUE/FALSE value indicating whether NAs should be removed
#' @param zero.propagate TRUE/FALSE value indicating whether zeroes should be propagated
#'
#' @return vector \code{geoMean} returns the geometric mean of the input values 
#' @export
#'
#' @examples
#' geoMean(rnorm(100, 5, 5))
geoMean <- function(x, nas = TRUE, zero.propagate = FALSE){
  # see: https://stackoverflow.com/a/25555105
  
  if(any(x < 0, na.rm = nas)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = nas)){
      return(0)
    }
    exp(mean(log(x), na.rm = nas))
  } else {
    exp(sum(log(x[x > 0]), na.rm = nas) / sum(x, na.rm = nas))
  }
}