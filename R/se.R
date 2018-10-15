#' Calculates standard error
#'
#' @param x numeric or integer
#'
#' @return value
#'
#' @examples
#' se(rnorm(10, 5, 5))
#' se(rnorm(100, 5, 5))
#' 
#' @importFrom stats sd
#' @export
se <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(sum(x, na.rm = TRUE))
}
