#' @title Apply a function with lists as multiple arguments 
#'
#' @description Function "fns" is applied to each of the elements in list1. The elements in 
#' list2 are supplied sequentially to the function call, 
#' e.g., output[[1]] <- fn(list1[[1]], list2[[1]], ...)
#' 
#' @usage mapLists(fns, list1, list2 = NULL, ...)
#' 
#' @param fns function
#' @param list1 a list (of data frames, models, etc.)
#' @param list2 a list of the same size as list1, specifying a second argument in the fn call associated with sequential elements of list1
#' @param ... other arguments passed on to [fns()]. 
#' 
#' @return list \code{mapLists} returns a list the same length as list1 and list2
#' 
#' 
#' @examples
#' \dontrun{
#' 
#' a_list       <- list(1:10, 80:83, 300:305)
#' another_list <- list(3, 0:3, 50)
#' mapLists(fns = function(x, y) x*y, list1 = a_list, list2 = another_list)
#' 
#' ### applications to WRTDS
#' # eBoot.tkn <- mapLists(wBT, nitro, caseSetUp.tkn)
#' # mapLists(fns = plotConcHistBoot, list1 = tp, list2 = CIAnnualResults)
#' }
#'  
#' @export



mapLists <- function(fns, list1, list2 = NULL, ...) {
  if (!is.null(list2)) {
    Map(
      function(fn, value1, value2, ...)
      {
        fn(value1, value2, ...)
      },
      list(fns),
      list1, 
      list2, ...
    )
  } else if (is.null(list2)) {
    Map(
      function(fn, value1, ...)
      {
        fn(value1, ...)
      },
      list(fns),
      list1, 
      ...
    )   
  }
}
