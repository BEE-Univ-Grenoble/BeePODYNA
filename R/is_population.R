#' Is.population
#' The function determines if the object is a population or not
#' @param x the object which must be a population to validate the condition
#' @return a logical "TRUE" or "FALSE"
#' @example
#' @author Cresciense Lecaude
#' @export

is.population <- function(x){
  if (class(x) == "population"){
    return(TRUE)
  }
 else {
  return(FALSE)
  }
}