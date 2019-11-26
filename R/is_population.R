#' Tests if an object is belonging the class \code{population}.
#'
#' The function determinines if the object is a population or not
#'
#' @param x the object which must be a population to validate the condition
#'
#' @return a logical "TRUE" or "FALSE"
#'
#' @examples
#'
#'   p <- population('example', 2, 0.5, 10)
#'   is_population(p) # Returns TRUE
#'   is_population(4) # Returns FALSE
#'
#'
#' @author Cresciense Lecaude
#'
#' @export
is_population <- function(x){
  class(x) == "population"
}
