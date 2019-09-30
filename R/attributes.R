#' Returns the name of the population
#'
#' @param pop is an object of the class 'population'
#'
#' @return returns the name of the population.
#'
#' @examples
#'     pop=population("test",42,2,200)
#'     names_population(pop)
#'
#' @author Nicolas BARTALUCCI
#'
#' @export
names_population = function(pop) {
  if (is.population(pop)==T) {pop$label}
  else {stop("The object must be a population.")}
}