#' Returns the name of the population
#'
#' @param pop is an object of the class 'population'
#'
#' @return returns the name of the population.
#'
#' @examples
#'     pop=population("Pop_1",42,2,200)
#'     names_population(pop)
#'
#' @author Nicolas BARTALUCCI <bartalucci.nico@gmail.com>
  names_population = function(pop) {
    if (is.population(pop)==T) {pop$label}
    else {stop("The object must be a population.")}
  }

  #' Returns the size of the population
  #'
  #' @param pop is an object of the class 'population'
  #'
  #' @return returns the size of the population.
  #'
  #' @examples
  #'     pop=population("Pop_1",42,2,200)
  #'     size_population(pop)
  #'
  #' @author Nicolas BARTALUCCI <bartalucci.nico@gmail.com>
  #'
  #' @export
  size_population = function(pop) {
    if (is.population(pop)==T) {pop$size}
    else {stop("The object must be a population.")}
  }

  #' Returns the capacity of the population
  #'
  #' @param pop is an object of the class 'population'
  #'
  #' @return returns the capacity of the population.
  #'
  #' @examples
  #'     pop=population("Pop_1",42,2,200)
  #'     capacity_population(pop)
  #'
  #' @author Nicolas BARTALUCCI <bartalucci.nico@gmail.com>
  #'
  #' @export
  capacity_population = function(pop) {
    if (is.population(pop)==T) {pop$capacity}
    else {stop("The object must be a population.")}
  }

  #' Returns the growth rate of the population
  #'
  #' @param pop is an object of the class 'population'
  #'
  #' @return returns the growth rate of the population.
  #'
  #' @examples
  #'     pop=population("Pop_1",42,2,200)
  #'     grate_population(pop)
  #'
  #' @author Nicolas BARTALUCCI <bartalucci.nico@gmail.com>
  #'
  #' @export
  grate_population = function(pop) {
    if (is.population(pop)==T) {pop$growth_rate}
    else {stop("The object must be a population.")}
  }