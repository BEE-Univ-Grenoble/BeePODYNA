#' names_population
#'
#' \code{names_population} returns the name of the population given in parameter.
#'
#' @param pop is an object of the class 'population'.
#'
#' @return It returns a character with the name of the population.
#'
#' @examples
#'     pop<-population("Pop_1",42,2,200)
#'     names_population(pop)
#'
#' @author Nicolas BARTALUCCI <bartalucci.nico@gmail.com>
#'
#' @export
names_population <- function(pop) {
  if (is.population(pop)) {pop$label}
  else {stop("The object must be a population.")}
}

#' size_population
#'
#' \code{size_population} returns the size of the population given in parameter.
#'
#' @param pop is an object of the class 'population'.
#'
#' @return It returns the size of the population.
#'
#' @examples
#'     pop<-population("Pop_1",42,2,200)
#'     size_population(pop)
#'
#' @author Nicolas BARTALUCCI <bartalucci.nico@gmail.com>
#'
#' @export
size_population <- function(pop) {
  if (is.population(pop)) {pop$size}
  else {stop("The object must be a population.")}
}

#' capacity_population
#'
#' \code{capacity_population} returns the size of the population given in parameter.
#'
#' @param pop is an object of the class 'population'.
#'
#' @return It returns the capacity of the population.
#'
#' @examples
#'     pop<-population("Pop_1",42,2,200)
#'     capacity_population(pop)
#'
#' @author Nicolas BARTALUCCI <bartalucci.nico@gmail.com>
#'
#' @export
capacity_population <- function(pop) {
  if (is.population(pop)) {pop$capacity}
  else {stop("The object must be a population.")}
}

#' grate_population
#'
#' \code{grate_population} returns the size of the population given in parameter.
#'
#' @param pop is an object of the class 'population'.
#'
#' @return It returns the growth rate of the population.
#'
#' @examples
#'     pop<-population("Pop_1",42,2,200)
#'     grate_population(pop)
#'
#' @author Nicolas BARTALUCCI <bartalucci.nico@gmail.com>
#'
#' @export
grate_population <- function(pop) {
  if (is.population(pop)) {pop$growth_rate}
  else {stop("The object must be a population.")}
}