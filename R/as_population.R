#' as_population
#'
#' The fonction \code{'as_population'} takes as argument a list
#' containing the population caracteristics (population,
#' number at initial time, growth rates.
#' And check if is a class population and  return a list duable by \code{BeePODYNA} package
#'
#'
#' @param object a list of populations objects (information on the population)
#'
#'
#' @return If \code{'object'} is a list, the function will test if it match with the
#'  the requirements of a class \code{'population'} object.
#'  If \code{'object'} is not a list an error message will be give.
#'
#' @examples
#'  obj = list(label ="pop.test",size= 100,growth_rate = 0.1,capacity = 500)
#'  as_population(obj)
#'
#' @author Martial <leroy.martial@etu.univ-grenoble-alpes.fr>
#' @export

as_population = function(object){
  UseMethod("as_population")
}

#' @rdname as_population
#' @export
as_population.default <- function(object) {
  stop(sprintf("I cannot cast an object of type %s to population object",
               class(object)
              )
      )
}


#' @rdname as_population
#' @export

as_population.population <- function(object) {
  object
}


#' @rdname as_population
#'
#' @export

as_population.list <- function(object) {
   n = names(object)

   if ("label" %in% n) {
     label <- object$label
   }
   else {
     stop("no label defined in list")
   }

   if ("size" %in% n) {
     size <- object$size[1]
   }
   else {
     stop("no size defined in list")
   }

   if ("growth_rate" %in% n) {
     growth_rate <- object$growth_rate
   }
   else {
     stop("no growth_rate defined in list")
   }

   if ("capacity" %in% n) {
     capacity <- object$capacity
   }
   else {
     stop("no capacity defined in list")
   }

   newpop <- population(label,size,growth_rate,capacity)

   if ("time" %in% n &&
       (length(object$size) == length(object$time))
      ) {
     newpop$size = object$size
     newpop$time = object$time
   }
   else {
     if (length(object$size) > 1)
       warning("No time information available, only intial size has been conservec")
   }

   newpop
}

