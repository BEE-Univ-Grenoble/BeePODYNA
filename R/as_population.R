#' as_population
#'
#' The fonction \code{'as_population'} takes as argument a list
#' containing the population caracteristics (population,
#' number at initial time, growth rates or birth rate and
#' death rate).
#' And return a list duable by \code{BeePODYNA} package
#'
#'
#' @param object if it is list of populations objects (information on the population)
#'
#'
#' @return If \code{'object'} is a list, the function will test if it match with the
#'  the requirements of a class \code{'population'} object.
#'  If \code{'object'} is a data frame the function will determine the growth rate,
#'  and form a list of class \code{'population'} containing the parameter of the population.
#'
#' @examples
#' obj = list(label ="pop.test",size= 100,growth_rate = 0.1,capacity = 500)
#' as_population(obj)
#'
#'
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

# # creation of a new class, type : List, containing characters and numeric.
#
#   setClass("population",representation("list"),contains = c("character","numeric"))
#
# # Check list element
#   if(class(object) == "list"){
#       population(object)
#
#     }
#
#   }
#
# # If object is a data frame
#
#   df_object = as.data.frame(object)
#   colnames(df_object) = c("time","pop") # /!\ DF must be in the right order
#
# ## Find the intial population :
#
#   init = df_object$pop[which(df.object$time == 0)]
#
# ## Find the growth rate
#
#   ### (too?) simple way :
#   gr.tempo = numeric(length(df_object$time))
#
#   for (i in c(1:length(df_object$time)-1)) {
#
#     gr_tempo[i] = (df_object$pop[i+1]-df_object$pop[i])/(df_object$time[i+1]-df_object$time[i])
#
#   }
# # GR : Growth Rate
#   approx_GR = mean(gr_tempo)
#
#   df_GR = as.data.frame(cbind(df_object$time,df_object$pop,gr_tempo)) #make a DF with the time and the growth rate
#   colnames(df_GR) = c("time","pop","growth_rate")
#
#   ### maybe try to fit a model on the data and extract the GR,
#
# ## Find the limit capacity : (where pop is stable ie. GR = 0)
#
#   for (i in length(df_GR$time)) {
#
#     if(sum(df_GR$growth_rates[i],df_GR$growth_rates[i+1]) == 0 ){
#
#       K = df_GR$pop[i]
#     }
#
#   }
#
# # @example
#
#   }




