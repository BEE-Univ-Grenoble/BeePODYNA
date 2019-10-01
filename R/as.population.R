#' The fonction *as.population* takes as argument a list
#' containing the population caracteristics (population,
#' number at initial time, growth rates or birth rate and
#' death rate) or a poputaion monitored throught time.
#' And return the inoramtion as a list duable by BeePODyna package

#' @param object if it is list of populations objects (information on the population)
#' must be in the same semantic as population. It could be
#' a dataframe containing the evolution of the population throught time.

#' @return If *object* is a list, the function will test if it match with the
#'  the requirements of a class *population* object.
#'  If *object* is a data frame the function will determine the growth rate,
#'  and form a list of class *population* containing the parameter of the population*.

#' @export

as.population = function(object){

  object = object

# creation of a new class, type : List, containing characters and numeric.

  setClass("population",representation("list"),contains = c("character","numeric"))

# Check list element
  if(class(object) == "list"){
    check_pop = is.population(object)

    if(check_pop == TRUE){

      return(new("population",object))

    print("The object ",object," is already a population,
          now class has been converted to 'population'")

    }
    if(check_pop == FALSE){

      population(object)

    }

  }

# If object is a data frame

  df_object = as.data.frame(object)
  colnames(df_object) = c("time","pop") # /!\ DF must be in the right order

## Find the intial population :

  init = df_object$pop[which(df.object$time == 0)]

## Find the growth rate

  ### (too?) simple way :
  gr.tempo = numeric(length(df_object$time))

  for (i in c(1:length(df_object$time)-1)) {

    gr_tempo[i] = (df_object$pop[i+1]-df_object$pop[i])/(df_object$time[i+1]-df_object$time[i])

  }
# GR : Growth Rate
  approx_GR = mean(gr_tempo)

  df_GR = as.data.frame(cbind(df_object$time,df_object$pop,gr_tempo)) #make a DF with the time and the growth rate
  colnames(df_GR) = c("time","pop","growth_rate")

  ### maybe try to fit a model on the data and extract the GR,

## Find the limit capacity : (where pop is stable ie. GR = 0)

  for (i in length(df_GR$time)) {

    if(sum(df_GR$growth_rates[i],df_GR$growth_rates[i+1]) == 0 ){

      K = df_GR$pop[i]
    }

  }

#' @example

  }




